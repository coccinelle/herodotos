(* Yoann Padioleau
 *
 * Copyright (C) 2010 INRIA, University of Copenhagen DIKU
 * Copyright (C) 1998-2009 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

exception Timeout
exception Failure of string

let (+>) o f = f o
let pr2 = prerr_endline

let trace msg =
        List.iter (fun m -> [%trace_log m]) (Str.split (Str.regexp_string "\n") msg)

let (with_open_stringbuf: (((string -> unit) * Buffer.t) -> unit) -> string) =
 fun f ->
  let buf = Buffer.create 1000 in
  let pr s = Buffer.add_string buf (s ^ "\n") in
  f (pr, buf);
  Buffer.contents buf

let timenow () =
  "sys:" ^ (string_of_float (Sys.time ())) ^ " seconds" ^
  ":real:" ^
    (let tm = Unix.time () +> Unix.gmtime in
     tm.Unix.tm_min +> string_of_int ^ " min:" ^
     tm.Unix.tm_sec +> string_of_int ^ ".00 seconds")
(* ---------------------------------------------------------------------- *)

type prof = PALL | PNONE | PSOME of string list
let profile = ref PNONE
let show_trace_profile = ref true

let check_profile category =
  match !profile with
    PALL -> true
  | PNONE -> false
  | PSOME l -> List.mem category l

let _profile_table = Hashtbl.create 101

let adjust_profile_entry category difftime =
  let (xtime, xcount) =
    (try Hashtbl.find _profile_table category
    with Not_found ->
      let xtime = ref 0.0 in
      let xcount = ref 0 in
	Hashtbl.add _profile_table category (xtime, xcount);
      (xtime, xcount)
    ) in
  xtime := !xtime +. difftime;
  xcount := !xcount + 1;
  ()

(* subtil: don't forget to give all argumens to f, otherwise partial app
 * and will profile nothing.
 *
 * todo: try also detect when complexity augment each time, so can
 * detect the situation for a function gets worse and worse ?
 *)
let profile_code_silent category f =
  if not (check_profile category)
  then f()
  else begin
    let t = Unix.gettimeofday () in
    let res, prefix =
      try Some (f ()), ""
      with Timeout -> None, "*"
    in
    let category = prefix ^ category in (* add a '*' to indicate timeout func *)
    let t' = Unix.gettimeofday () in

      adjust_profile_entry category (t' -. t);
      (match res with
	 | Some res -> res
	 | None -> raise Timeout
      );
  end

let profile_diagnostic () =
  if !profile = PNONE then "" else
    begin
      let xs =
	Hashtbl.fold (fun k v acc -> (k,v)::acc) _profile_table []
	+> List.sort (fun (_, (t1,_)) (_, (t2,_)) -> compare t2 t1)
      in
	with_open_stringbuf
	  (fun (pr,_) ->
	     pr "---------------------";
	     pr "profiling result";
	     pr "---------------------";
	     xs +> List.iter (fun (k, (t,n)) ->
				pr (Printf.sprintf "%-40s : %10.3f sec %10d count" k !t !n)
			     )
	  )
    end

let profile_code category f =
  profile_code_silent category
    (fun () ->
       if not (check_profile category)
       then f()
       else begin
	 if !Misc.debug then pr2 ("starting: " ^ category);
	 let t = Unix.gettimeofday () in
	   try
	     let res = f () in
	     let t' = Unix.gettimeofday () in
	       if !Misc.debug then pr2 (Printf.sprintf "ending: %s, %fs" category (t' -. t));
	       res
	   with e ->
	     let t' = Unix.gettimeofday () in
	       if !Misc.debug then pr2 (Printf.sprintf "ending with failure: %s, %fs" category (t' -. t));
	       match e with
		   Failure _ -> raise e
		 | _ ->
		     let msg = Printexc.get_backtrace () in
		       prerr_endline msg;
		       raise (Failure msg)
       end
  )
