open Helper

exception Unsupported of string

let build_message verbose fn info an =
  (* (filename, info, author) = verbose *)
  let msg = match verbose with
      ( true,  true,  true) -> [fn;info;an]
    | ( true,  true, false) -> [fn;info   ]
    | ( true, false,  true) -> [fn     ;an]
    | ( true, false, false) -> [fn        ]
    | (false,  true,  true) -> [   info;an]
    | (false,  true, false) -> [   info   ]
    | (false, false,  true) -> [        an]
    | (false, false, false) -> [          ] (* This should not happen! *)
  in
    String.concat " " msg

let block = 5

let rec string_of_legendandmark step count i =
  if i < count then
    let y = float_of_int i *. step in
    let hd = Printf.sprintf "hash_label at %02f : %d\nhash_at %02f\n" y i y in
      hd^(string_of_legendandmark step count (i+count/block))
  else ""

let get_ylegendandmark size count =
  if count > 100 then
    let fcount = float_of_int count in
    let order = 10.0 ** (floor (log10 fcount)) in
    let max = (ceil (fcount/.order)) *. order in
    let step = size /. fcount in
    let str = string_of_legendandmark step (int_of_float max) 0 in

    let gorder = 10.0 ** (floor (log10 fcount)-. 1.0) in
    let gmax = ((ceil (fcount/.gorder)) *. gorder) in
    let maxp = gmax *. step in
    let hd = Printf.sprintf "hash_label at %02f : %d\nhash_at %02f\n" maxp (int_of_float gmax) maxp in
      (maxp, str^hd^"draw_at -10\n")
  else
    (size, "no_draw_axis")

let draw_graph verbose outch vlist fbl xaxis xlabel ylabel notexistcolor cleancolor defectcolor gsize =
  let (filename, info, author) = verbose in
  let is_compact = not filename && not info && not author in
  let draw_at = if is_compact then "-0.05" else "-1" in
  let count = List.length fbl in
  let fsize = float_of_int count in
  let size = if is_compact then fsize /. 20.0 else fsize -. 1.0 in
  let max = Array.length vlist - 1 in
  let (xlegend, xmark, xmax, xstep, xpos) =
    match xaxis with
	"version" ->
	  let xstep = 1 in
	    (get_names_by_idx vlist, get_mark_idx vlist, max+xstep, xstep, fun x -> x)
      | "date" ->
	  let (_, lastestday,_,_) = Array.get vlist max in
	  let extradays = 100 in
	  let next = lastestday + extradays in
	  let get_xpos = (fun x ->
			    try
			      let (_, xpos, _,_) = Array.get vlist x in xpos
			    with _ -> next
			 ) in
	    (get_years_with_mark extradays vlist, "", next, extradays,  get_xpos)
      | _ -> raise (Unknown "xaxis type")
  in
  let (gxsize, gysize) =
    match gsize with
	None -> (5.0, if is_compact then size *. 0.40 else size /. 10.0 +. 1.0)
      | Some s -> s
  in
  let marksize = if is_compact then "marksize 0.01 0.01" else "(* no marksize *)" in
  let (size, ylegendandmark) = get_ylegendandmark size count in
  let step = if is_compact then 0.05 else 1.0 in
  Printf.fprintf outch "
newgraph
xaxis min 0 max %02d size %02.2f label : %s
draw_at %s grid_lines no_auto_hash_labels no_auto_hash_marks
(* hash_labels rotate 45 vjc hjr *)
hash_labels rotate -45 vjc hjl
draw_hash_labels
%s
hash_label at %02d : next
%s
hash_at %02d

yaxis min 0 max %02.2f size %02.2f label : %s
no_auto_hash_marks no_auto_hash_labels
%s

"
    xmax gxsize xlabel
    draw_at
(*    (get_names_by_idx vlist)*)
    xlegend xmax
    xmark   xmax
    size gysize ylabel
    ylegendandmark;
    Printf.fprintf outch "newcurve %s marktype ybar color %s\npts\n"
      marksize notexistcolor;
    ignore(List.fold_left (fun i bug ->
			   let (_, _, _, femin, bmin, bmax, femax) = bug in
			     (if (femax < max) then
			       Printf.fprintf outch " %02d %02.2f\n" (xpos xmax) i;
			     );
			     i+.step) 0.0 fbl);
  Printf.fprintf outch "
newcurve %s marktype ybar color %s
pts\n"  marksize cleancolor;
  ignore(List.fold_left (fun i bug ->
			   let (_, _, _, femin, bmin, bmax, femax) = bug in
			     (if (femax > bmax) then
			       Printf.fprintf outch " %02d %02.2f\n" (xpos (femax+1)) i
			     );
			     i+.step) 0.0 fbl);
  Printf.fprintf outch "
newcurve %s marktype ybar color %s
pts" marksize defectcolor;
  ignore(List.fold_left (fun i bug ->
			   let (_, _, _, femin, bmin, bmax, femax) = bug in
		    Printf.fprintf outch " %02d %02.2f\n" (xpos (bmax+1)) i;
		    i+.step) 0.0 fbl);
  Printf.fprintf outch "
newcurve %s marktype ybar color %s
pts" marksize cleancolor;
  ignore(List.fold_left (fun i bug ->
			   let (_, _, _, femin, bmin, bmax, femax) = bug in
			     (if (femin < bmin) then
				Printf.fprintf outch " %02d %02.2f\n" (xpos bmin) i
			     );
			      i+.step) 0.0 fbl);
  Printf.fprintf outch "
newcurve %s marktype ybar color %s
pts\n" marksize notexistcolor;
  ignore(List.fold_left (fun i bug ->
			   let (_, _, _, femin, bmin, bmax, femax) = bug in
			     (if (femin > 0) then
				Printf.fprintf outch " %02d %02.2f\n" (xpos femin) i
			     );
			     i+.step) 0.0 fbl);
  if filename || info || author then
    begin
      Printf.fprintf outch "
newstring
hjl vjc
(* fonsize 9 *)
font Helvetica-Narrow\n";
      match fbl with
	  [] -> raise (Unsupported "No bug to draw!")
	| (aopt, f0, txtinfo, _,_,_,_)::tail ->
	    let msg = build_message verbose f0 txtinfo "" in
	      Printf.fprintf outch " x %02d y 00 : %s\n" (xmax+xstep) msg;
	      ignore(List.fold_left
		       (fun i (aopt, f, txtinfo,_, _,_,_) ->
			 let msg =
			   build_message verbose f txtinfo ""
			 in
			   Printf.fprintf outch " copystring y %02d : %s\n"  i msg;
			   i+1) 1 (tail))
    end

let draw v1 v2 name (atts, curves) allbugs =
  let gname = !Setup.prefix ^"/"^ name in
  let outch = Misc.create_dir_and_open v2 gname in
    match curves with
	[curve] ->
	    let (project, pattopt, catts, _) = curve in
	    let patt = match pattopt with None -> "" | Some v -> v in
	    let vminopt = Config.get_vmin_of patt in
	    let prjname = Config.get_project atts project in
	      (try
		let author = Graph.is_with_author v2 name atts catts in
		let scmpath = Helper.get_scmpath author prjname in
		let size = Config.get_size atts in
		let bugset = "" in
		let tmp = snd (List.find (Helper.by_bugset bugset) allbugs) in
		let (varray, fel, fbl, _) = tmp in
		let filename = Graph.is_with_fn v2 name atts catts in
		let info = Graph.is_with_info v2 name atts catts in
		let xaxis  = Graph.get_xtype name atts in
		let xlegend = Graph.get_xlegend v2 name atts "Versions/Date" in
		let ylegend = Graph.get_ylegend v2 name atts "Defects" in
		  (* FIXME Give priority of catts over atts *)
		let notexistcolor = Graph.get_notexistcolor v2 name (catts@atts) "0 0 0" in
		let cleancolor    = Graph.get_cleancolor    v2 name (catts@atts) "1 1 1" in
		let defectcolor   = Graph.get_patterncolor  v2 name (catts@atts) "0 1 1" in

		let verbose = (filename,info, author) in
		let bugs = compute_graph v1 v2 "" fel fbl varray scmpath vminopt in
		  if v1 then Stats.show_stat v2 varray bugs prjname;

		  prerr_endline ("Drawing "^gname);
		  draw_graph verbose outch varray bugs xaxis xlegend ylegend notexistcolor cleancolor defectcolor size;
		  close_out outch;
		  gname
	      with Not_found ->
		prerr_endline ("*** ERROR *** Pb in " ^ name ^" curve " ^ prjname ^ " and "^patt);
		raise (Unsupported "Maybe you have not checked the false positives yet!")
	      )
      | _ -> raise (Unsupported ("The type 'defect' used by "^name^" does not support multicurve setting."))
