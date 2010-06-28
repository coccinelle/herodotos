exception FileNotFound of string
exception Unknown of string
exception Misconfigured of string
exception Unimplemented

type datatype =
    Single of float option
  | Duple of float option * float option

type version = (string * int * Unix.tm * int) array
type bugset = string
type value = version * string * string * string * string * Setup.PrjTbl.key * bugset * datatype array
type eval_type =
    Empty
  | Value of value
  | Cst   of float
type op = Add | Sub | Mul | Div

let wrap_single_some data =
    Array.map (fun x -> Single (Some x)) data

let wrap_single data =
    Array.map (fun x -> Single x) data

let wrap_duple data =
    Array.map (fun (x, y) -> Duple (x,y)) data

(*
let get_value_of_dt data =
  match data with
    | Single v -> v
    | Duple _ -> failwith "What are you trying to do !?"

let combine data1 data2 =
  Array.mapi (fun i d1 ->
		Duple (get_value_of_dt d1, get_value_of_dt (Array.get data2 i))
	     ) data1
*)

let combine data1 data2 =
  Array.mapi (fun i d1 -> Duple (d1, Array.get data2 i)) data1

let match_f bug fe =
  let (fb, _, _, _,_) = bug in
  let (fe, _, _) = fe in
    fb = fe

let get_femin fel bug =
  let (fb, _, _, _,_) = bug in
  try
    let (_, fmin, _) = List.find (fun fe -> match_f bug fe) fel in
      fmin
  with _ -> raise (FileNotFound fb)

let get_femax fel bug =
  let (fb, _, _, _,_) = bug in
  try
    let (_, _, fmax) = List.find (fun fe -> match_f bug fe) fel in
      fmax
  with _ -> raise (FileNotFound fb)

let get_data_of vb conn curve =
  let (_, _, catts, pos) = curve in
    try
      let query = Config.get_query catts in
	if query <> "" then
	  begin
	    if vb then prerr_endline ("Query: "^query);
	    (*
	      Query the database using the connection conn
	      and the query 'query'
	    *)
	    try
	      (query, Database.get_tuples vb conn query)
	    with _ ->
(* 	      Printexc.print_backtrace stderr; *)
	      raise (Config.Misconfigurationat ("Check query \""^query^"\"", pos))
	  end
	else
	  raise (Config.Misconfigurationat ("Empty query!", pos))
    with Not_found ->
      raise (Config.Misconfigurationat ("No query found!", pos))

let compute_bug_info vb1 vb2 prefix fel vlist scmpath vminopt bug =
  let fmin = get_femin fel bug in
  let fmax = get_femax fel bug in
  let (fb,c,bmin,bmax,pos) = bug in
  let noneed_author =
    if scmpath = "" then true else
      match vminopt with
	  None -> false
	| Some vmin ->
	    let vminidx = Misc.get_idx_of_version vlist vmin in
	      if bmin < vminidx then
		true
	      else
		false
  in
  let (v,_,_,_) = Array.get vlist bmin in
  let author =
    if noneed_author then None
    else
      (
	let (line, _, _) = pos in
	let a = Git.blame vb2 vlist scmpath bmin line fb in
	  if vb2 then prerr_endline (a^";"^fb^";"^v);
	  Some a
      )
  in
    (author, fb, c, fmin, bmin, bmax, fmax)

(**
   Return: (authoropt, fb, c, fmin, bmin, bmax, fmax)
*)
let compute_graph vb1 vb2 prefix fel bl vlist scmpath vminopt =
  List.map (compute_bug_info vb1 vb2 prefix fel vlist scmpath vminopt) bl

(**

*)
let transpose varray fel =
  let find_count i =
    let (version, _,_,_) = Array.get varray i in
    let (_, v, _) = List.find (fun (vers, _,_) -> vers= version) fel in
      float_of_int v
  in
    wrap_single_some (Array.init (Array.length varray) find_count)

let update_dir dir assoc =
  try
    let v = List.assoc dir assoc in
    let res = List.remove_assoc dir assoc in
      (dir, v+1)::res
  with Not_found ->
    (dir, 1)::assoc

let compute_bydir bugs =
  let p1_re = Str.regexp "^\\([^/]+\\)/.*$" in
    List.fold_left (fun res (_, fb, _, _, _, _,_) ->
		      if(Str.string_match p1_re fb 0) then
			let dir = Str.matched_group 1 fb in
			  update_dir dir res
		      else
			update_dir "" res
		   ) [] bugs

let cd_ratio vlist bugs =
  let max_ver = Array.length vlist - 1 in
  List.fold_left
    (fun (i,j) (author, fc, c, fmin, bmin, bmax, fmax) ->
       ((if (fmin = bmin && fmin != 0) then i+1 else i),
	(if (fmax = bmax && fmax != max_ver) then j+1 else j))
    ) (0,0) bugs

let sl_ratio_ver bugs =
  let (shortest, sum, longest) =  List.fold_left
    (fun (i,j,k) (author, fc, c, fmin, bmin, bmax, fmax) ->
       let duration = bmax - bmin + 1 in
	 ((if (duration < i) then duration else i),
	  j + duration,
	  (if (duration > k) then duration else k))
    ) (max_int, 0, 0) bugs
  in
  let size = List.length bugs in
  let avg = float_of_int sum /. float_of_int size in
    (shortest, avg, longest)

let sl_ratio_day vlist bugs =
  let (shortest, sum, num, longest) =  List.fold_left
    (fun (i,j,k,l) (author, fc, c, fmin, bmin, bmax, fmax) ->
       let dbmax =
	 try
	   let (_,dbmax,_,_) = Array.get vlist (bmax +1) in
	     dbmax
	 with Invalid_argument _ ->
	   let (_,dbmax,_,_) = Array.get vlist bmax in
	     dbmax
       in
       let (_,dbmin,_,_) = Array.get vlist bmin in
       let duration = dbmax - dbmin in
	 ((if (duration < i && duration > 0) then duration else i),
	  j + duration,
	  (if (duration > 0) then k+1 else k),
	  (if (duration > l) then duration else l))
    ) (max_int, 0, 0, 0) bugs
  in
  let avg = if num = 0 then 0 else sum / num in
    (shortest, avg, longest)

let verbose_stat vlist bugs =
  List.iter (fun (author, fc, c, fmin, bmin, bmax, fmax) ->
	       print_endline (fc ^ " - " ^ c ^
				" fm: " ^ (Misc.get_version_name vlist fmin) ^
				" bm: " ^ (Misc.get_version_name vlist bmin) ^
				" bM: " ^ (Misc.get_version_name vlist bmax) ^
				" fM: " ^ (Misc.get_version_name vlist fmax)))
    bugs

let affected_files fbl=
  List.length (List.fold_left
		 (fun pre bug ->
		    let (_,fb, _, _, _, _,_) = bug in
		      if List.mem fb pre then
			pre
		      else
			fb::pre
		 ) [] fbl)

(* Function for marks using version name / date of releases *)
let get_names_by_idx varray =
  let va = Array.mapi
    (fun i (name,_,_,_) ->
       "hash_label at "^ string_of_int i ^ " : " ^ name) varray
  in
  String.concat "\n" (Array.to_list va)

let get_mark_idx varray =
  let va = Array.mapi
    (fun i _ ->
       Printf.sprintf "hash_at % 4d" i
    ) varray
  in
  String.concat "\n" (Array.to_list va)

let get_names_by_date varray =
  let va = Array.mapi
    (fun i (name,dn,_,_) ->
       Printf.sprintf "hash_label at % 4d : %s" dn name
    ) varray
  in
  String.concat "\n" (Array.to_list va)

let get_mark_date varray =
  let va = Array.mapi
    (fun i (name,dn,_,_) ->
       Printf.sprintf "hash_at % 4d" dn
    ) varray
  in
  String.concat "\n" (Array.to_list va)


(* Function for marks using date during the entire periode *)
let rec gen_tm_year_serie minyear maxyear =
  if minyear <= maxyear then
    let mintm = snd (Unix.mktime
		       {Unix.tm_mon=0; Unix.tm_mday=1; Unix.tm_year=minyear-1900;
			(* Don't care about the time *)
			Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
			(* Will be normalized by mktime *)
			Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
		       }) in
      mintm :: gen_tm_year_serie (minyear +1) maxyear
  else
    []

let get_years_with_mark extradays varray =
  let (_, _, mintm, _) = Array.get varray 0 in
  let (_, _, relmaxtm, _) = Array.get varray (Array.length varray - 1) in
  let maxtm = snd  (Unix.mktime
		       {Unix.tm_mon=relmaxtm.Unix.tm_mon;
			Unix.tm_mday=relmaxtm.Unix.tm_mday+extradays;
			Unix.tm_year=relmaxtm.Unix.tm_year;
			(* Don't care about the time *)
			Unix.tm_sec=0; Unix.tm_min=0; Unix.tm_hour=0;
			(* Will be normalized by mktime *)
			Unix.tm_wday=0; Unix. tm_yday=0; Unix.tm_isdst=false
		       }) in
  let minyear =
    if mintm.Unix.tm_mon < 6
    then
      Misc.get_year_of mintm
    else
      Misc.get_year_of mintm +1
  in
  let maxyear = Misc.get_year_of maxtm in
  let years = gen_tm_year_serie minyear maxyear in
  let list = List.map
    (fun tmyear ->
       let name = string_of_int (Misc.get_year_of tmyear) in
       let dn = Config.get_rel_days_of (Config.get_abs_days_of mintm) tmyear in
	 Printf.sprintf "hash_at % 4d\nhash_label at % 4d : %s" dn dn name
    ) years
  in
    String.concat "\n" list

let by_bugset bugset bug =
  let (bs, _) = bug in
    bugset = bs

let max_idx_w_vminopt varray vminopt bmin_init =
    match vminopt with
	None -> bmin_init
      | Some vmin ->
	  let vmin_idx = Misc.get_idx_of_version varray vmin in
	    max vmin_idx bmin_init

let crop_array prune vmin_idx idx valuedt =
  if idx < vmin_idx then
    match valuedt with
	Single _ -> if prune then Single None else valuedt
      | Duple  _ ->
	  if prune then
	    Duple (None, None)
	  else
	    valuedt
  else
    valuedt

let crop varray atts curve values =
  let (_, pattern,catts,_) = curve in
  let d_list = Config.get_pattern atts pattern catts in
    match d_list with
	[d] ->
	  begin
	    let vminopt = Config.get_vmin_of d in
	    let prune = Config.get_prune_of d in
	      match vminopt with
		  None -> values
		| Some vmin ->
		    let vmin_idx = Misc.get_idx_of_version varray vmin in
		      Array.mapi (crop_array prune vmin_idx) values
	  end
      | _ -> values (* TODO: Crop according to *EACH* pattern *)

let mark_vmin debug name atts curves =
  List.fold_left
    (fun tail curve ->
       let (prj, pattern,catts,_) = curve in
       let d_list = Config.get_pattern atts pattern catts in
	 match d_list with
	     [d] ->
	       begin
		 let vminopt = Config.get_vmin_of d in
		   match vminopt with
		       None -> tail
		     | Some vmin ->
			 let linetype = Graph.get_linetype debug name atts curve "linetype solid" in
			 let color   = Graph.get_color debug name atts curve in
			 let marktype = Graph.get_marktype debug name atts curve "" in
			 let marksize = Graph.get_marksize debug name atts curve "" in
			 let p = Config.get_project atts prj in
			 let (depth, varray) = Config.get_versinfos p in
			 let vmin_idx = Misc.get_idx_of_version varray vmin in
			 let (_, vmind, _, _) = Array.get varray vmin_idx in
			   (linetype, color, marktype, marksize, vmind)::tail
	       end
	   | _ -> tail (* TODO: Handle multi-pattern curve *)
    ) [] curves

let get_scmpath scmfeature prj =
  if scmfeature then
    let scm = try Config.get_scm prj with _ -> "" in
      Str.replace_first (Str.regexp_string "git:") "" scm
  else
    ""

(*
let build_evolution_for_org vb debug name grinfo scmfeature atts curve allbugs evolfunc project patt info =
  let (linetype, color, marktype, marksize, label) = info in
  let (_, _, catts, _) = curve in
  let prj = Config.get_project (catts@atts) project in
    try
      let bugset = [] in (* FIXME: Nico *)
      let info = snd (List.find (by_bugset bugset) allbugs) in
      let (varray, fel, bl, _) = info in
      let vminopt = Config.get_vmin_of patt in
      let scmpath = get_scmpath scmfeature prj in
      let bugs =
	try
	  if scmpath <> "" then prerr_endline (prj^"/"^patt^": Using git info. This may take some time.");
	    compute_graph vb debug "" fel bl varray scmpath vminopt;
	with FileNotFound file ->
	  prerr_endline ("*** ERROR *** Unable to find "^file^" when generating "^name^
			   "\nCheck/Refresh exist file for project "^prj^ " and pattern "^patt);
	  []
      in
      let values = evolfunc varray bugs grinfo vminopt in
	Some (varray, linetype, color, marktype, marksize, label, bugset, crop varray atts curve values)
    with _ ->
      prerr_endline ("*** ERROR *** Pb in " ^ name ^" curve " ^ prj ^ " and "^patt);
      None
*)

let build_evolution_for_single vb debug name grinfo scmfeature atts curve allbugs evolfunc project patt =
  let linetype = Graph.get_linetype debug name atts curve "linetype solid" in
  let color   = Graph.get_color debug name atts curve in
  let marktype = Graph.get_marktype debug name atts curve "" in
  let marksize = Graph.get_marksize debug name atts curve "" in
  let label = Graph.get_label curve "noname" in
  let ytype = Config.get_ytype "" atts in

    match ytype with
	"size" | "sizepct" | "usersize" -> (** For 'size' graph, we don't have to deal with bugs. **)
	  (match curve with
	       (Some p,_,_,_) ->
		 let (_, varray) = Config.get_versinfos p in
(* 		 let evolfunc = if ytype = "usersize" then Size.evolution else evolfunc in *)
		 let values = evolfunc varray [] grinfo None in
		   Some (varray, linetype, color, marktype, marksize, label, "",crop varray atts curve values)
	     | (_,_,_,pos) ->
		 Misc.report_error pos "project must be defined to draw 'size' and 'sizepct' graphs."
	  )

      | _  ->
	  let bugset = "" in (* FIXME NICO *)
	  let info = snd (List.find (by_bugset bugset) allbugs) in
	  let (varray, fel, bl, _) = info in
	  let values = transpose varray fel in
	    Some (varray, linetype, color, marktype, marksize, label, bugset, crop varray atts curve values)

let build_simple_evolution vb debug name scmfeature atts curve info =
  let linetype = Graph.get_linetype debug name atts curve "linetype solid" in
  let color   = Graph.get_color debug name atts curve in
  let marktype = Graph.get_marktype debug name atts curve "" in
  let marksize = Graph.get_marksize debug name atts curve "" in
  let label = Graph.get_label curve "noname" in
  let (query, (varray, data)) = info in
  let values = wrap_single_some data in
    Some (varray, linetype, color, marktype, marksize, label, query, crop varray atts curve values)

let is_align varray1 varray2 =
  let dlist1 = Array.fold_left (fun acc (_, d,_,_) -> d::acc) [] varray1 in
  let dlist2 = Array.fold_left (fun acc (_, d,_,_) -> d::acc) [] varray2 in
    dlist1 = dlist2

let merge_float op v1 v2 : float option =
  match op with
      Add -> Some (v1 +. v2)
    | Sub -> Some (v1 -. v2)
    | Mul -> Some (v1 *. v2)
    | Div ->
	if v2 <> 0.0 then Some (v1 /. v2) else
	  if v1 = 0.0 then None
	  else raise (Unknown ("Div by zero of "^string_of_float v1))

let merge_opt_value op v1opt v2opt : float option =
  match v1opt, v2opt with
      Some v1, Some v2 -> merge_float op v1 v2
    | _      , None    -> v1opt
    | None   , _       -> v2opt

let merge_value d op v1 v2 =
  match (v1, v2) with
      Single v1opt, Single v2opt           -> Single (merge_opt_value op v1opt v2opt)
    | Single v1opt, Duple (v21opt, v22opt) -> Duple (merge_opt_value op v1opt v21opt, merge_opt_value op v1opt v22opt)
    | Duple (v11opt, v12opt), Single v2opt -> Duple (merge_opt_value op v11opt v2opt, merge_opt_value op v12opt v2opt)
    | Duple (v11opt, v12opt), Duple (v21opt, v22opt) ->
	Duple (merge_opt_value op v11opt v21opt, merge_opt_value op v12opt v22opt)

let merge d op v1 v2 =
  match v1, v2 with
      Value (varray1, linetype, color, marktype, marksize, label, bugset, data1),
      Value (varray2, _, _, _, _, _, _, data2) ->
	if not (is_align varray1 varray2) then
	  (prerr_endline ("*** ERROR *** Data of user-define graph are inconsistent. They should be synchronized.");
	   Empty)
	else
	  let newvalues = Array.mapi (fun i v -> merge_value d op v (Array.get data2 i)) data1 in
	    if d then prerr_string "Merging 2 sets -> ";
	    if d then prerr_endline ("New set size: "^string_of_int (Array.length newvalues));
	    Value (varray1, linetype, color, marktype, marksize, label, bugset, newvalues)
    | Cst c,
	Value (varray, linetype, color, marktype, marksize, label, bugset, data)
    | Value (varray, linetype, color, marktype, marksize, label, bugset, data),
	Cst c ->
	let newvalues = Array.mapi (fun i v -> merge_value d op v (Single (Some c))) data in
	  if d then prerr_endline "Merging a set with cst";
	  if d then prerr_endline ("New set size: "^string_of_int (Array.length newvalues));
	  Value (varray, linetype, color, marktype, marksize, label, bugset, newvalues)
    | Cst c1, Cst c2 ->
	begin
	  match merge_float op c1 c2 with
	      None -> raise (Misconfigured "A data attribute performs a division by zero!")
	    | Some c -> Cst c
	end
    | _, _ -> Empty

let rec eval_data d ctxt e : eval_type =
  match e with
      Ast_config.Pattern patt ->
	let (vb, debug, name, grinfo, scmfeature, atts, curve, allbugs, evolfunc) = ctxt in
	let (project, _, catts, pos) = curve in
	let newcurve = (project, Some patt, catts, pos) in
	let v = build_evolution_for_single vb debug name grinfo scmfeature atts newcurve allbugs evolfunc project patt in
	  (match v with
	       None -> Empty
	     | Some data ->
		 let (varray, linetype, color, marktype, marksize, label, bugset, values) = data in
		   if d then prerr_endline ("Set size: "^string_of_int (Array.length values));
		   Value (varray, linetype, color, marktype, marksize, label, bugset, values)
	  )

    | Ast_config.Project prj ->
	let (vb, debug, name, grinfo, scmfeature, atts, curve, allbugs, evolfunc) = ctxt in
	let (_, pattern, catts, pos) = curve in
	let newcurve = (Some prj, pattern, catts, pos) in
	let v = build_evolution_for_single vb debug name grinfo scmfeature atts newcurve allbugs evolfunc (Some prj) "" in
	  (match v with
	       None -> Empty
	     | Some data ->
		 let (varray, linetype, color, marktype, marksize, label, bugset, values) = data in
		   if d then prerr_endline ("Set size: "^string_of_int (Array.length values));
		   Value (varray, linetype, color, marktype, marksize, label, bugset, values)
	  )

    | Ast_config.Cst     c        -> Cst c
    | Ast_config.Plus    (e1, e2) -> merge d Add (eval_data d ctxt e1)(eval_data d ctxt e2)
    | Ast_config.Minus   (e1, e2) -> merge d Sub (eval_data d ctxt e1)(eval_data d ctxt e2)
    | Ast_config.Mul     (e1, e2) -> merge d Mul (eval_data d ctxt e1)(eval_data d ctxt e2)
    | Ast_config.Div     (e1, e2) ->
	let v2 = eval_data d ctxt e2 in
	  try
	    merge d Div (eval_data d ctxt e1) v2
	  with Unknown msg ->
	    let (_, _, name, _, _, _, _, _, _) = ctxt in
	    match v2 with
		Value (varray, linetype, color, marktype, marksize, label, bugset, values) ->
		    prerr_endline ("*** ERROR *** Processing curve "^label ^" in "^name^": "^msg);
		    Empty
	      | Cst _ ->
		  prerr_endline ("*** ERROR *** Processing graph "^name^": "^msg);
		  Empty
	      | Empty -> Empty

let build_evolution_of_size vb debug name grinfo scmfeature atts curve evolfunc =
  Debug.profile_code "Helper.build_evolution_of_size"
    (fun () ->
       let linetype = Graph.get_linetype debug name atts curve "linetype solid" in
       let color   = Graph.get_color debug name atts curve in
       let marktype = Graph.get_marktype debug name atts curve "" in
       let marksize = Graph.get_marksize debug name atts curve "" in
       let label = Graph.get_label curve "noname" in

	 match curve with
	     (Some p,_,_,_) ->
	       let (_, varray) = Config.get_versinfos p in
	       let values = evolfunc varray [] grinfo None in
		 Some (varray, linetype, color, marktype, marksize, label, "",crop varray atts curve values)

	   | (_,_,_,pos) ->
	       Misc.report_error pos "project must be defined to draw 'size' and 'sizepct' graphs."
    )

let build_evolution vb debug conn name grinfo scmfeature atts curve : value option =
  Debug.profile_code "Helper.build_evolution"
    (fun () ->
       try
	 build_simple_evolution vb debug name scmfeature atts curve (get_data_of debug conn curve)
       with _ ->
(* 	 Printexc.print_backtrace stderr; *)
	 None
(*
       let patt_list = Config.get_pattern atts pattern catts in
	 match patt_list with
	     [] ->  (* For size and sizepct graphs *)
	       build_evolution_of_size vb debug name grinfo scmfeature atts curve evolfunc
	   | [patt] ->
		 build_evolution_for_single vb debug name grinfo scmfeature atts curve data evolfunc project patt
	   | _ ->
	       raise Unimplemented
*)
    )

let build_evolutions vb debug conn name grinfo scmfeature atts curves =
  List.fold_left
    (fun evols curve ->
       match build_evolution vb debug conn name grinfo scmfeature atts curve with
	   None      -> evols
	 | Some evol -> evol::evols
    ) [] curves

let get_xmax vers =
  let (_, xmax, _, _) = Array.get vers (Array.length vers - 1) in
    1.02 *. float_of_int xmax

let get_value_of_opt optvalue =
  match optvalue with
      None   -> 0.0
    | Some v -> v

let get_value_of valuedt =
  match valuedt with
      Single v       -> (0.0, get_value_of_opt v)
    | Duple (v1, v2) ->
	(
	  get_value_of_opt v1
	    ,
	  get_value_of_opt v2
	)

let get_max arrayopt =
  let array = Array.map (fun max ->
			   snd (get_value_of max)
			) arrayopt in
  Array.fold_left max 0.0 array

let get_ymax1 evols =
  let sizes = List.map (fun (_,_,_,_,_,_, _,evol) -> get_max evol) evols in
  let ymax = List.fold_left max 0.0 sizes in
    (1.02 *. ymax)

let bound_duple (max1, min1) (max2, min2) =
  (max max1 max2, min min1 min2)

let get_bound arrayopt =
  let array = Array.map (fun valuedt  -> get_value_of valuedt) arrayopt in
  Array.fold_left bound_duple (0.0, 0.0) array

let get_ymax2 evols =
  let sizes = List.map (fun (_,_,_,_,_,_, _, evol) -> get_bound evol) evols in
  let (ymin, ymax) = List.fold_left bound_duple (0.0, 0.0) sizes in
    (1.02 *. ymin, 1.02 *. ymax)

(* Retrieve graph info *)
let get_info debug name atts xdft ydft fdft =
  Debug.profile_code "Helper.get_info"
    (fun () ->
       let size = Config.get_size atts in
       let xaxis  = Graph.get_xtype name atts in
       let legend  = Graph.get_legend debug name atts "" in
       let xlegend = Graph.get_xlegend debug name atts xdft in
       let ylegend = Graph.get_ylegend debug name atts ydft in
       let ylegfactor = Graph.get_ylegendfactor debug name atts in
       let factor = Graph.get_factor debug name atts fdft in
	 (size, xaxis, legend, xlegend, ylegend, ylegfactor, factor)
    )

(* Drawing functions *)

let draw_header ch xdftmin xdftmax ymax (size, xaxis, legend, xlabel, ylabel, ylabfact, _) vlist =
  let (xlegend, xmark, xmin, xmax) =
    match xaxis with
	"version" -> (get_names_by_date vlist, get_mark_date vlist, xdftmin, xdftmax)
      | "date" ->
	  let extradays = 100 in
	    (get_years_with_mark extradays vlist, "", xdftmin, xdftmax +. float_of_int(extradays))
      | _ -> raise (Unknown "xaxis type")
  in
  let (gxsize, gysize) =
    match size with
	None -> (2.5, 1.0)
      | Some s -> s
  in
    Printf.fprintf ch "
newgraph
xaxis min %02f max %02f size %02.2f label fontsize 11 : %s
no_auto_hash_labels no_auto_hash_marks
(* hash_labels rotate 45 vjc hjr *)
hash_labels fontsize 6 rotate -45 vjc hjl
draw_hash_labels
%s
%s

yaxis min %02.4f max %02.4f size %02.2f label fontsize 9 : %s
mhash 0 hash_labels fontsize 8

%s\n\n"
      xmin (xmax +. 1.0) gxsize xlabel
      xlegend xmark
      (fst ymax)(snd ymax) gysize ylabel
      legend

let get_point v mindate d value =
  let dayname = Misc.string_of_date d in
  let dn = Config.get_rel_days_of mindate d in
  let pos = Printf.sprintf "% 5d % 12.6f" dn value in
    Printf.sprintf "%s (* %s %s *)\n" pos v dayname

let value_to_ch ch v mindate d valueopt =
  match valueopt with
      None -> () (* Pattern doesn't apply to this version.. skip *)
    | Some value ->
	let point = get_point v mindate d value in
	  Printf.fprintf ch "%s" point

let value_to_str v mindate d valueopt =
  match valueopt with
      None -> "" (* Pattern doesn't apply to this version.. skip *)
    | Some value ->
	get_point v mindate d value

let print_pt ch pt =
  if pt != "" then Printf.fprintf ch "%s" pt

let mark_zero_once = ref false

let mark_zero ch mindate vlist =
  if not !mark_zero_once then
    let (_,_,d,_) = Array.get vlist (Array.length vlist -1) in
    let dn = Config.get_rel_days_of mindate d + 100 in
      Printf.fprintf ch "\nnewcurve linetype solid color 0 0 0 marktype none\npts\n";
      Printf.fprintf ch "0 0\n%4d 0\n" dn;
      mark_zero_once := true

let draw_vmin ch (ymin, ymax) (linetype, color, marktype, marksize, vmin) =
  Debug.profile_code "Helper.draw_vmin"
    (fun () ->
       Printf.fprintf ch "newcurve marktype none linetype dotted color %s pts\n" color;
       Printf.fprintf ch "% 4d % 4f\n"   vmin ymin;
       Printf.fprintf ch "% 4d % 4f\n\n" vmin ymax;
       Printf.fprintf ch "newcurve %s%s color %s pts\n" marktype marksize color;
       Printf.fprintf ch "% 4d % 4f\n\n" vmin (0.95 *. ymax)
    )

let draw_curve ch msg mindate ((vlist, linetype, color, marktype, marksize, label, file, evol):value) =
  Debug.profile_code "Helper.draw_curve"
    (fun () ->
       Printf.fprintf ch ("(* %s %s *)\n") msg file;
       let cplxduple = Array.to_list (Array.mapi (fun i (v,_,d,_) ->
						    let valuedt = Array.get evol i in
						      match valuedt with
							  Single valueopt ->
							    (value_to_str v mindate d valueopt, "")
							| Duple (v1opt, v2opt) ->
 							    (value_to_str v mindate d v1opt,
 							     value_to_str v mindate d v2opt)
						 ) vlist);
       in
       let (pts, cplx) = List.split cplxduple in
       let label = if label = "" then label else " label : "^label in
	 if (String.concat "" cplx) <> "" then
	   (
	     mark_zero ch mindate vlist;
	     Printf.fprintf ch "newcurve %s color %s%s%s%s\npts\n" linetype color marktype marksize label;
	     List.iter (print_pt ch) pts;
	     Printf.fprintf ch "\ncopycurve pts\n";
	     List.iter (print_pt ch) cplx;
	     Printf.fprintf ch ("\n")
	   )
	 else
	   (
	     Printf.fprintf ch "newcurve %s color %s%s%s%s\npts\n" linetype color marktype marksize label;
	     List.iter (print_pt ch) pts;
	   );
	 Printf.fprintf ch ("\n\n")
    )

let set_ymin atts (ymin, ymax) =
  match Config.get_ymin atts with
      None ->      (ymin, ymax)
    | Some ymin -> (ymin, ymax)

let set_ymax atts (ymin, ymax) =
  match Config.get_ymax atts with
      None ->      (ymin, ymax)
    | Some ymax -> (ymin, ymax)

let draw vb debug conn name grdft (atts, curves) =
  Debug.profile_code "Helper.draw"
    (fun () ->
       let (msg, xdft, ydft, fdft, xmax, ymax_f, scm, _) = grdft in
       let vers = Config.get_grversinfos atts curves in
       let gname = !Setup.prefix ^"/"^ name in
       let grinfo = get_info debug name atts xdft ydft fdft in
       let evols = build_evolutions vb debug conn name grinfo scm atts curves in
       let (_,_,mindate,_) = Array.get vers 0 in
       let xmin = Config.get_xmin debug name atts in
       let xmax = xmax vers evols in
       let ymax = set_ymax atts (set_ymin atts (ymax_f evols)) in
       let vmins = mark_vmin debug name atts curves in
       let outch = Misc.create_dir_and_open debug gname in
	 prerr_endline ("Drawing "^gname);
	 draw_header outch xmin xmax ymax grinfo vers;
	 List.iter (draw_vmin outch ymax) vmins;
	 mark_zero_once := false;
	 List.iter (draw_curve outch msg (Config.get_abs_days_of mindate)) (List.rev evols);
	 close_out outch;
	 gname
    )
