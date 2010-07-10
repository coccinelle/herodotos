
exception Unsupported of string

let emptyarr:(string * int * Unix.tm * int) array = Array.of_list []

let get_grdata_of vb name conn atts =
  let query = Config.get_query atts in
    if query <> "" then
      begin
	if vb then prerr_endline ("Query: "^query);
	(*
	  Query the database using the connection conn
	  and the query 'query'
	*)
	try
	  (query, Database.get_grtuples vb conn query)
	with _ ->
	  Printexc.print_backtrace stderr;
	  raise (Config.Misconfiguration ("Check query \""^query^"\" of "^name))
      end
    else
      raise (Config.Misconfiguration ("Empty query in graph "^name^"!"))

let get_data_of vb conn atts pos =
  let query = Config.get_query atts in
    if query <> "" then
      begin
	if vb then prerr_endline ("Query: "^query);
	(*
	  Query the database using the connection conn
	  and the query 'query'
	*)
	try
	  (query, Database.get_grtuples vb conn query)
	with _ ->
	  Printexc.print_backtrace stderr;
	  raise (Config.Misconfigurationat ("Check query \""^query^"\"", pos))
      end
    else
      raise (Config.Misconfigurationat ("Empty query!", pos))

let build_graph vb1 vb2 name atts conn =
  let linetype = "linetype none" in
(*     Graph.get_color false name [] curve *)
  let marksize = Graph.get_marksize_in_atts_or_dft vb1 name atts " marksize 0.6 " in
  let factor = Graph.get_factor vb1 name atts 1.0 in
  let info = get_grdata_of vb2 name conn atts in
  let (query, (xlegend, ylegend, data)) = info in
    List.map (fun (label, values) ->
		let curve = (None, Some label, [], Misc.dummy_pos_in name) in
		let color = Graph.get_color vb2 name atts curve in
		let wrap_values =
		  Helper.wrap_single
		    (Array.map
		       (fun vopt ->
			  match vopt with
			      None -> None
			    | Some v -> Some (v/.factor)
		       ) values
		    )
		in
		  (emptyarr, linetype, color, "", marksize, (label, ""), query, wrap_values)
	     ) data

let build_stat_of vb1 vb2 name singleprj conn scmfeature grpname atts curve =
  let linetype = "linetype none" in
  let (prjopt, pattopt, catts, pos) = curve in
  let legend  = Graph.get_label curve "" in
(*   let patt = match pattopt with None -> "" | Some v -> v in *)
  let color   = if singleprj then
    let newcurve = (None, pattopt,[], pos) in
      (*
	Fool get_color function when there is only one project.
	The color will thus default to the pattern color. :D
      *)
      Graph.get_color false name [] newcurve
  else
    Graph.get_color false name [] curve
  in
  let (grplabel, scmpath) = match prjopt with
      None -> (grpname, "")
    | Some prj -> (prj, Helper.get_scmpath scmfeature prj)
  in
  let marksize = Graph.get_marksize vb1 name atts curve " marksize 0.6 " in
  let info = get_data_of vb2 conn catts pos in
  let (query, (_, _, data)) = info in
  let (label, values) =
    if data = [] then ("", Helper.wrap_single (Array.init 1 (fun _ -> None)))
    else
      let (label, v) = List.hd data in
	(label, Helper.wrap_single v)
  in
  let curve_label = if legend = "" then label else legend in
    (emptyarr, linetype, color, "", marksize, (grplabel, curve_label), query, values)

let build_stat vb debug name conn scmfeature groups atts =
  let singleprj = if List.length groups = 1 then true else false in
    if groups <> [] then
      begin
	List.flatten
	  (List.map
	     (fun group ->
		match group with
		    Ast_config.GrpCurve (grpname, curves) ->
		      List.rev (List.fold_left
			(fun tail curve ->
			   try
			     (build_stat_of vb debug name singleprj conn scmfeature grpname atts curve)::tail
			   with
			       Config.Misconfigurationat( msg, pos) ->
				 Misc.print_error pos msg;
				 tail
			     | _ -> tail
			) [] curves)
		  | Ast_config.GrpPatt _ ->
		      raise (Unsupported "group pattern")
	     ) groups
	  )
      end
    else
      build_graph vb debug name atts conn

let compute_labels evols fstep =
  let labels = Misc.unique_list (List.map (fun  (_, _, _, _, _, (label,_), _, _) -> label) evols) in
  let array = Array.of_list (List.rev labels) in
  let offset = 0.25 *. fstep in
  let arrlabels = Array.mapi
    (fun i label ->
       let pos = float_of_int i *. fstep +. offset in
	 Printf.sprintf "hash_label at % 2.2f : %s\n" pos label
    ) array
  in
    String.concat "" (Array.to_list arrlabels)

(* Drawing functions *)
let rec compute_marks xdftmax step =
    if xdftmax > 0.0 then
      let head = compute_marks (xdftmax -. step) step in
	head ^ (Printf.sprintf "hash_at % 2.2f\n" xdftmax)
    else
      ""

let draw_header ch xdftmax ymax (size, xaxis, legend, xlabel, ylabel, ylabfact, _) step evols =
  let (xlegend, xmark, xmax) =
    match xaxis with
	"groups" ->
	  let fstep = xdftmax /. float_of_int step in
	  let marks = compute_marks (xdftmax -. fstep) fstep in
	  (compute_labels evols fstep, marks, xdftmax)
      | _ -> raise (Unsupported "xaxis type")
  in
  let (gxsize, gysize) =
    match size with
	None -> (max 2.5 (0.5 *. (float_of_int step)), 1.0)
      | Some s -> s
  in
    Printf.fprintf ch "
newgraph
xaxis min 0 max %02.4f size %02.2f label fontsize 11 : %s
no_auto_hash_labels no_auto_hash_marks
hash_labels fontsize 6 rotate -45 vjb hjl
draw_hash_labels
%s
%s

yaxis min %02.4f max %02.4f size %02.2f label fontsize 9 : %s
mhash 1 hash_labels fontsize 8

%s\n\n"
      xmax gxsize xlabel
      xlegend xmark
      (fst ymax) (snd ymax) gysize ylabel
      legend

let _labellist:string list ref = ref []

let check_label label =
  if (not (List.mem label !_labellist))
   && (label <> "")
  then
    begin
      _labellist := label::!_labellist;
      Printf.sprintf " label : %s" label
    end
  else ""

let draw_curve ch msg prjnum ynodata idx (_, linetype, color, _, marksize, (_,label), file, data) =
  let labelstr = check_label label in
(*   let labelstr = if idx < prjnum then Printf.sprintf " label : %s" label else "" in *)
    Printf.fprintf ch ("(* %s\n\t%s *)\n") msg file;
    Printf.fprintf ch "newcurve marktype xbar %s color %s %s%s\npts\n" linetype color marksize labelstr;
    Array.iter (fun data_dt ->
		  match data_dt with
		      Helper.Single dopt ->
			(match dopt with
			     None ->
			       Printf.fprintf ch "newstring fontsize 4 rotate 90";
			       Printf.fprintf ch " x % 2.2f y % 2.2f hjl vjc : No data\n"
				 ((float_of_int idx) +. 0.5) ynodata
			   | Some d ->
			       if d = 0.0 then
				 (
				   Printf.fprintf ch "newstring fontsize 4 rotate 90";
				   Printf.fprintf ch " x % 2.2f y % 2.2f hjl vjc : Zero\n"
				     ((float_of_int idx) +. 0.5) ynodata
				 )
			       else
				 let pos = Printf.sprintf "% 2.2f % 4f" ((float_of_int idx) +. 0.5) d in
				   Printf.fprintf ch "%s\n" pos
			)
		    | Helper.Duple _ -> failwith "Unsupported feature: duple with group"
	       ) data;
    Printf.fprintf ch ("\n");
    idx +1

let draw vb debug conn name grdft (atts, groups) =
  _labellist := [];
  let (msg, xdft, ydft, fdft, xmax, ymax_f, scm, _) = grdft in
  let gname = !Setup.prefix ^"/"^ name in
  let outch = Misc.create_dir_and_open debug gname in
  let grinfo = Helper.get_info debug name atts xdft ydft fdft in
  let evols = build_stat vb debug name conn scm groups atts in
  let grpnum = List.length groups in
  let evolnum = List.length evols in
  let prjnum = if grpnum = 0 then evolnum else grpnum in
  let y_max = Helper.set_ymax atts (Helper.set_ymin atts (ymax_f evols)) in
  let ynodata =
    let ymax = snd y_max in
      if ymax > 10.0 then
	if ymax > 100.0 then
	  1.0
	else
	  0.1
      else 0.0
  in
    prerr_endline ("Drawing "^gname);
    draw_header outch (float_of_int evolnum) y_max grinfo prjnum evols;
    ignore(List.fold_left (draw_curve outch msg prjnum ynodata) 0 evols);
    close_out outch;
    gname
