
exception Unsupported of string

let build_stat_of vb1 vb2 name singleprj grinfo scmfeature allbugs evolfunc curve =
  let linetype = "linetype none" in
  let (prjopt, pattopt, catts, _) = curve in
  let patt = match pattopt with None -> "" | Some v -> v in
  let vminopt = Config.get_vmin_of patt in
  let color   = if singleprj then
    let newcurve = (None, pattopt,[],Misc.dummy_pos) in
      (*
	Fool get_color function when there is only one project.
	The color will thus default to the pattern color. :D
      *)
      Graph.get_color false name [] newcurve
  else
    Graph.get_color false name [] curve
  in
    (* FIXME atts of graph should not be empty. *)
  let marksize = Graph.get_marksize vb1 name [] curve " marksize 0.6 " in
  let bugset = List.hd (Config.get_bugset_of_curve [] curve) in
  let info = snd (List.find (Helper.by_bugset bugset) allbugs) in
  let (varray, fel, fbl, _) = info in
  let (label, scmpath) = match prjopt with
      None -> ("noname", "")
    | Some prj -> (prj, Helper.get_scmpath scmfeature prj)
  in
    if scmpath <> "" then (
      prerr_endline (label^"/"^patt^": Using git info. This may take some time.");
      Git.load_authors scmpath
    );
    let pdir = match prjopt with None -> "" | Some prj -> Config.get_prjdir prj in
    let prefix = !Setup.projectsdir ^ pdir ^"/" in
    let bugs = Helper.compute_graph vb1 vb2 prefix fel fbl varray scmpath vminopt in
    let values = evolfunc varray bugs grinfo vminopt in
      (varray, linetype, color, "", marksize, label, bugset, values)

let build_stat vb debug name grinfo scmfeature projects groups allbugs evolfunc =
  let singleprj = if List.length projects = 1 then true else false in
    List.flatten
      (List.map
	 (fun group ->
	    match group with
		Ast_config.GrpCurve (_, curves) ->
		  List.map (build_stat_of vb debug name singleprj grinfo scmfeature allbugs evolfunc) curves
	      | Ast_config.GrpPatt (patt, _) ->
		  List.map (fun prj ->
			      let curve = (Some prj, Some patt,[],Misc.dummy_pos) in
				build_stat_of vb debug name singleprj grinfo scmfeature allbugs evolfunc curve
			   ) projects
	 ) groups
      )

let compute_labels groups fstep =
  let array = Array.of_list groups in
  let offset = 0.25 *. fstep in
  let arrlabels = Array.mapi
    (fun i group ->
       let pos = float_of_int i *. fstep +. offset in
       match group with
	   Ast_config.GrpCurve (name, _) ->
	     Printf.sprintf "hash_label at % 2.2f : %s\n" pos name
	 | Ast_config.GrpPatt (patt, _) ->
	     Printf.sprintf "hash_label at % 2.2f : %s\n" pos patt
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

let draw_header ch xdftmax ymax (size, xaxis, legend, xlabel, ylabel, ylabfact, _) step groups =
  let (xlegend, xmark, xmax) =
    match xaxis with
	"groups" ->
	  let fstep = float_of_int step in
	  let marks = compute_marks (xdftmax -. fstep) fstep in
	  (compute_labels groups fstep, marks, xdftmax)
      | _ -> raise (Unsupported "xaxis type")
  in
  let (gxsize, gysize) =
    match size with
	None -> (2.5, 1.0)
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

yaxis size 1 min %02.4f max %02.4f size %02.2f label fontsize 9 : %s
mhash 1 hash_labels fontsize 8

%s\n\n"
      xmax gxsize xlabel
      xlegend xmark
      (fst ymax) (snd ymax) gysize ylabel
      legend

let draw_curve ch msg prjnum idx (_, linetype, color, _, marksize, label, file, data) =
  let labelstr = if idx < prjnum then Printf.sprintf " label : %s" label else "" in
    Printf.fprintf ch ("(* %s %s *)\n") msg file;
    Printf.fprintf ch "newcurve marktype xbar %s color %s %s%s\npts\n" linetype color marksize labelstr;
    Array.iter (fun data_dt ->
		  match data_dt with
		      Helper.Single dopt ->
			(match dopt with
			     None ->
			       Printf.fprintf ch "newstring fontsize 6 rotate 90";
			       Printf.fprintf ch " x % 2.2f y 1 hjl vjc : No data\n"
				 ((float_of_int idx) +. 0.5)
			   | Some d ->
			       let pos = Printf.sprintf "% 2.2f % 4f" ((float_of_int idx) +. 0.5) d in
				 Printf.fprintf ch "%s\n" pos
			)
		    | Helper.Duple _ -> failwith "Unsupported feature: duple with group"
	       ) data;
    Printf.fprintf ch ("\n");
    idx +1

let draw vb debug name grdft (atts, groups) allbugs =
  let (msg, xdft, ydft, fdft, xmax, ymax, scm, evolfunc) = grdft in
  let gname = !Setup.prefix ^"/"^ name in
  let outch = Misc.create_dir_and_open debug gname in
  let grinfo = Helper.get_info debug name atts xdft ydft fdft in
  let projects = Config.get_projects atts in
  let prjnum = List.length projects in
  let evols = build_stat vb debug name grinfo scm projects groups allbugs evolfunc in
    prerr_endline ("Drawing "^gname);
    draw_header outch (xmax (Array.of_list []) evols) (ymax evols) grinfo prjnum groups;
    ignore(List.fold_left (draw_curve outch msg prjnum) 0 evols);
    Printf.fprintf outch "%s" (Graph.get_footer atts);
    close_out outch;
    gname
