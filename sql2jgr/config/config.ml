
open Lexing

exception Not_Found
exception Unrecoverable
exception Warning of string
exception Misconfiguration of string
exception Misconfigurationat of string * Ast.pos

let parse_config file : unit =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    Misc.init file lexbuf;
    let ast = Config_parser.main Config_lexer.token lexbuf in
      close_in in_ch;
      ast
  with
      (Config_lexer.Lexical msg) ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Config Lexer Error: " ^ msg);
    | Config_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Config Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")

let get_abs_days_of tm =
  (* 60.0 (s) *. 60.0 (mn) *. 24.0  (h) = 86400.0 *)
  fst (Unix.mktime tm) /. (86400.0)

let get_rel_days_of refday tm =
  let ds = get_abs_days_of tm in
    int_of_float (ds -. refday)

let get_versinfo dm (name, date, size) =
  let days = get_rel_days_of dm date in
    (name, days, date, size)

let rec get_vers_min vs =
  match vs with
      [] -> raise Unrecoverable
    | hd::tail ->
	let (_, dhd, _) = hd in
	let m = get_abs_days_of dhd in
	  try
	    let dtail = get_vers_min tail in
	      min m dtail
	  with _ -> m

let sort_by_date v1 v2 =
  let (_,d1,_,_) = v1 in
  let (_,d2,_,_) = v2 in
    compare d1 d2

let compute_versinfos vs =
  let m = get_vers_min vs in
  let list = List.map (get_versinfo m) vs in
  let ordered = List.sort sort_by_date list in
    Array.of_list ordered

let get_versinfos p =
  try
    let (optarray, atts) = Setup.PrjTbl.find Setup.projects p in
      match optarray with
	  None ->
	    begin
	      try
		match
		  List.find (fun x ->
			       match x with
				   Ast_config.Version _ -> true
				 | _ -> false
			    ) atts
		with
		    Ast_config.Version (depth, vs) -> (depth, compute_versinfos vs)
		  | _ -> raise Unrecoverable
	      with _ ->
		raise (Warning ("project versions are not set for "^p))
	    end
	| Some (depth, array) -> (depth, array)
  with Not_found ->
    raise (Misconfiguration ("project "^p^" is not declared"))

let show_attr attr =
  if !Misc.debug then
    match attr with
	Ast_config.Color (r,g,b) -> prerr_endline ("color = "^Misc.string_of_rgb (r,g,b))
      | Ast_config.Correl mode -> prerr_endline ("correl = "^mode)
      | Ast_config.CleanColor (r,g,b) -> prerr_endline ("cleancolor = "^Misc.string_of_rgb (r,g,b))
      | Ast_config.PatternColor (r,g,b) -> prerr_endline ("patterncolor = "^Misc.string_of_rgb (r,g,b))
      | Ast_config.DftPattern (s) -> prerr_endline ("pattern = "^s)
      | Ast_config.Data (e) -> prerr_endline ("data = <FIXME: Needed ?>")
      | Ast_config.SQLData (e) -> prerr_endline ("sql = "^ e)
      | Ast_config.Dir (s) -> prerr_endline ("dir = "^s)
      | Ast_config.SubDir (s) -> prerr_endline ("subdir = "^s)
      | Ast_config.Factor (f) -> prerr_endline ("factor = "^string_of_float f)
      | Ast_config.File (sopt) ->
	  (match sopt with
	       None -> prerr_endline ("file = none")
	     | Some s -> prerr_endline ("file = \""^s^"\"")
	  )
      | Ast_config.Filename b -> prerr_endline ("filename = "^Misc.string_of_bool b)
      | Ast_config.Footer f -> prerr_endline ("footer = \""^f^"\"")
      | Ast_config.Format mode -> prerr_endline ("format = "^mode)
      | Ast_config.Author (b) -> prerr_endline ("author = "^Misc.string_of_bool b)
      | Ast_config.Info b -> prerr_endline ("info = "^Misc.string_of_bool b)
      | Ast_config.DftProject (s) -> prerr_endline ("project = "^s)
      | Ast_config.Legend (s) -> prerr_endline ("legend = \""^s^"\"")
      | Ast_config.XLabel (s) -> prerr_endline ("xlabel = \""^s^"\"")
      | Ast_config.YLabel (s) -> prerr_endline ("ylabel = \""^s^"\"")
      | Ast_config.XLegend (s) -> prerr_endline ("xlegend = \""^s^"\"")
      | Ast_config.YLegend (s) -> prerr_endline ("ylegend = \""^s^"\"")
      | Ast_config.YLegendFactor (s) -> prerr_endline (" = "^s)
      | Ast_config.LineType (s) -> prerr_endline ("linetype = "^s)
      | Ast_config.MarkType (s) -> prerr_endline ("marktype = "^s)
      | Ast_config.MarkSize (v) -> prerr_endline ("marksize = "^string_of_float v)
      | Ast_config.XMin (v) -> prerr_endline ("xmin = "^string_of_float v)
      | Ast_config.YMin (v) -> prerr_endline ("ymin = "^string_of_float v)
      | Ast_config.YMax (v) -> prerr_endline ("ymax = "^string_of_float v)
      | Ast_config.XAxis (s) -> prerr_endline ("xaxis = "^s)
      | Ast_config.YAxis (s) -> prerr_endline ("yaxis = "^s)
      | Ast_config.Ratio b -> prerr_endline ("ratio = "^Misc.string_of_bool b)
      | Ast_config.NotExistColor (r,g,b) -> prerr_endline ("notexistcolor = "^Misc.string_of_rgb (r,g,b))
      | Ast_config.Version (_, vs) ->
	  (try
	     prerr_endline "version = {";
	     (*
	       let m = get_vers_min vs in
	       List.iter (show_version m) vs;
	     *)
	   with _ -> prerr_endline "No version set !"
	  );
	  prerr_endline "}"
      | Ast_config.VMin (s) -> prerr_endline ("vmin = \""^s^"\"")
      | Ast_config.Prune (b) -> prerr_endline ("prune = "^Misc.string_of_bool b)
      | Ast_config.Flags (s) -> prerr_endline ("flags = \""^s^"\"")
      | Ast_config.SCM (s) -> prerr_endline ("scm = \""^s^"\"")
      | Ast_config.Size (x,y) -> prerr_endline ("size = "^string_of_float x ^ "x" ^ string_of_float y)
  else
    match attr with
      | Ast_config.Version (_, vs) ->
	  ()
	    (*
	  (try
	    let m = get_vers_min vs in
	      List.iter (show_version m) vs
	  with _ -> prerr_endline "\tNo version set !"
	  )
	    *)
      |_ -> ()

let get_query atts =
  List.fold_left
    (fun q att ->
       match att with
	   Ast_config.SQLData s -> s
	 | _ -> q
    ) "" atts

let get_dft_project atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.DftProject s -> true
		     | _ -> false
		) atts
    with
	Ast_config.DftProject s -> s
      | _ -> raise Unrecoverable
  with Not_found ->
    raise (Misconfiguration "curve: project is not set.")

let get_projects atts =
  try
    List.map (fun x ->
		match x with
		    Ast_config.DftProject s -> s
		  | _ -> raise Unrecoverable
	     )
      (List.find_all (fun x ->
		       match x with
			   Ast_config.DftProject s -> true
			 | _ -> false
		    ) atts
      )
  with _ ->
    raise (Misconfiguration "curve: project is not set.")

let get_project catts project =
  match project with
      Some s -> s
    | None -> get_dft_project catts


let get_xtype g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.XAxis _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.XAxis t -> t
      | _ -> raise Unrecoverable
  with _ ->
    raise (Misconfiguration ("XAxis type of "^g^" is not defined"))

let get_ytype g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YAxis _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YAxis t -> t
      | _ -> raise Unrecoverable
  with _ ->
    raise (Misconfiguration ("YAxis type of "^g^" is not defined"))

let get_dft_pattern gatts latts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.DftPattern s -> true
		     | _ -> false
		) gatts
    with
	Ast_config.DftPattern s -> [s]
      | _ -> raise Unrecoverable
  with _ ->
    match get_ytype "" gatts with
	"size" | "sizepct" | "usersize" -> []
      | _      -> raise (Misconfiguration "curve: pattern is not set")

let get_pattern gatts pattern catts =
  match pattern with
      Some d -> [d]
    | None -> get_dft_pattern gatts catts

let get_xmin db g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.XMin _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.XMin v -> v
      | _ -> raise Unrecoverable
  with _ ->
    if db then prerr_endline ("XMin of "^g^" is not defined. Assuming 0.");
    0.0

let get_ymax atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YMax _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YMax v -> Some v
      | _ -> raise Unrecoverable
  with Not_found -> None

let get_ymin atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YMin _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YMin v -> Some v
      | _ -> raise Unrecoverable
  with Not_found -> None

let get_scm prj =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects prj) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.SCM _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.SCM s -> s
	  | _ -> raise Unrecoverable
      with _ ->
	raise (Misconfiguration "project source code repository is not set")
  with Not_found ->
	raise (Misconfiguration ("project "^prj^" is not declared"))

let get_vmin atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.VMin _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.VMin v -> Some v
      | _ -> None
  with _ -> None

let get_prune atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Prune _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Prune v -> v
      | _ -> false
  with _ -> false

let get_size atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Size _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Size (x,y) -> Some (x,y)
      | _ -> None
  with _ -> None

let get_vmin_of d =
  try
    let atts = Setup.DftTbl.find Setup.smatchs d in
      get_vmin atts
  with Not_found -> None

let get_prune_of d =
  try
    let atts = Setup.DftTbl.find Setup.smatchs d in
      get_prune atts
  with Not_found -> false

let get_versions p =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects p) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.Version _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.Version (depth, v) ->
	      (depth, List.map (fun (n, _, _) -> n) v)
	  | _ -> (0, [])
      with _ -> (0, [])
  with Not_found ->
    raise (Misconfiguration ("project "^p^" is not declared"))

let get_projects_of gatts curves =
  Misc.unique_list (
    List.flatten
      (List.map
	 (fun (project, _,atts,_) ->
	    try
	      [get_project (gatts@atts) project]
	    with _ ->
	      get_projects (gatts@atts)
	 ) curves
      )
  )

let update_days mindate elem =
  let (name, _, date, size) = elem in
  let days = get_rel_days_of mindate date in
    (name, days, date, size)

let get_min_of_arrays curmin array =
  let (_,_,locmin,_) = Array.get array 0 in
  let locmindays = get_abs_days_of locmin in
    if locmindays < curmin || curmin = 0.0 then
      locmindays
    else
      curmin

let get_grversinfos gatts curves =
  Debug.profile_code "Config.get_grversinfos"
    (fun () ->
       let vinfosarr =
	 List.fold_left (fun arrs x ->
			   let v =
			     try
			       Some (snd (get_versinfos x))
			     with Warning _ -> None
			   in
			     match v with
				 None -> arrs
			       | Some a -> a::arrs
			) [] (get_projects_of gatts curves)
       in
       let mindate = List.fold_left (get_min_of_arrays) 0.0 vinfosarr in
       let vinfos = List.map Array.to_list vinfosarr in
       let uniquelist = Misc.unique_list (List.flatten vinfos) in
       let updinfos = List.map (update_days mindate)  uniquelist in
       let sortedlist = List.sort sort_by_date updinfos in
       let sorted = Array.of_list sortedlist in
	 sorted
    )

(*
  Helper functions to retrieve color
*)

let get_rgb_in_atts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Color _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Color (r,v,b) -> (r,v,b)
      | _ -> raise Unrecoverable
  with _ ->
    raise Not_Found

let get_rgb_color verbose g gatts curve =
  let (project, defect, atts, pos) = curve in
    try (* Check if color is set for this curve *)
      get_rgb_in_atts atts
    with Not_Found -> (* If not, get dft color from project/defect according to curve def. *)
      try
	match (project, defect) with
	  | None, Some d -> (* Defect is set for this curve,
			       Project should be cst for the graph.
			       We pick the defect color
			    *)
	      begin
		try
		  let dftatts = Setup.DftTbl.find Setup.smatchs d in
		    get_rgb_in_atts dftatts
		with Not_found ->
		  raise (Misconfigurationat  ("Pattern "^d^" is not defined.", pos))
	      end
	  | Some p, None -> (* Project is set for this curve,
			       Defect should be cst for the graph.
			       We pick the project color
			    *)
	      begin
		try
		  let dftatts = snd (Setup.PrjTbl.find Setup.projects p) in
		    get_rgb_in_atts dftatts
		with Not_found ->
	      raise (Misconfigurationat ("Project "^p^" is not defined.", pos))
		  end
	  | Some p, Some d ->  (* Prj and Defect are both set ! Pick prj. We are likely in the group graph *)
	      (*
		let dftatts = snd (Setup.PrjTbl.find Setup.projects p) in
		get_rgb_in_atts dftatts
	      *)
	      raise (Misconfigurationat ("In graph "^g^", color is not defined for the curve with "^p^" and "^d^".", pos))
	  | None, None ->  (* Prj and Defect are both not set ! Pick dft *)
	      raise (Misconfigurationat ("In graph "^g^", color is not defined.",pos))
      with Not_Found ->
	raise (Misconfigurationat ("In graph "^g^", color is not defined.",pos))

let show_global () =
  prerr_endline ("Prefix = " ^ !Setup.prefix);
  prerr_endline ("Project source code in " ^ !Setup.projectsdir);
  match !Setup.cpucore with
      None ->
	let cores = string_of_int (Misc.get_number_of_cores ()) in
	prerr_endline ("CPU core = <undefined: assuming "^cores^">")
    | Some cpucore ->
	prerr_endline ("CPU core = " ^ string_of_int cpucore)

let show_curve v atts = function
  (prjopt, pattopt, catts, pos) ->
    List.iter show_attr catts

let show_config v1 v2 =
  Debug.profile_code "Config.show_config"
    (fun () ->
       show_global ();
       if not v1 then prerr_string ("Project");
       Setup.PrjTbl.iter
	 (fun name (_,atts) ->
	    if not v1 then
	      prerr_string (" "^name)
	    else
	      (
		prerr_endline ("Project "^name);
		List.iter show_attr atts
	      )
	 )
	 Setup.projects;
       prerr_string ("\nPattern");
       if v2 then prerr_newline ();
       Setup.DftTbl.iter
	 (fun name atts ->
	    prerr_string (" "^name);
	    if v2 then
	      ( prerr_newline ();
		List.iter show_attr atts
	      )
	 )
	 Setup.smatchs;
       prerr_newline ();
       if not v1 then prerr_endline ("Graph");
       Setup.GphTbl.iter
	 (fun name (atts, subgraph) ->
	    if v1 then
	      begin
	      prerr_endline ("Graph "^name);
 		List.iter show_attr atts;
		match subgraph with
		    Ast_config.Curves curves ->
 		      List.iter (show_curve v2 atts) curves
		  | Ast_config.Groups _ -> ()
	      end
	    else
	      prerr_string (" "^name)
	 )
	 Setup.graphs;
       if not v1 then prerr_newline ();
    )

let init_color ct =
  let levels = exp((log (float_of_int (ct+1(*avoid white*)))) /. 3.0) in
  let inc = 1.0 /. ((ceil levels) -. 1.0) in
  let rec loop c1 c2 c3 = function
      0  -> []
    | lct ->
        let (c3carry,c3p) =
          let c3p = c3 +. inc in
          if c3p > 1.0 then (inc,0.0) else (0.0,c3p) in
        let (c2carry,c2p) =
          let c2p = c2 +. c3carry in
          if c2p > 1.0 then (inc,0.0) else (0.0,c2p) in
        let c1p = c1 +. c2carry in
        ((c1, c2, c3)::(loop c1p c2p c3p (lct-1))) in
  loop 0.0 0.0 0.0 ct

let rec crop ct list =
  if ct = 0 then []
  else
    match list with
	[] -> []
      | hd::tail -> hd::(crop (ct-1) tail)

(* Fix curves in graphs *)
let fixcolor_of_graph name (atts, subgraph) =
  match subgraph with
      Ast_config.Curves curves ->
	let (crv_wc, crv_woc, colors) = List.fold_left
	  (fun (crv_wc, crv_woc, colors) curve ->
	     try
	       let (r,v,b) = get_rgb_color false name [] curve in
		 (crv_wc +1, crv_woc, (r,v,b)::colors)
	     with Misconfiguration msg
	       | Misconfigurationat (msg, _) ->
		   if !Misc.debug then
		     prerr_endline (msg^" Automatically picking one...");
		   (crv_wc, curve::crv_woc, colors)
	  ) (0,[],[]) curves
	in
	let count = crv_wc + (List.length crv_woc) in
	let newcolors = init_color count in
	let availcolors = List.filter (fun c -> not (List.mem c colors)) newcolors in
	let availcolors2 = crop (List.length crv_woc) availcolors in
	let newcrvs = List.fold_left2
	  (fun newcrvs curve (r,v,b) ->
	     if List.mem curve crv_woc then
	       let (project, defect, catts, pos) = curve in
	       let newcurve = (project, defect, (Ast_config.Color (r,v,b))::catts, pos) in
		 (curve,newcurve)::newcrvs
	     else
	       newcrvs
	  ) [] crv_woc availcolors2
	in
	let updcrvs = List.fold_left
	  (fun updcrvs curve ->
	     try
	       let newcurve = List.assoc curve newcrvs in
		 newcurve::updcrvs
	     with _ ->
	       curve::updcrvs
	  ) [] curves
	in
	  Setup.updGph name (atts, (Ast_config.Curves(List.rev updcrvs)))

    | Ast_config.Groups _ -> () (* FIXME *)

let fixcolor _ =
  (* Fix projects *)
  try
    let (prj_wc, prj_woc, colors) = Setup.PrjTbl.fold
      (fun name (arr,atts) (prj_wc, prj_woc, colors) ->
	 try
	   let (r,v,b) = get_rgb_in_atts atts in
	     (prj_wc +1, prj_woc, (r,v,b)::colors)
	 with _ ->
	   (prj_wc, (name, (arr,atts))::prj_woc, colors)
      ) Setup.projects (0,[],[])
    in
    let count = prj_wc + List.length prj_woc in
    let newcolors = init_color count in
    let availcolors = List.filter (fun c -> not (List.mem c colors)) newcolors in
    let availcolors2 = crop (List.length prj_woc) availcolors in
      List.iter2 (fun (name, (arr,atts)) (r,v,b) ->
		    Setup.updPrj name (arr, (Ast_config.Color (r,v,b))::atts)
		 ) prj_woc availcolors2;

      (* Fix patterns *)
      let (patt_wc, patt_woc, colors) = Setup.DftTbl.fold
	(fun name atts (patt_wc, patt_woc, colors) ->
	   try
	     let (r,v,b) = get_rgb_in_atts atts in
	       (patt_wc +1, patt_woc, (r,v,b)::colors)
	   with _ ->
	     (patt_wc, (name, atts)::patt_woc, colors)
	) Setup.smatchs (0,[],[])
      in
      let count = patt_wc + (List.length patt_woc) in
      let newcolors = init_color count in
      let availcolors = List.filter (fun c -> not (List.mem c colors)) newcolors in
      let availcolors2 = crop (List.length patt_woc) availcolors in
	List.iter2 (fun (name, atts) (r,v,b) ->
		      Setup.updDft name ((Ast_config.Color (r,v,b))::atts)
		   ) patt_woc availcolors2;

	Setup.GphTbl.iter fixcolor_of_graph Setup.graphs
  with Misconfigurationat (msg, pos) ->
    Misc.report_error pos ("*** ERROR *** "^ msg)
