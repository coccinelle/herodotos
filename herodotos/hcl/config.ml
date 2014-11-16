
open Lexing

exception Not_Found
exception Unrecoverable
exception Warning of string
exception Misconfiguration of string
exception BadDate of string
exception Misconfigurationat of string * Ast.pos

(* Parse the .projects_configfile to recover each project versions *)  
let parse_cache cache_file =
  try
    let in_ch = open_in cache_file in
    let lexbuf = Lexing.from_channel in_ch in
    try   
      Misc.init cache_file lexbuf;
      let ast = Config_parser.projects_cache Config_lexer.token lexbuf in
      close_in in_ch;
      ast
    with
	(Config_lexer.Lexical msg) ->
	  let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = cache_file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Config Lexer Error: " ^ msg);
      | Config_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	Misc.report_error
	  { Ast.file  = cache_file;
	    Ast.line  = pos.pos_lnum;
	    Ast.colfr = pos.pos_cnum - pos.pos_bol;
 	    Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	  ("Config Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")
  with _ -> LOG "File %s does not exit, run make preinit." cache_file LEVEL ERROR;
    failwith ("File "^cache_file^" does not exit, run make preinit.")

let parse_config_no_cache file : unit =
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

let parse_config file : unit =
 let cache = parse_cache (".projects_"^file) in
 let ast = parse_config_no_cache file in
 ast
   (* TODO: Merge cache and ast *)

let get_date d = match d with
    Some d -> d
  | None -> raise(BadDate "Date is not defined")

let get_abs_days_of tm =
  (* 60.0 (s) *. 60.0 (mn) *. 24.0  (h) = 86400.0 *)
  fst (Unix.mktime (get_date tm)) /. (86400.0)


let get_rel_days_of refday tm =
  let ds = get_abs_days_of tm in
    int_of_float (ds -. refday)

let get_versinfo dm (name, date, size) =
  let days = get_rel_days_of dm date in
    (name, days, date, size)

let show_versinfo (name, days, date, size) =
  ("Version "^name ^
      " released the "^ Misc.string_of_date (get_date date) ^
      " -- " ^ (string_of_int days) ^
      " ("^ (string_of_int size) ^" LOC)")

let show_version dm (name, tm, size) =
  let info = get_versinfo dm (name, tm, size) in
    show_versinfo info

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
		    Ast_config.Version (depth, vs) ->
		      let ordered = compute_versinfos vs in
		      (depth, ordered)
		  | _ -> raise Unrecoverable
	      with _ ->
		raise (Warning ("project versions are not set for "^p))
	    end
	| Some (depth, array) -> (depth, array)
  with Not_found ->
    raise (Misconfiguration ("project "^p^" is not declared"))

let rec get_expression e : string =
  match e with
      Ast_config.Pattern p -> "pattern "^p
    | Ast_config.Project p -> "project "^p
    | Ast_config.Cst     c -> string_of_float c
    | Ast_config.Plus    (e1, e2) -> "("^ (get_expression e1) ^ " + " ^(get_expression e2)^ ")"
    | Ast_config.Minus   (e1, e2) -> "("^ (get_expression e1) ^ " - " ^(get_expression e2)^ ")"
    | Ast_config.Mul     (e1, e2) -> "("^ (get_expression e1) ^ " * " ^(get_expression e2)^ ")"
    | Ast_config.Div     (e1, e2) -> "("^ (get_expression e1) ^ " / " ^(get_expression e2)^ ")"

let show_attr attr : string =
  match attr with
      Ast_config.Color (r,g,b) -> ("color = "^Misc.string_of_rgb (r,g,b))
    | Ast_config.Correl mode -> ("correl = "^mode)
    | Ast_config.CleanColor (r,g,b) -> ("cleancolor = "^Misc.string_of_rgb (r,g,b))
    | Ast_config.PatternColor (r,g,b) -> ("patterncolor = "^Misc.string_of_rgb (r,g,b))
    | Ast_config.DftPattern (s) -> ("pattern = "^s)
    | Ast_config.Data (e) -> ("data = "^ get_expression e)
    | Ast_config.Dir (s) -> ("dir = "^s)
    | Ast_config.SubDir (s) -> ("subdir = "^s)
    | Ast_config.Factor (f) -> ("factor = "^string_of_float f)
    | Ast_config.File (sopt) ->
      (match sopt with
	  None -> ("file = none")
	| Some s -> ("file = \""^s^"\"")
      )
    | Ast_config.Filename b -> ("filename = "^Misc.string_of_bool b)
    | Ast_config.Footer f -> ("footer = \""^f^"\"")
    | Ast_config.Format mode -> ("format = "^mode)
    | Ast_config.Author (b) -> ("author = "^Misc.string_of_bool b)
    | Ast_config.Info b -> ("info = "^Misc.string_of_bool b)
    | Ast_config.DftProject (s) -> ("project = "^s)
    | Ast_config.Legend (s) -> ("legend = \""^s^"\"")
    | Ast_config.XLegend (s) -> ("xlegend = \""^s^"\"")
    | Ast_config.YLegend (s) -> ("ylegend = \""^s^"\"")
    | Ast_config.YLegendFactor (s) -> (" = "^s)
    | Ast_config.LineType (s) -> ("linetype = "^s)
    | Ast_config.MarkType (s) -> ("marktype = "^s)
    | Ast_config.MarkSize (v) -> ("marksize = "^string_of_float v)
    | Ast_config.XMin (v) -> ("xmin = "^string_of_float v)
    | Ast_config.XAxis (s) -> ("xaxis = "^s)
    | Ast_config.YAxis (s) -> ("yaxis = "^s)
    | Ast_config.Ratio b -> ("ratio = "^Misc.string_of_bool b)
    | Ast_config.NotExistColor (r,g,b) -> ("notexistcolor = "^Misc.string_of_rgb (r,g,b))
    | Ast_config.Version (_, vs) ->
      (try
	 LOG "version = {" LEVEL TRACE;
	 let m = get_vers_min vs in
	 List.iter (fun v -> LOG "%s" (show_version m v) LEVEL TRACE) vs;
	 "}"
       with _ -> "No version set !"
      )
    | Ast_config.VMin (s) -> ("vmin = \""^s^"\"")
    | Ast_config.Prune (b) -> ("prune = "^Misc.string_of_bool b)
    | Ast_config.SPFlags (s) -> ("flags = \""^s^"\"")
    | Ast_config.LOCALSCM (s) -> ("scm = \""^s^"\"")
    | Ast_config.Size (x,y) -> ("size = "^string_of_float x ^ "x" ^ string_of_float y)
    | Ast_config.Sort b -> ("sort = "^Misc.string_of_bool b)
    |_ -> ""

let rec extract_patterns acc e =
  match e with
  Ast_config.Pattern (p) -> p::acc
| Ast_config.Project _ -> acc
| Ast_config.Cst     _ -> acc
| Ast_config.Plus  (e1, e2) ->
    (extract_patterns [] e1) @ (extract_patterns acc e2)
| Ast_config.Minus (e1, e2) ->
    (extract_patterns [] e1) @ (extract_patterns acc e2)
| Ast_config.Mul   (e1, e2) ->
    (extract_patterns [] e1) @ (extract_patterns acc e2)
| Ast_config.Div   (e1, e2) ->
    (extract_patterns [] e1) @ (extract_patterns acc e2)

let rec extract_projects acc e =
  match e with
  Ast_config.Pattern _   -> acc
| Ast_config.Project (p) -> p::acc
| Ast_config.Cst     _   -> acc
| Ast_config.Plus  (e1, e2) ->
    (extract_projects [] e1) @ (extract_projects acc e2)
| Ast_config.Minus (e1, e2) ->
    (extract_projects [] e1) @ (extract_projects acc e2)
| Ast_config.Mul   (e1, e2) ->
    (extract_projects [] e1) @ (extract_projects acc e2)
| Ast_config.Div   (e1, e2) ->
    (extract_projects [] e1) @ (extract_projects acc e2)

let get_data_exp atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Data _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Data d -> d
      | _ -> raise Unrecoverable
  with _ ->
    raise (Misconfiguration "curve: user-defined curve must have a \"data\" attribute.")

let get_patterns_of_data atts =
  let d = get_data_exp atts in
    extract_patterns [] d

let get_projects_of_data atts =
  let d = get_data_exp atts in
    extract_projects [] d

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
    let prjs =
      try
	get_projects_of_data atts
      with _ -> raise (Misconfiguration "curve: project is not set.")
    in
      match prjs with
	  []    -> raise (Misconfiguration "curve: project is not set, even in 'data' attribute.")
	| [prj] -> prj
	| _     -> raise (Misconfiguration "curve: multiple projects are set in 'data' attribute.")

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
    let prjs =
      try
	get_projects_of_data atts
      with _ -> raise (Misconfiguration "curve: project is not set.")
    in
      match prjs with
	  []    -> raise (Misconfiguration "curve: project is not set, even in 'data' attribute.")
	| _     -> prjs

let get_project catts project =
  match project with
      Some s -> s
    | None -> get_dft_project catts

let get_prjdir p =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects p) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.Dir _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.Dir d -> "/" ^ d
	  | _ -> raise Unrecoverable
      with _ ->
	""
  with Not_found ->
	raise (Misconfiguration ("project "^p^" is not declared"))

let rev_prjdir pdir =
  let pair = Setup.PrjTbl.fold
    (fun name _ pair ->
       (get_prjdir name, name):: pair
    ) Setup.projects []
  in
    List.assoc pdir pair

let get_subdir p =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects p) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.SubDir _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.SubDir d -> Some d
	  | _ -> raise Unrecoverable
      with _ ->
	None
  with Not_found ->
	raise (Misconfiguration ("project "^p^" is not declared"))

let has_coccifile d =
  try
    let atts = Setup.DftTbl.find Setup.smatchs d in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.File _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.File fopt -> if fopt = None then false else true
	  | _ -> raise Unrecoverable
      with _ ->
	raise (Misconfiguration "pattern cocci file is not set")
  with Not_found ->
	raise (Misconfiguration ("pattern '"^d^"' is not declared"))

let get_coccifile d =
  try
    let atts = Setup.DftTbl.find Setup.smatchs d in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.File _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.File fopt ->
	      (match fopt with
		   None -> raise (Misconfiguration "pattern cocci file is not properly set")
		 | Some f -> f

	      )
	  | _ -> raise Unrecoverable
      with _ ->
	raise (Misconfiguration "pattern cocci file is not set")
  with Not_found ->
	raise (Misconfiguration ("pattern '"^d^"' is not declared"))

let rev_pattfile pfile =
  let pair = Setup.DftTbl.fold
    (fun name _ pair ->
       (get_coccifile name, name):: pair
    ) Setup.smatchs []
  in
    List.assoc pfile pair

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
    LOG "XMin of %s is not defined. Assuming 0." g LEVEL DEBUG;
    0.0

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

let get_dft_pattern prune gatts latts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.DftPattern s -> true
		     | _ -> false
		) gatts
    with
	Ast_config.DftPattern s ->
	  if prune then
	    if has_coccifile s then
	      [s]
	    else
	      []
	  else
	    [s]
      | _ -> raise Unrecoverable
  with _ ->
    match get_ytype "" gatts with
	"size" | "sizepct" | "usersize" -> []
      | "user"    ->
	  get_patterns_of_data latts
      | _      -> raise (Misconfiguration "curve: pattern is not set")

let get_pattern prune gatts pattern catts =
  match pattern with
      Some d ->
	if prune then
	  if has_coccifile d then
	    [d]
	  else
	    []
	else
	  [d]
    | None -> get_dft_pattern prune gatts catts

let get_correl_mode verbose pattern =
  try
    let atts = Setup.DftTbl.find Setup.smatchs pattern in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.Correl _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.Correl s ->
	      LOG "Pattern %s has correlation mode: %s" pattern s LEVEL TRACE;
	      (match s with
		   "strict"  -> Ast_config.Strict
		 | "none"    -> Ast_config.Nocorrel
		 | "default" -> Ast_config.Default
		 | _         ->
		     raise (Misconfiguration ("Unknown correlation type in pattern "^pattern))
	      )
	  | _ -> raise Unrecoverable
      with _ ->
	LOG "Pattern %s has no correlation mode set: default assumed" pattern LEVEL TRACE;
	Ast_config.Default
  with Not_found ->
    raise (Misconfiguration ("pattern '"^pattern^"' is not declared"))

let get_format pattern =
  try
    let atts = Setup.DftTbl.find Setup.smatchs pattern in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.Format _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.Format s ->
	      begin
		match s with
		    "csv" | "Csv" | "CSV" -> Ast_config.CSV
		  | "org" | "Org" | "ORG" -> Ast_config.Org
		  | _     ->
		      raise (Misconfiguration ("Unknonw format "^s ^" used for pattern "^pattern))
	      end
	  | _ -> raise Unrecoverable
      with _ -> Ast_config.Org
  with Not_found ->
    raise (Misconfiguration ("pattern '"^pattern^"' is not declared"))

let get_flags atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.SPFlags _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.SPFlags s -> " " ^ s
      | _ -> ""
  with _ -> ""

let get_scm prj =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects prj) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.LOCALSCM _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.LOCALSCM s -> s
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

let get_prjflags p =
  let (_, atts) = Setup.PrjTbl.find Setup.projects p in
    get_flags atts

let get_pattflags d =
  let atts = Setup.DftTbl.find Setup.smatchs d in
    get_flags atts

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
	  | _ -> raise Unrecoverable
      with _ -> (0, [])
  with Not_found ->
    raise (Misconfiguration ("project "^p^" is not declared"))

let get_versionsRE p =
  try
    let atts = snd (Setup.PrjTbl.find Setup.projects p) in
      try
	match
	  List.find (fun x ->
		       match x with
			   Ast_config.VersionRE _ -> true
			 | _ -> false
		    ) atts
	with
	    Ast_config.VersionRE re -> re
	  | _ -> raise Unrecoverable
      with _ -> "" (* No RE defined *)
  with Not_found ->
    raise (Misconfiguration ("project "^p^" is not declared"))

let rec spatch_cli_per_cpu cpu cpumax outsetcmd flags out =
  if cpu < cpumax then
    let index = string_of_int cpu in
    let idxmax = string_of_int cpumax in
    let suffix = "." ^ index ^"-"^ idxmax in
    let cpuorg = out ^ suffix ^ Global.origext in
    let cpuout = out ^ suffix in
    let cpuflags = flags ^ " -index " ^ index ^ " -max " ^ idxmax in
    let basesetcmd = Str.global_replace (Str.regexp_string "%b") cpuout outsetcmd in
    let flagsetcmd = Str.global_replace (Str.regexp_string "%f") cpuflags basesetcmd  in
    let cmd = Str.global_replace (Str.regexp_string "%o") cpuorg flagsetcmd in
      (cpuorg, cmd)::(spatch_cli_per_cpu (cpu+1) cpumax outsetcmd flags out)
  else
    []

let spatch_cli p d =
  let prjdir = get_prjdir p in
  let coccifile = get_coccifile d in
  let cocci = !Setup.smatchdir ^"/" ^ coccifile in
  let project = !Setup.projectsdir ^ prjdir in
  let cpucore = !Setup.findchild in
  let lprjflags = get_prjflags p in
  let lpattflags = get_pattflags d in
  let (depth, versions) = get_versions p in
  let vmin = get_vmin_of d in
  let prune = get_prune_of d in
  let init_st = if vmin = None then true else not prune in
  let flags = !Setup.spflags ^ lprjflags ^ lpattflags in
  let findcmd = Str.global_replace (Str.regexp_string "%p") cocci !Setup.findcmd in
  let cpucmd =
    if cpucore = None then
      (*
	There is not multi-threaded call, and thus no need to compute -index option.
	Flags can be substitute here for efficiency.
      *)
      Str.global_replace (Str.regexp_string "%f") flags findcmd
    else
      findcmd
  in
  let outbase = !Setup.resultsdir ^ prjdir in
(*  let outfile = Filename.chop_extension coccifile in *)
  let outfile = p ^ Global.sep ^ d in
  let (_, res) = List.fold_left
    (fun (st, head) v ->
       if vmin = Some v || st then
	 let prj = match get_subdir p with
	     None     -> project ^ "/" ^ v
	   | Some dir -> project ^ "/" ^ v ^ "/" ^ dir
	 in
	 let out = outbase ^ "/" ^ v ^ "/" ^ outfile in
	 let org = out ^ Global.origext in
	 let vsetcmd = Str.global_replace (Str.regexp_string "%d") prj cpucmd in
	   match cpucore with
	       None ->
		 (* Flags has already been substituted outside of fold_left *)
		 let basesetcmd = Str.global_replace (Str.regexp_string "%b") out vsetcmd in
		 let outsetcmd = Str.global_replace (Str.regexp_string "%o") org basesetcmd in
		   (true, (org, (outsetcmd,[]))::head)
	     | Some cpucore ->
		 (* Flags will be substituted by spatch_cli_per_cpu *)
		 begin
		   let subcmds = spatch_cli_per_cpu 0 cpucore vsetcmd flags out in
		   let catcmd = "cat $^ > $@\n\t-cat $(^:%.orig.org=%.log) > $(@:%.orig.org=%.log)" in
		     (true, (org, (catcmd, subcmds))::head)
		 end
       else
	 (st, head) (* Skip versions until vmin is reached. *)
    ) (init_st, []) versions
  in List.rev res

let locate_data p d datatype =
(*  let cocci = get_coccifile d in
  let data = (Filename.chop_extension cocci) in *)
    !Setup.resultsdir ^ get_prjdir p ^ "/" ^ p ^ Global.sep ^ d ^ datatype

let get_projects_of gatts curves =
  Misc.unique_list (
    List.flatten
      (List.map
	 (fun (project, _,atts,_) ->
	    try
	      [get_project (atts@gatts) project]
	    with _ ->
	      get_projects (atts@gatts)
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
       let updinfos = List.map (update_days mindate) (List.flatten vinfos) in
       let sortedlist = List.sort sort_by_date updinfos in
       let sorted = Array.of_list sortedlist in
	 sorted
    )

let get_bugset_of_curve gatts curve : string list =
  let (project, pattern, atts, pos) = curve in
    try
      (* FIXME Give priority of atts over gatts *)
      let p = get_project (atts@gatts) project in
      let d_list = get_pattern false gatts pattern atts in
	List.map (fun d ->
		    locate_data p d Global.bugext)
	  d_list
    with
	Misconfiguration s ->
	  Misc.report_error pos s

let get_bugset_of_grp_patt patt pos prj : string =
  try
    locate_data prj patt Global.bugext
  with
      Misconfiguration s ->
	Misc.report_error pos s

let get_cmdlist gatts curve =
  let (project, pattern, atts, pos) = curve in
    try
      (* FIXME Give priority of atts over gatts *)
      let p = get_project (atts@gatts) project in
      let d_list = get_pattern true gatts pattern atts in
	List.map (fun d ->
		    let data = locate_data p d Global.origext in
		    let cli = spatch_cli p d in
		      ((p, d), (data, cli))
		 )
	  d_list
    with
	Misconfiguration s ->
	  Misc.report_error pos s

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
	    let prj_count = List.length (List.filter (fun p -> match p with Ast_config.DftProject _ -> true | _ -> false) gatts) in
	    let pat_count = List.length (List.filter (fun p -> match p with Ast_config.DftPattern _ -> true | _ -> false) gatts) in
	    (match (prj_count, pat_count) with
		(0, 0) ->
		  raise (Misconfigurationat ("In graph "^g^", color is not defined for the curve with "^p^" and "^d^".", pos))
	      | (0, _) ->
		let dftatts = Setup.DftTbl.find Setup.smatchs d in
		get_rgb_in_atts dftatts
	      | (_, 0) ->
		let dftatts = snd (Setup.PrjTbl.find Setup.projects p) in
		get_rgb_in_atts dftatts
	      | (_,_) ->
		raise (Misconfigurationat ("In graph "^g^", don't know which color do use for the curve with "^p^" and "^d^".", pos))
	    )
	  | None, None ->  (* Prj and Defect are both not set ! Pick dft *)
	      raise (Misconfigurationat ("In graph "^g^", color is not defined.",pos))
      with Not_Found ->
	raise (Misconfigurationat ("In graph "^g^", color is not defined.",pos))


(***********************************************
  Author: Lotfi <lotfi.manseur@imag.fr>
  Comment: Added for automated size computation
           and to compute commands used in
	   generated Makefiles.
*)
let rec get_origs cli=match cli with
                      []->[]
                     |cl::q->let (org,reste)=cl in org::(get_origs q)

(* get a command list from an experience*)
let rec get_cmdList p pattern_list: ((string * string) *(string * (string * (string * (string * string) list)) list))list=
  match pattern_list with
      []->[]
    |patt::tail->let cli=spatch_cli p patt in 
                 let _ = get_origs cli in
                 let data = locate_data p patt Global.origext in
                 ((p,patt),(data,cli))::(get_cmdList p tail)

(**********************************************)

(*
  Helper functions to pretty print
*)
let show_curve atts curve =
  Debug.profile_code "Config.show_curve"
    (fun () ->
       let (project, pattern, catts, pos) = curve in
	 try
	   let d_list = get_pattern false atts pattern catts in
	     if d_list = [] then
	       LOG "No need to compute data on bugs for this curve." LEVEL TRACE
	     else
	       begin
		 let p = get_project (catts@atts) project in
		   List.iter (fun d ->
		     LOG "Curve with p:%s and d:%s " p d LEVEL TRACE
			     ) d_list;
		   List.iter (fun d ->
				let data = locate_data p d Global.origext in
				LOG "Reading data from: %s" data LEVEL TRACE
			     ) d_list
	       end;
	     List.iter (fun x ->
			  LOG "\t%s" (show_attr x) LEVEL TRACE
		       )
	       catts;
	 with
	     Misconfiguration s ->
	       Misc.report_error pos s
    )

let show_global () =
  LOG "Prefix = %s" !Setup.prefix LEVEL DEBUG;
  LOG "Cocci files in %s" !Setup.smatchdir LEVEL DEBUG;
  LOG "Project source code in %s" !Setup.projectsdir LEVEL DEBUG;
  LOG "Analysis results in %s" !Setup.resultsdir LEVEL DEBUG;
  LOG "FindCmd = \"%s\"" !Setup.findcmd LEVEL DEBUG;
  LOG "Flags = \"%s\"" !Setup.spflags LEVEL DEBUG;
  match !Setup.cpucore with
      None ->
	LOG "CPU core = <undefined: assuming %d>" (Misc.get_number_of_cores ()) LEVEL DEBUG
    | Some cpucore ->
	LOG "CPU core = %d" cpucore LEVEL DEBUG

let show_config () =
  Debug.profile_code "Config.show_config"
    (fun () ->
       show_global ();
       Setup.PrjTbl.iter
	 (fun name (_,atts) ->
	   LOG "Project %s" name LEVEL DEBUG;
	   List.iter (fun a -> LOG "%s" (show_attr a) LEVEL TRACE) atts
	 )
	 Setup.projects;
       Setup.DftTbl.iter
	 (fun name atts ->
	   LOG "Pattern %s" name LEVEL DEBUG;
	   List.iter (fun a -> LOG "%s" (show_attr a) LEVEL TRACE) atts
	 )
	 Setup.smatchs;
       Setup.GphTbl.iter
	 (fun name (atts, subgraph) ->
	   LOG "Graph %s" name LEVEL DEBUG;
	   List.iter (fun a -> LOG "%s" (show_attr a) LEVEL TRACE) atts;
	   match subgraph with
	       Ast_config.Curves curves ->
		 List.iter (show_curve atts) curves
	     | Ast_config.Groups _ -> ()
	 )
	 Setup.graphs;
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
		 LOG "%s Automatically picking one..." msg LEVEL ERROR;
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
