
(*------------------------------------------------------------------------------------------------------------------------------------------------
(* preinit parsing rules*)
projectPreinit:
  TPROJECT name=TId TLCB versions=list(prjattr) TRCB {versions_string:= !versions_string^name^(String.concat "\n" versions)}

(*currently used for preinit parsing *)
prjattr:
  | TDIR           TEQUAL d=path                     {Setup.setDir d ;""}
  | TSUBDIR        TEQUAL path                       {"" }
  | TVERSIONS      TEQUAL TLCB  vs=list(versionPreinit) TRCB {"{\n"^(String.concat "\n" vs)^"\n}\n"}
  | TVERSIONS TEQUAL exp=TSTRING
      {let vs = Compute_size_and_date.extract_vers_infos (!Setup.projectsdir^"/"^(!Setup.dir)) exp !local_scm !already_declared_versions !public_scm in
        "{\n"^(String.concat "\n" vs)^"\n}\n" }
  | TLOCALSCM      TEQUAL v=TSTRING                  {local_scm := v;"" }
  | TPUBLICSCM     TEQUAL v=TSTRING                  {public_scm := v;"" }

versionPreinit:
  TLPAR name=TSTRING  d=datePreinit  size=sizePreinit TRPAR {
    let _ =Compute_size_and_date.extract_code (!Setup.projectsdir^"/"^(!Setup.dir)) name (!local_scm) (!public_scm) in
    let date = if d="" then 
                         (Compute_size_and_date.get_date (!Setup.projectsdir^"/"^(!Setup.dir))  name (!local_scm))
                      else d in
    let _ =List.length (Str.split (Str.regexp_string (Str.quote "/")) name) in if size=0 then      
      
      let size=Compute_size_and_date.get_size (!Setup.projectsdir^"/"^(!Setup.dir)^"/"^name) in 
      "("^"\""^name^"\""^","^ date^","^(string_of_int size)^")"
     else 
        "("^"\""^name^"\""^","^ date ^","^(string_of_int size)^")"
  }

versionPreinit:
  TLPAR name=TSTRING TCOMMA d=datePreinit  size=suitePreinit TRPAR {
    (* FIXME: Should not be in parser !!! *)
    ignore(Compute_size_and_date.extract_code (!Setup.projectsdir^"/"^(!Setup.dir)) name (!repository_git) (!repository_git));
    let date =
      if d = "" then 
        (Compute_size_and_date.get_date (!Setup.projectsdir^"/"^(!Setup.dir))  name (!repository_git))
      else d
    in
    let corrected_size =
      if size = 0 then    
	Compute_size_and_date.get_size (!Setup.projectsdir^"/"^(!Setup.dir)^"/"^name)
      else
	size
    in
    "("^"\""^name^"\""^","^ date^","^(string_of_int corrected_size)^")"
  }


*)

let build_updated_cache cache_projects =
  Setup.PrjTbl.fold 
    (fun key data cache ->
      LOG "Processing %s for cache" key LEVEL INFO;
      try
	let cacheddata = List.assoc key cache_projects in
	LOG "Data found in the cache" LEVEL INFO;
	(key, cacheddata)::cache
      with Not_found ->
	let versinfos = Array.to_list (snd (Config.get_versinfos key)) in
	if versinfos = [] then
	  begin
	    (* There is no info. TODO: Need to check for a RE *)
	    LOG "No data in cache and no data provided. Use regexp and compute" LEVEL INFO;
	    cache
	  end
	else
	  begin
	    let data = List.map (fun (name, days, date, size) -> (name, date, size)) versinfos in
	    LOG "No data in cache. Use provided data" LEVEL INFO;
	    (key, data)::cache
	  end
    )
    Setup.projects []

let  print_cache out_channel prj_cache =
  let (prj, vl) = prj_cache in
  Printf.fprintf out_channel "%s {\n" prj;
  List.iter (fun (name, date_opt, size) ->
    match date_opt with
	Some date ->
	  Printf.fprintf out_channel "(\"%s\", %s, %d)\n" name (Misc.string_of_date date) size
      | None -> failwith ("No date for " ^ prj)
  ) vl;
  Printf.fprintf out_channel "}\n"

let preinit v1 v2 v3 configfile =
  ignore(Config.parse_config_no_cache configfile);
  LOG "Config parsing OK!" LEVEL INFO;
  (*  Config.show_config ();*)
  let cache_file = ".projects_"^configfile in
  let cache =
    if Sys.file_exists cache_file then
      Config.parse_cache cache_file
    else
      []
  in
  let new_cache = build_updated_cache cache in
  let out_channel = open_out (".projects_"^configfile) in
  List.iter (print_cache out_channel) new_cache;
  close_out out_channel

(*
  prerr_string ("Cache file "^versions_file^" does not exist or has been modified, run make preinit.\n")   
*)
