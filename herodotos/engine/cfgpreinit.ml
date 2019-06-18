
let build_updated_cache cache_projects withsizes =
  Setup.PrjTbl.fold 
    (fun prj _ cache ->
      LOG "Processing %s for cache" prj LEVEL INFO;
      (* Check for a regular expression *)
      let re = Config.get_versionsRE prj in
      try
	let cacheddata = List.assoc prj cache_projects in
	LOG "Data found in the cache for %s" prj LEVEL INFO;
	if re <> "" then
	  let infos = Cpt_scm_stats.extract_vers_infos prj re cacheddata withsizes in
	  (prj, infos)::cache
	else
	  (prj, cacheddata)::cache
      with Not_found ->
	LOG "No data in cache for %s" prj LEVEL INFO;
	let versinfos =
	  try
	    List.map (fun (name, days, date, size) -> (name, date, size))
	      (Array.to_list (snd (Config.get_versinfos prj)))
	  with _ ->
	    LOG "No version information manually provided." LEVEL DEBUG;
	    []
	in
	if re <> "" then
	  let infos = Cpt_scm_stats.extract_vers_infos prj re versinfos withsizes in
	  (prj, infos)::cache
	else
	  if versinfos = [] then
	    begin
	      LOG "No data source provided." LEVEL FATAL;
	      failwith "No data source provided."
	    end
	  else
	    begin
	      LOG "Use only user provided data." LEVEL INFO;
	      (prj, versinfos)::cache
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

let preinit v1 v2 v3 configfile withsizes =
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
  let new_cache = build_updated_cache cache withsizes in
  let out_channel = open_out cache_file in
  List.iter (print_cache out_channel) new_cache;
  close_out out_channel
