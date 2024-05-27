let sys_command cmd =
  let logmsg=Printf.sprintf "Execute: '%s'" cmd in
  [%info_log logmsg];
  let status = Sys.command cmd in
  let logmsg2=Printf.sprintf "Status: %d" status in
  [%info_log logmsg2];
  status

let tag_filter tag_list filter =
  let result = ref [] in 
  List.iter(fun tag -> if Str.string_match (Str.regexp filter) tag 0 then
      result := tag :: !result
    else 
      ())tag_list ; 
  !result

let rec string_match_exp (exp:Str.regexp) (strings:string list) =
  match strings with
      [] -> ""
    | s1::tail ->
      try 
        let _ = Str.search_forward exp s1 0 in
        let posbeg=Str.match_beginning() in
        (String.sub s1 posbeg ((String.length s1)-posbeg))
      with Not_found -> (string_match_exp exp tail) 

let rec read_recursive lines in_channel=
  try
    Scanf.bscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines) in_channel)
  with
    End_of_file->lines
 
let get_size dir =
  let logmsg=Printf.sprintf "Retrieving C code size of %s" dir in
  [%info_log logmsg];
  let cmd = "sloccount "^dir in
  let logmsg=Printf.sprintf "Execute: '%s'" cmd in
  [%debug_log logmsg];
  let in_channel = Unix.open_process_in cmd in
  let lines = read_recursive [] (Scanf.Scanning.from_channel in_channel) in
  let chaine = String.concat "\n" lines in       
  let expression =  Str.regexp "ansic: *[0-9]+" in
  let expNumber = Str.regexp"[0-9]+" in 
  let expSep = Str.regexp" +" in
  let size =
    try
      let _ = Str.search_forward expression chaine 0 in
      int_of_string (string_match_exp expNumber (Str.split expSep (string_match_exp expression lines)))
    with Not_found ->
      [%error_log "Error while retrieving the ansi C code size"];
      0
  in
  close_in in_channel ;
  let logmsg=Printf.sprintf "Size: %d" size in
  [%debug_log logmsg];
  size

(* extracts versions information thanks to a regexp describing versions tags *)
let extract_vers_infos prj expression declared_versions withsizes =
  let path = !Setup.projectsdir ^ (Config.get_prjdir prj) in
  let local_scm = Config.get_scm prj in
  let origin =
    try
      Config.get_public_scm prj
    with e ->
      (let logmsg=Printf.sprintf "%s" (Printexc.to_string e) in
      [%error_log logmsg]);
      ""
  in
  let logmsg=Printf.sprintf "Local SCM: %s" local_scm in
  [%debug_log logmsg];
  let deposit = Str.replace_first (Str.regexp "git:") "" local_scm in
  let scm = path^"/"^deposit in
  let logmsg=Printf.sprintf "Checking local SCM: %s" scm in
  [%debug_log logmsg];
  if not ((Sys.file_exists scm)
	  &&(Sys.is_directory scm)) then
    if origin <> "" then
      ignore(sys_command ("git clone "^origin^" "^path^"/"^deposit ))
    else
      (let logmsg=Printf.sprintf "No public SCM defined, but %s is not available" deposit in
        [%fatal_log logmsg];
       failwith "A public SCM is needed."
      );
  let tag_list = Git.get_tags scm expression in
  List.iter (
    fun version ->
      if not ((Sys.file_exists (path^"/"^version))
	      &&(Sys.is_directory(path^"/"^version))) then
        ignore(sys_command
		 ("git --git-dir "^path^"/"^deposit ^" archive --format=tar --prefix="^version^"/ "^ version
		  ^" | (cd "^path^" && tar xf -)"))
  ) tag_list;
  List.map (fun version ->
    try
      List.find (fun (name, date, size) -> name = version) declared_versions
    with Not_found ->
      let date = Git.get_version_date path version deposit in
      let size = if withsizes == true then get_size (path^"/"^version)
                                      else -1
      in
      (version, Some date, size)
  ) tag_list 
