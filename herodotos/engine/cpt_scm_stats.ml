let sys_command cmd =
  LOG "Execute: '%s'" cmd LEVEL INFO;
  let status = Sys.command cmd in
  LOG "Status: %d" status LEVEL INFO;
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
    Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines) in_channel)
  with
    End_of_file->lines
 
let get_size dir =
  let cmd = "sloccount "^dir in
  LOG "Execute: '%s'" cmd LEVEL DEBUG;
  let in_channel = Unix.open_process_in cmd in
  let lines = read_recursive [] in_channel in
  let chaine = String.concat "\n" lines in       
  let expression =  Str.regexp "ansic: *[0-9]+" in
  let expNumber = Str.regexp"[0-9]+" in 
  let expSep = Str.regexp" +" in
  let _ = Str.search_forward expression chaine 0 in
  let size = int_of_string (string_match_exp expNumber (Str.split expSep (string_match_exp expression lines))) in
  close_in in_channel ;
  LOG "Size: %d" size LEVEL DEBUG;
  size

(* extracts versions information thanks to a regexp describing versions tags *)
let extract_vers_infos prj expression declared_versions =
  let path = !Setup.projectsdir ^ (Config.get_prjdir prj) in
  let local_scm = Config.get_scm prj in
  let origin =
    try
      Config.get_public_scm prj
    with e ->
      LOG "%s" (Printexc.to_string e) LEVEL ERROR;
      ""
  in
  LOG "Local SCM: %s" local_scm LEVEL DEBUG;
  let deposit = Str.replace_first (Str.regexp "git:") "" local_scm in
  let scm = path^"/"^deposit in
  LOG "Checking local SCM: %s" scm LEVEL DEBUG;
  if not ((Sys.file_exists scm)
	  &&(Sys.is_directory scm)) then
    if origin <> "" then
      ignore(sys_command ("git clone "^origin^" "^path^"/"^deposit ))
    else
      (LOG "No public SCM defined, but %s is not available" deposit LEVEL FATAL;
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
    with _ ->
      let date = Git.get_version_date path version deposit in
      let size = get_size (path^"/"^version) in
      (version, Some date, size)
  ) tag_list 
