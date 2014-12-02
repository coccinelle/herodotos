let sys_command cmd =
  LOG "Execute: '%s'" cmd LEVEL INFO;
  0

let exec_count dir = sys_command ("sloccount "^dir^" > count.tmp")

let get_already_declared version_list version =
  List.find (
    fun v ->
      let (name,date,size) = v in
      name = version
  ) version_list

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
  let _ = exec_count dir in                 
  let in_channel=open_in "count.tmp" in
  let lines=read_recursive [] in_channel in
  let chaine = String.concat "\n" lines in       
  let expression=  Str.regexp "ansic: *[0-9]+" in
  let expNumber= Str.regexp"[0-9]+" in 
  let expSep = Str.regexp" +" in
  let _ = Str.search_forward expression chaine 0 in
  let size = int_of_string (string_match_exp expNumber (Str.split expSep (string_match_exp expression lines))) in
  close_in in_channel ;
  Sys.remove "count.tmp";
  size

(* extracts versions information thanks to a regexp describing versions tags *)
let extract_vers_infos path expression local_scm declared_versions origin =
  let deposit = Str.replace_first (Str.regexp "git:") "" local_scm in
  if not ((Sys.file_exists (path^"/"^deposit))
	  &&(Sys.is_directory(path^"/"^deposit))) then
    ignore(sys_command ("cd "^path^";git clone "^origin^" "^deposit ));
  let tag_list = Git.get_tags path deposit expression in
  List.iter (
    fun tag -> 
      let version = tag in
      if not ((Sys.file_exists (path^"/"^version))
	      &&(Sys.is_directory(path^"/"^version))) then
        ignore(sys_command
		 ("cd "^(path^"/"^deposit)^
		     " && git archive --format=tar --prefix="^version^"/ "^ tag 
		  ^" | (cd .. && tar xf -)"))
  ) tag_list;
  List.map(fun tag ->
    let version = tag in 
    try
      let v = get_already_declared declared_versions version in
      let (n,d,s) = v in
      "(\""^n^"\","^d^","^s^")"
    with _ ->
      let date = Git.get_date path version deposit in
      let size = get_size (path^"/"^version) in
      "(\""^version^"\","^date^","^(string_of_int size)^")"
  ) tag_list 
