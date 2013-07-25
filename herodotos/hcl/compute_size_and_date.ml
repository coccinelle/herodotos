exception Not_Declared of string

let update_path ()= Sys.command "export PATH=/usr/bin/sloccount:$PATH" 

let exec_count dir = Sys.command ("sloccount "^dir^" > count.tmp")

let clean nameFile =Sys.command ("rm "^nameFile)


let get_already_declared version_list version = List.find(fun v -> let (name,date,size) = v in name = version) version_list


let get_tags path deposit = let tag_list = Sys.command("cd "^(path^"/"^deposit)^";"^"git tag > .tags.tmp") in
                                           let in_channel = open_in ".tags.tmp" in 
                                           let tag_string = String.create (in_channel_length in_channel) in
                                           let buff =  really_input in_channel tag_string 0 (in_channel_length in_channel) in
                                           Str.split (Str.regexp "\n") tag_string

let tag_filter tag_list filter = let result = ref [] in 
                                 let loop = List.iter(fun tag -> if Str.string_match (Str.regexp filter) tag 0 then
                                                                   result := tag :: !result
                                                                 else 
                                                                   ())tag_list in !result



let rec string_match_exp (exp:Str.regexp) (strings:string list)= match strings with
                                        []->""
                                       |s1::tail -> try 
                                                      let correspond = Str.search_forward exp s1 0 in
                                                      let posbeg=Str.match_beginning() in
                                                      (String.sub s1 posbeg ((String.length s1)-posbeg))
                                                    with Not_found->(string_match_exp exp tail) 
		 


let version_from_tag tag = "linux-"^(String.sub tag 1 ((String.length tag) -1))

let rec read_recursive lines in_channel=
  try
    Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines) in_channel)
  with
    End_of_file->lines
 


let get_tag version = let num = Str.regexp "[0-9].*" in
                      let start_num = Str.search_forward num version 0 in 
                      "v"^(String.sub version start_num ((String.length version)- start_num)) 

let get_size dir = 
                   let count=exec_count dir in                 
		   let in_channel=open_in "count.tmp" in
 		   let lines=read_recursive [] in_channel in
		   let chaine = String.concat "\n" lines in       
		   let expression=  Str.regexp "ansic: *[0-9]+" in
                   let expNumber= Str.regexp"[0-9]+" in 
		   let expSep = Str.regexp" +" in
		   let correspond=Str.search_forward expression chaine 0 in
                   let size = int_of_string (string_match_exp expNumber (Str.split expSep (string_match_exp expression lines))) in
		   let close = close_in in_channel in
                   let clean=clean "count.tmp" in
                   size



let get_date path version deposit =
                               try 
                                 let current_dir = Sys.command "pwd > .pwd.tmp" in
                                 let in_channel = open_in ".pwd.tmp" in
                                 let pwd = input_line in_channel in 
                                 let tag = get_tag version in
                                 let depl = Sys.command ("cd "^(path^"/"^deposit)^" ; 
                                                        git log --pretty=raw --format=\"%ci\"  "^tag ^" -1 | cut -f1 -d' ' >            "^pwd^"/.date.tmp") in
                                 let in_ch_date = open_in ".date.tmp" in 
                                 let date = input_line in_ch_date in 
                                 let list_comp = Str.split (Str.regexp "-") date in
                                 let close = close_in in_channel;close_in in_ch_date in
                                 let clean = clean ".pwd.tmp";clean ".date.tmp" in
                                 (List.hd(List.tl list_comp))^"/"^(List.hd (List.tl(List.tl list_comp)))^"/"^(List.hd list_comp)
                               with _->raise (Not_Declared "Error in deposit declaration")  


let extract_code path version deposit = if((Sys.file_exists (path^"/"^version))&&(Sys.is_directory(path^"/"^version))) then  
                                          0 
                                        else 
                                          (*let tag = get_tag version in*) 
                                          Sys.command ("cd "^(path^"/"^deposit)^" && 
                                                git archive --format=tar --prefix="^version^"/ "^
                                                version^" > ../"^version^".tar; cd .. && tar xf "^version^".tar;rm "^version^".tar")

(* extracts versions information thanks to a regexp describing versions tags *)
let extract_vers_infos path expression deposit declared_versions = 
                                let current_dir = Sys.getcwd() in
                                let tags = Sys.command ("cd "^path^"/"^deposit^"&&
                                                 git tag | grep \""^expression   ^"\" > "^current_dir^"/.tags.tmp") in
                                let in_channel = open_in (current_dir^"/.tags.tmp") in
                                let tag_string = String.create (in_channel_length in_channel) in
                                let buff =  really_input in_channel tag_string 0 (in_channel_length in_channel) in
                                let close = close_in in_channel in
                                let clean = clean (current_dir^"/.tags.tmp") in
                                let tag_list = Str.split (Str.regexp "\n") tag_string  in
                                let code_recup = List.map(fun tag->let version = (*version_from_tag*) tag in
                                      if((Sys.file_exists (path^"/"^version))&&(Sys.is_directory(path^"/"^version))) then
                                          0
                                      else           
                                           Sys.command ("cd "^(path^"/"^deposit)^" && 
                                           git archive --format=tar --prefix="^version^"/ "^
                                           tag^" > ../"^version^".tar; cd .. && tar xf "^version^".tar;rm "^version^".tar") ) tag_list in 
                                List.map(fun tag -> let version = (*version_from_tag*) tag in 
                                                    try
                                                      let v = get_already_declared declared_versions version in
                                                      let (n,d,s) = v in
                                                      "(\""^n^"\","^d^","^s^")"
                                                    with _ ->
                                                    let date = get_date path version deposit in
                                                    let size = get_size (path^"/"^version) in
                                                    "(\""^version^"\","^date^","^(string_of_int size)^")") tag_list 



