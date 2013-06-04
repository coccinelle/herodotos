
(*could find better *)
let update_path ()= Sys.command "export PATH=/usr/bin/sloccount:$PATH" 

let exec_count dir = Sys.command ("sloccount "^dir^" > count.tmp")

let clean nameFile =Sys.command ("rm "^nameFile)

let rec string_match_exp (exp:Str.regexp) (strings:string list)= match strings with
                                        []->""
                                       |s1::tail -> try 
                                                      let correspond = Str.search_forward exp s1 0 in
                                                      let posbeg=Str.match_beginning() in
                                                      (String.sub s1 posbeg ((String.length s1)-posbeg))
                                                    with Not_found->(string_match_exp exp tail) 
		 


let rec read_recursive lines in_channel=
  try
    Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines) in_channel)
  with
    End_of_file->lines
  


let get_size dir = let maj=update_path in
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


