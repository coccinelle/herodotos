exception Misconfiguration of string

let erase_file v1 v2 prjs patts =
  let targetlist =
    List.flatten (List.map
      (fun (p, cmds) ->
	 let datalist = Cfginit.erase_file_patt v2 cmds in
	 let (data, targets) = List.split datalist in
	 let flat_targets = List.flatten targets in
	   Printf.printf "%s:\n\t%s" p (String.concat "\n\t" data);
	   Printf.printf "\n\t%s\n" (String.concat "\n\t" flat_targets);
	   datalist
      ) prjs)
  in
  List.iter (fun (patt, data) ->
	       let datalist = String.concat "\n\t" data in
		 Printf.printf "%s:\n\t%s" patt datalist;
		 List.iter (fun d ->
			      let targets = List.assoc d targetlist in
				Printf.printf "\n\t%s" (String.concat "\n\t" targets)
			   ) data;
		 Printf.printf "\n\n"
	    ) patts

let erase_env v1 v2 v3 configfile filter =
  ignore(Config.parse_config configfile);
  LOG "Config parsing OK!" LEVEL INFO;
  Config.show_config ();
  let (prjs, patts) = Cfginit.compute_makefile () in
    erase_file v1 v2 prjs patts;
