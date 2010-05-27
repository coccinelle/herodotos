
let get_data_of conn = function
    (_, _, atts, _) ->
      try
	let query = List.fold_left
	  (fun q att ->
	     match att with
		 Ast_config.SQLData s -> s
	       | _ -> q
	  ) "" atts
	in
	  if query <> "" then
	    begin
	      prerr_endline ("Query: "^query);
	      Database.query conn query
	    end
      with _ -> ()

let test conn =
  Setup.GphTbl.iter
    (fun name (atts, subgraph) ->
       prerr_endline ("Graph "^name);
       match subgraph with
	   Ast_config.Curves curves ->
	     List.iter (get_data_of conn) curves
	 | Ast_config.Groups _ -> ()
    )
    Setup.graphs

let run v1 v2 v3 config pdf png web freearg =
  Config.parse_config config;
  Config.show_config v1 v2;
  prerr_endline ("Connecting to "^ !Setup.dbconn);
  try
    let conn = Database.open_db v1 !Setup.dbconn in
      prerr_endline "Connection - OK !";
      prerr_endline "Testing...";
      test conn;
      prerr_endline "Disconnecting...";
      Database.close_db conn;
      prerr_endline "Done."
  with Postgresql.Error (e) ->
    prerr_endline "Connection - KO !";
    match e with
      | Postgresql.Field_out_of_range (_, _) 
      | Postgresql.Tuple_out_of_range (_, _)
      | Postgresql.Binary
	->
	  prerr_endline "Unexpected failure"
      | Postgresql.Connection_failure (s)
      | Postgresql.Unexpected_status ( _, s, _)
      | Postgresql.Cancel_failure (s)
	->
	  prerr_endline s


    

