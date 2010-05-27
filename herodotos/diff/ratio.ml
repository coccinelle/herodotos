exception MalformedExists

let compute_delta delta hunk =
  let (dm, dp) = delta in
  let ((_,lm),(_,lp)) = hunk in
    (dm+lm, dp+lp)

let compute_diff diffs keys =
   List.map (fun (k: string * string) ->
		     try
		       let hunks = List.assoc k diffs in
			 List.fold_left compute_delta (0,0) hunks
		     with Not_found -> (0,0)
	    ) keys

let rec compute_key vers file =
    match vers with
	[] -> []
      | hd::tail -> (hd, file)::compute_key tail file

let compute_ratio delta state =
  let (dm, dp) = delta in
    match state with
	Ast_exist.False  -> 0.0
      | Ast_exist.Size s ->
	  let linechange = float_of_int (dm+dp) in
	  let filechange = float_of_int (dp-dm) in
	  let filesize = float_of_int s in
	  let fileweight = (2.0 *. filesize) +. filechange in
	    linechange /. fileweight
      | Ast_exist.True   -> raise MalformedExists

let compute_deltas diffs exists =
  let (vers, entries) = exists in
  let files_states = List.map (fun (file, states)-> (file,states)) entries in
  let keys = List.map (fun (f,s) -> (f, s, compute_key vers f)) files_states in
    List.map (fun (f, states, vers) ->
		let deltas = compute_diff diffs vers in
		(f, List.map2 compute_ratio deltas states)) keys

let print diffs exists =
  let deltas = compute_deltas diffs exists in
    print_endline "Ratio";
    List.iter (fun (f, ratios) ->
		 Printf.printf "%s " f;
		 List.iter (fun r -> Printf.printf "%0.4f " r) ratios;
		 Printf.printf "\n"
	      ) deltas
