exception Not_Found
exception Unrecoverable
exception Misconfiguration of string

let get_xtype g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.XAxis _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.XAxis t -> t
      | _ -> raise Unrecoverable
  with _ ->
    raise (Misconfiguration ("XAxis type of "^g^" is not defined"))

let get_ytype g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YAxis _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YAxis t -> t
      | _ -> raise Unrecoverable
  with _ ->
    raise (Misconfiguration ("YAxis type of "^g^" is not defined"))

let rec is_with_fn verbose g topatts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Filename _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Filename v -> v
      | _ -> raise Unrecoverable
  with _ ->
    match topatts with
	[] ->
	  if verbose then
	    prerr_endline ("Filename flag of "^g^" is not defined. Assuming true");
	  true
      | _ -> is_with_fn verbose g [] topatts

let rec is_with_info verbose g topatts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Info _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Info v -> v
      | _ -> raise Unrecoverable
  with _ ->
    match topatts with
	[] ->
	  if verbose then
	    prerr_endline ("Info flag of "^g^" is not defined. Assuming true");
	  true
      | _ -> is_with_info verbose g [] topatts

let rec is_with_author verbose g topatts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Author _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Author v -> v
      | _ -> raise Unrecoverable
  with _ ->
    match topatts with
	[] ->
	  if verbose then
	    prerr_endline ("Author flag of "^g^" is not defined. Assuming true");
	  true
      | _ -> is_with_author verbose g [] topatts

let get_legend verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Legend _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Legend s -> " legend " ^s
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Legend of "^g^" is not defined. Assuming \""^dft^"\"");
    dft

let get_xlegend verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.XLegend _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.XLegend v -> v
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("X legend of "^g^" is not defined. Assuming \""^dft^"\"");
    dft

let get_xlabel verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.XLabel _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.XLabel v -> v
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("X label of "^g^" is not defined. Assuming \""^dft^"\"");
    dft

let get_ylabel verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YLabel _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YLabel v -> v
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Y label of "^g^" is not defined. Assuming \""^dft^"\"");
    dft

let get_ylegend verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YLegend _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YLegend v -> v
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Y legend of "^g^" is not defined. Assuming \""^dft^"\"");
    dft

let get_ylegendfactor verbose g atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.YLegendFactor _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.YLegendFactor v -> Some v
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Y legend factor of "^g^" is not defined. Assuming none");
    None

let get_notexistcolor verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.NotExistColor _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.NotExistColor (r,v,b) ->
	  string_of_float r ^ " " ^ string_of_float v ^" " ^ string_of_float b
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("NotExist color of "^g^" is not defined. Assuming "^dft);
    dft

let get_cleancolor verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.CleanColor _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.CleanColor (r,v,b) ->
	  string_of_float r ^ " " ^ string_of_float v ^" " ^ string_of_float b
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Clean/No occurrence color of "^g^" is not defined. Assuming "^dft);
    dft

let get_patterncolor verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.PatternColor _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.PatternColor (r,v,b) ->
	  string_of_float r ^ " " ^ string_of_float v ^" " ^ string_of_float b
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Occurrence color of "^g^" is not defined. Assuming "^dft);
    dft

(*
  Helper functions to retrieve color
*)

let get_color verbose g gatts curve =
  let (r,v,b) = Config.get_rgb_color verbose g gatts curve in
    string_of_float r ^ " " ^ string_of_float v ^" " ^ string_of_float b

(*
  Helper functions to retrieve linetype
*)
let get_linetype_in_atts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.LineType _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.LineType mt -> " linetype " ^ mt
      | _ -> raise Unrecoverable
  with _ ->
    raise Not_Found

let get_dft_linetype verbose g dft =
  if verbose then
    prerr_endline ("Linetype of "^g^" is not defined. Assuming \""^dft^"\"");
  dft

let get_linetype verbose g gatts curve dft =
  let (project, defect, atts, pos) = curve in
    try (* Check if linetype is set for this curve *)
      get_linetype_in_atts atts
    with Not_Found -> (* If not, get dft linetype from project/defect according to curve def. *)
      try
	match (project, defect) with
	  | None, Some d -> (* Defect is set for this curve,
			       Project should be cst for the graph.
			       We pick the defect linetype
			    *)
	      let dftatts = Setup.DftTbl.find Setup.smatchs d in
		get_linetype_in_atts dftatts
	  | Some p, None -> (* Project is set for this curve,
			       Defect should be cst for the graph.
			       We pick the project linetype
			    *)
	      let dftatts = snd (Setup.PrjTbl.find Setup.projects p) in
		get_linetype_in_atts dftatts
	  | _,_ ->  (* Prj and Defect are both set or none is set ! Pick dft *)
	      get_dft_linetype verbose g dft
      with Not_Found ->
	get_dft_linetype verbose g dft

(*
  Helper functions to retrieve marktype
*)
let get_marktype_in_atts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.MarkType _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.MarkType mt -> " marktype " ^ mt
      | _ -> raise Unrecoverable
  with _ ->
    raise Not_Found

let get_dft_marktype verbose g dft =
  if verbose then
    prerr_endline ("Marktype of "^g^" is not defined. Assuming \""^dft^"\"");
  dft

let get_marktype verbose g gatts curve dft =
  let (project, defect, atts, pos) = curve in
    try (* Check if marktype is set for this curve *)
      get_marktype_in_atts atts
    with Not_Found -> (* If not, get dft marktype from project/defect according to curve def. *)
      try
	match (project, defect) with
	  | None, Some d -> (* Defect is set for this curve,
			       Project should be cst for the graph.
			       We pick the defect marktype
			    *)
	      let dftatts = Setup.DftTbl.find Setup.smatchs d in
		get_marktype_in_atts dftatts
	  | Some p, None -> (* Project is set for this curve,
			       Defect should be cst for the graph.
			       We pick the project marktype
			    *)
	      let dftatts = snd (Setup.PrjTbl.find Setup.projects p) in
		get_marktype_in_atts dftatts
	  | _,_ ->  (* Prj and Defect are both set or none is set ! Pick dft *)
	      get_dft_marktype verbose g dft
      with Not_Found ->
	get_dft_marktype verbose g dft

let get_marksize_in_atts atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.MarkSize _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.MarkSize v -> " marksize "^string_of_float v
      | _ -> raise Unrecoverable
  with _ -> raise Not_Found

let get_marksize_in_atts_or_dft db g atts dft =
  try
    get_marksize_in_atts atts
  with _ ->
    if db then prerr_endline ("Marksize of "^g^" is not defined.");
    dft

(* FIXME: See get_marktype - Should use project and pattern definitions *)
let get_marksize db g atts curve dft =
  try
    let (_,_,catts,_) = curve in
      get_marksize_in_atts catts
  with _ ->
    get_marksize_in_atts_or_dft db g atts dft

let get_user_label atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Legend _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Legend s -> Some s
      | _ -> raise Unrecoverable
  with _ -> None

let get_label curve dft =
  let (project, defect, atts, pos) = curve in
  let userdef = get_user_label atts in
    match userdef with
	None ->
	  (match (project, defect) with
	     | None, Some d -> d
	     | Some p, None -> p
	     | Some p, Some d -> p ^ "/" ^d
	     | None, None -> dft
	  )
      | Some s -> s

let get_factor verbose g atts dft =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Factor _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Factor f -> f
      | _ -> raise Unrecoverable
  with _ ->
    if verbose then
      prerr_endline ("Factor of "^g^" is not defined. Assuming \""^ string_of_float dft^"\"");
    dft

let get_footer atts =
  try
    match
      List.find (fun x ->
		   match x with
		       Ast_config.Footer _ -> true
		     | _ -> false
		) atts
    with
	Ast_config.Footer mt -> mt
      | _ -> raise Unrecoverable
  with _ -> ""
