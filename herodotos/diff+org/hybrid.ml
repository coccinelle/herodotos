
let verbose = ref false
let gnudiff = ref ""
let gumtree = ref ""
let gumtree_cmd = ref []

let register_cmd list =
  gumtree_cmd := list

let parse_config v f =
  verbose := v;
  gnudiff := f ^ Global.patchext;
  gumtree := f ^ Global.gumtreeext

let make_path prefix ver file =
  prefix ^ Filename.dir_sep ^ ver ^ Filename.dir_sep ^ file

let get_cmd gumfile : string option =
  try
    Some (List.assoc gumfile !gumtree_cmd)
  with Not_found ->
    None (* If the file is already in the cache, the command will not be in the list. *)

(* Called by Occ_correl *)
let get_cmd2 file ver =
  let gumfile = make_path !gumtree ver file in
  get_cmd gumfile

let alt_new_pos (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  let gumfile = make_path !gumtree ver file in
  let logmsg=Printf.sprintf "GNU Diff correlation failed. Trying Gumtree with %s" gumfile in
  Bolt.Logger.log "" Bolt.Level.TRACE logmsg;
  let diffs = Gumtree.parse_diff !verbose (!gumtree^Filename.dir_sep) gumfile in
  Gumtree.compute_new_pos_with_gumtree diffs file ver pos

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  Debug.profile_code_silent "Hybrid.compute_new_pos"
    (fun () ->
      let gnufile = make_path !gnudiff ver file in
      let diffs = Gnudiff.parse_diff !verbose (!gnudiff^Filename.dir_sep) gnufile in
      let (_, new_pos) as gnures = Gnudiff.compute_new_pos_with_findhunk diffs file ver pos in
      match new_pos with
	  (* This will force the execution of 'alt_new_pos diffs file ver pos' during the second phase. *)
	  (Ast_diff.Deleted false, 0, 0) -> (true, (Ast_diff.Deleted true, 0, 0))
	| _ -> gnures
    )
