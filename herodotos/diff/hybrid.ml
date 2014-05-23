
let verbose = ref false
let gnudiff = ref ""
let gumtree = ref ""

let parse_config v f =
  verbose := v;
  gnudiff := f ^ Global.patchext;
  gumtree := f ^ Global.gumtreeext

let make_path prefix ver file =
  prefix ^ Filename.dir_sep ^ ver ^ Filename.dir_sep ^ file

let alt_new_pos (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  let gumfile = make_path !gumtree ver file in
  if !Misc.debug then Printf.eprintf "GNU Diff correlation failed. Trying Gumtree with %s\n" gumfile;
  let diffs = Gumtree.parse_diff !verbose (!gumtree^Filename.dir_sep) gumfile in
  Gumtree.compute_new_pos_with_gumtree diffs file ver pos

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos : bool * (Ast_diff.lineprediction * int * int) =
  Debug.profile_code_silent "Hybrid.compute_new_pos"
    (fun () ->
      let gnufile = make_path !gnudiff ver file in
      let diffs = Gnudiff.parse_diff !verbose (!gnudiff^Filename.dir_sep) gnufile in
      let (_, new_pos) as gnures = Gnudiff.compute_new_pos_with_findhunk diffs file ver pos in
      match new_pos with
	  (Ast_diff.Deleted, 0, 0) -> alt_new_pos diffs file ver pos
	| _ -> gnures
    )
