
let filter_sub version prefix found org =
  let Ast_org.Org(l, s, r, link, sub) = org in
  let (file, ver, pos, face, t) = Org.flat_link prefix 1 link in
    if version = ver then
      Some (org, pos, t)
    else
      found

let filter_version_of version prefix acc org =
  let (l, s, r, file, ver, pos, face, t, _, _, sub) = org in
  let text = Org.clean_link_text prefix ver file pos t in
  let filtered =
    List.fold_left (filter_sub version prefix) None sub
  in
    match filtered with
	Some (newsub, newpos, tsub) ->
	  (l, s, r, file, version, newpos, face, text^" "^tsub,
	   {Ast_org.is_head=true}, {Ast_org.def=None},
	   [newsub]
	  )::acc
      | None -> acc

let filter_version extract prefix (orgs:Ast_org.bugs) : Ast_org.bugs =
  let filtered = List.fold_left (filter_version_of extract prefix) [] orgs in
    List.rev filtered
