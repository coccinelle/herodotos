
let print_head path =
  let (_, vlist) = Config.get_versinfos "Linux-2.6" in
    prerr_endline ("Looking in " ^path);
    Git.load_authors path;
    (*   let author = Git.blame true vlist path "linux-2.6.0" "linux-2.6.26" 326 "mm/sparse.c" in *)
    (*     prerr_endline ("Bug author:"); *)
    (*     Git.prerr_author vlist author "linux-2.6.26"; *)
    let file = "arch/sparc64/kernel/irq.c" in
    let (author, _) = Git.blame true path "linux-2.6.0" "linux-2.6.28" file 946 in
      prerr_endline ("Bug author:");
      Git.prerr_author vlist author "linux-2.6.17";
      let (author, _) = Git.blame true path "linux-2.6.0" "linux-2.6.23" file 997 in
	prerr_endline ("Bug author:");
	Git.prerr_author vlist author "linux-2.6.17";
	let (author, _) = Git.blame true path "linux-2.6.0" "linux-2.6.22" file 908 in
	  prerr_endline ("Bug author:");
	  Git.prerr_author vlist author "linux-2.6.17";
	  let (author, _) = Git.blame true path "linux-2.6.0" "linux-2.6.17" file 1000 in
	    prerr_endline ("Bug author:");
	    Git.prerr_author vlist author "linux-2.6.17"

let test configfile =
  ignore(Config.parse_config configfile);
  prerr_endline "Config parsing OK!";
  let name = "Linux-2.6" in
  let scm = Config.get_scm name in
  let scmpath =  Str.replace_first (Str.regexp_string "git:") "" scm in
  print_head (!Setup.projectsdir ^ Config.get_prjdir name ^ "/" ^ scmpath)

