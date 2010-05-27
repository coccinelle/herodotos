
let print_head path =
  let (_, vlist) = Config.get_versinfos "Linux" in
    Git.load_authors path;
    (*   let author = Git.blame true vlist path "linux-2.6.26" 326 "mm/sparse.c" in *)
    (*     prerr_endline ("Bug author:"); *)
    (*     Git.prerr_author vlist author "linux-2.6.26"; *)
    let idx = Misc.get_idx_of_version vlist "linux-2.6.28" in
    let author = Git.blame true vlist path idx 946 "arch/sparc64/kernel/irq.c" in
      prerr_endline ("Bug author:");
      Git.prerr_author vlist author "linux-2.6.17";
      let idx = Misc.get_idx_of_version vlist "linux-2.6.23" in
      let author = Git.blame true vlist path idx 997 "arch/sparc64/kernel/irq.c" in
	prerr_endline ("Bug author:");
	Git.prerr_author vlist author "linux-2.6.17";
	let idx = Misc.get_idx_of_version vlist "linux-2.6.22" in
	let author = Git.blame true vlist path idx 908 "arch/sparc64/kernel/irq.c" in
	  prerr_endline ("Bug author:");
	  Git.prerr_author vlist author "linux-2.6.17";
	  let idx = Misc.get_idx_of_version vlist "linux-2.6.17" in
	  let author = Git.blame true vlist path idx 1000 "arch/sparc64/kernel/irq.c" in
	    prerr_endline ("Bug author:");
	    Git.prerr_author vlist author "linux-2.6.17"

let test configfile =
  ignore(Config.parse_config configfile);
  prerr_endline "Config parsing OK!";
  Setup.PrjTbl.iter
    (fun name (_,atts) ->
       let scm = Config.get_scm name in
       let scmpath =  Str.replace_first (Str.regexp_string "git:") "" scm in
	 print_head scmpath
    )
    Setup.projects;

