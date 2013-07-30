
let preinit configfile =
   let check = Config.parse_check_versions (".projects_"^configfile) in 
   let out_channel = open_out (".projects_"^configfile) in
   let versions = Config.parse_preinit configfile in
   Printf.fprintf out_channel "%s" versions;
   close_out out_channel
