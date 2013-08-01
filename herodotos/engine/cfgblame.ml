let blame v1 v2 v3 configfile filter =
  ignore(Config.parse_config configfile);
  if v2 then prerr_endline "Config parsing OK!";
  if v1 then Config.show_config v2 v3
    
