let freearg = ref ""

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ "\n\nOptions:\n"

let options = [
  "-h", Arg.Unit (fun _ -> ()) , " Display this list of options";
  "-help", Arg.Unit (fun _ -> ()), " Display this list of options";
  "--help", Arg.Unit (fun _ -> ()), " Display this list of options"
]

let anon_fun = fun x -> freearg := x

let _ =
  let aligned = Arg.align options in
  (try
     Arg.parse_argv Sys.argv aligned anon_fun usage_msg;
   with Arg.Bad msg ->
     (prerr_string msg; exit 0));
  let x = open_in_bin !freearg in
  input_value x;
  close_in x
