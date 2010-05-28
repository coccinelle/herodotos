let htmlfiles = ref []
let pngfiles = ref []
let svgfiles = ref []

let register (file: string) =
  htmlfiles := file :: !htmlfiles

let register_png (file: string) =
  pngfiles := file :: !pngfiles

let register_svg (file: string) =
  svgfiles := file :: !svgfiles

let gen_png path =
  let index = path ^ "/index.png.html" in
  let ch = open_out index in
    Printf.fprintf ch ("<html><head></head><body>\n");
    Printf.fprintf ch ("<h1>Herodotos generated graphs</h1>\n");
    List.iter (fun filename ->
		 let f = Filename.basename filename in
		 Printf.fprintf ch ("<a alt=\"%s\" href=\"%s\">") f f;
		 Printf.fprintf ch ("<img alt=\"%s\" src=\"small_%s\" width=\"300\" height=\"200\">") f f;
		 Printf.fprintf ch ("</a>\n")
	      ) (List.sort (compare) !pngfiles);
    Printf.fprintf ch ("</body></html>\n");
    close_out ch

let gen_svg path =
  let index = path ^ "/index.svg.html" in
  let ch = open_out index in
    Printf.fprintf ch ("<html><head></head><body>\n");
    Printf.fprintf ch ("<h1>Herodotos generated graphs</h1>\n");
    List.iter (fun filename ->
		 let f = Filename.basename filename in
		 let png = Str.replace_first (Str.regexp_string ".svg")".png" f in

		   (* For SVG *)
		   Printf.fprintf ch "<object type=\"image/svg+xml\" data=\"%s\" " f;
		   Printf.fprintf ch "name=\"%s\" width=\"300\" height=\"200\" " f;
		   Printf.fprintf ch "standby=\"%s\" usemap=\"#%s_map\">\n" f f;

		   (* For PNG *)
		   Printf.fprintf ch "<object type=\"image/png\" data=\"small_%s\" " png;
		   Printf.fprintf ch "name=\"%s\" width=\"300\" height=\"200\" " png;
		   Printf.fprintf ch "standby=\"%s\" usemap=\"#%s_map\">\n" png png;

		   (* <embed> is not a W3C standard and should be removed... *)
		   Printf.fprintf ch "<embed src=\"%s\" width=\"300\" height=\"200\" " f;
		   Printf.fprintf ch "pluginspage=\"http://www.adobe.com/svg/viewer/install/\" />";

		   Printf.fprintf ch "</object>\n"; (* For PNG *)
		   Printf.fprintf ch "</object>\n"; (* For SVG *)

		   Printf.fprintf ch ("<map name=\"%s_map\">") f;
		   Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,300,200\">") f f;
		   Printf.fprintf ch ("</map>\n");

		   Printf.fprintf ch ("<map name=\"%s_map\">") png;
		   Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,300,200\">") png png;
		   Printf.fprintf ch ("</map>\n");
	      ) (List.sort (compare) !svgfiles);
    Printf.fprintf ch ("</body></html>\n");
    close_out ch

let gen_html_files path base html png svg =
  let index = path ^ "/" ^ html in
  let ch = open_out index in
    Printf.fprintf ch "<html><head></head><body>\n";
    Printf.fprintf ch "<h1>%s</h1>\n" base;

    (* For SVG *)
    Printf.fprintf ch "<object type=\"image/svg+xml\" data=\"%s\" " svg;
    Printf.fprintf ch "name=\"%s\" width=\"100%%\" standby=\"%s\" usemap=\"#%s_map\">\n" svg svg svg;

    (* For PNG *)
    Printf.fprintf ch "<object type=\"image/png\" data=\"%s\" " png;
    Printf.fprintf ch "name=\"%s\" width=\"100%%\" standby=\"%s\" usemap=\"#%s_map\">\n" png png png;

    (* <embed> is not a W3C standard and should be removed... *)
    Printf.fprintf ch "<embed src=\"%s\" " svg;
    Printf.fprintf ch "pluginspage=\"http://www.adobe.com/svg/viewer/install/\" />";

    Printf.fprintf ch "</object>\n"; (* For PNG *)
    Printf.fprintf ch "</object>\n"; (* For SVG *)

    Printf.fprintf ch ("<map name=\"%s_map\">") svg;
    Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,800,600\">") svg svg;
    Printf.fprintf ch ("</map>\n");

    Printf.fprintf ch ("<map name=\"%s_map\">") png;
    Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,800,600\">") png png;
    Printf.fprintf ch ("</map>\n");

    Printf.fprintf ch ("</body></html>\n");
    close_out ch

let gen_html_index path config =
  let index = path ^ "/index.html" in
  let ch = open_out index in
    Printf.fprintf ch ("<html><head></head><body>\n");
    Printf.fprintf ch ("<h1>Herodotos generated graphs for %s</h1>\n") config;
    List.iter (fun filename ->
		 let base = Filename.basename filename in
		 let html = base ^ ".html" in
		 let svg = base ^ ".svg" in
		 let png = base ^ ".png" in

		   Printf.fprintf ch "<div style=\"position:float;float:left;width:300;\">\n";
		   (* For SVG *)
		   Printf.fprintf ch "<object type=\"image/svg+xml\" data=\"%s\" " svg;
		   Printf.fprintf ch "name=\"%s\" width=\"300\" height=\"200\" " svg;
		   Printf.fprintf ch "standby=\"%s\" usemap=\"#%s_map\">\n" svg svg;

		   (* For PNG *)
		   Printf.fprintf ch "<object type=\"image/png\" data=\"small_%s\" " png;
		   Printf.fprintf ch "name=\"%s\" width=\"300\" height=\"200\" " png;
		   Printf.fprintf ch "standby=\"%s\" usemap=\"#%s_map\">\n" png png;

		   (* <embed> is not a W3C standard and should be removed... *)
		   Printf.fprintf ch "<embed src=\"%s\" " svg;
		   Printf.fprintf ch "pluginspage=\"http://www.adobe.com/svg/viewer/install/\" />";

		   Printf.fprintf ch "</object>\n"; (* For PNG *)
		   Printf.fprintf ch "</object>\n"; (* For SVG *)

		   Printf.fprintf ch ("<map name=\"%s_map\">") svg;
		   Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,300,200\">") svg html;
		   Printf.fprintf ch ("</map>\n");

		   Printf.fprintf ch ("<map name=\"%s_map\">") png;
		   Printf.fprintf ch ("<area alt=\"%s\" href=\"%s\" shape=\"rect\" coords=\"0,0,300,200\">") png html;
		   Printf.fprintf ch ("</map>\n");
		   Printf.fprintf ch "<div style=\"text-align: center;\">\n";
		   Printf.fprintf ch "%s<br />" base;
		   Printf.fprintf ch "[<a href=\"%s\">HTML</a>]\n" html;
		   Printf.fprintf ch "[<a href=\"%s\">SVG</a>]\n" svg;
		   Printf.fprintf ch "[<a href=\"%s\">PNG</a>]\n" png;
		   Printf.fprintf ch "</div>\n";
		   Printf.fprintf ch "</div>\n";

		   gen_html_files path base html png svg
	      ) (List.sort (compare) !htmlfiles);
    Printf.fprintf ch ("</body></html>\n");
    close_out ch

let gen_site path config =
  gen_png path;
  gen_svg path;
  gen_html_index path config
