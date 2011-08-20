open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

exit 1;;

let _ =
	Array.iter print_endline Sys.argv

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let split s ch =
	let x = ref [] in
	let rec go s =
	let pos = String.index s ch in
		x := (String.before s pos)::!x;
		go (String.after s (pos + 1))
	in
	try
		go s
	with Not_found -> !x

let split_nl s = split s '\n'

	let before_space s =
	try
		String.before s (String.index s ' ')
	with Not_found -> s

(* this lists all supported packages *)
let installed_packages () = List.map before_space (split_nl & run_and_read "ocamlfind list")

(* List of syntaxes: *)
let syntaxes = [ "camlp4o"; "camlp4r"; "camlp5o"; "camlp5r" ]

let flag_all_except_link tag f =
	flag ["ocaml"; "compile"; tag] f;
	flag ["ocaml"; "ocamldep"; tag] f;
	flag ["ocaml"; "doc"; tag] f

let flag_all tag f =
	flag_all_except_link tag f;
	flag ["ocaml"; "link"; tag] f

let deps = ["META"; "pa_le.cmo"; "pa_le.cmi"; "pa_le.cmx"; "pa_le.o"]

let files = List.map (fun f -> A f) deps

let _ =
	dispatch begin function
		| After_rules ->

			let ocamlfind x = S[A"ocamlfind"; A x] in
			Options.ocamlc   := ocamlfind "ocamlc";
			Options.ocamlopt := ocamlfind "ocamlopt";
			Options.ocamldep := ocamlfind "ocamldep";
			Options.ocamldoc := ocamlfind "ocamldoc";

			flag ["ocaml"; "link"; "program"] & A"-linkpkg";

			List.iter
				(fun package -> flag_all ("pkg_" ^ package) (S[A"-package"; A package]))
				(installed_packages ());

			List.iter
				(fun syntax -> flag_all_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
				syntaxes;

			rule "Install" ~prod:"install" ~deps
				(fun _ _ -> Cmd (S(A"ocamlfind" :: A"install" :: A"pa_le" :: files)));

			rule "Uninstall" ~prod:"uninstall" ~deps:[]
				(fun _ _ -> Cmd (S[A"ocamlfind"; A"remove"; A"pa_le"]));

		| _ -> ()
	end
