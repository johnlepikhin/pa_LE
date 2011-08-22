open Ocamlbuild_plugin

let files = ["META"; "pa_le.cmo"; "pa_le.cmi"; "pa_le.cmx"; "pa_le.o"; "le.cmxa"; "le.cma"; "le.mli"; "le.cmi"; "le.a"]

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

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

let ocamlfind x = S[A"ocamlfind"; A x]

let rule_ocamlfind l _ _ = Cmd (S((A"ocamlfind") :: l))

let installer_rules ~files ~name =
	let deps = List.map (fun f -> f) files in
	let files = List.map (fun f -> A f) files in
	rule ("Install " ^ name) ~prod:"install" ~deps (rule_ocamlfind (A"install" :: A name :: files));
	rule ("Uninstall " ^ name) ~prod:"uninstall" ~deps:[] (rule_ocamlfind [A"remove"; A name]);
	rule ("Reinstall" ^ name) ~prod:"reinstall" ~deps:["uninstall"; "install"] (fun _ _ -> Cmd (S[A"/bin/true"]))

let prepare () =
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
		syntaxes

let _ =
	dispatch begin function
		| After_rules ->
			prepare ();
			installer_rules ~files ~name:"pa_le";
		| _ -> ()
	end
