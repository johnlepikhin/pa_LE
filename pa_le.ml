open Camlp4
open Camlp4.PreCast

module Id = struct
	let name = "Camlp4LE"
	let version = Sys.ocaml_version
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
	include Syntax

	module Location = struct
		let get _loc =
			let filename = Loc.file_name _loc in
			let line = Loc.start_line _loc in
			let line = string_of_int line in
			(filename, line)

		let register _loc =
			let (f, l) = get _loc in
				<:expr<
				($str:f$, $int:l$)
			>>
	end

	module Debug = struct
		let enabled = ref false

		let register ~fmt ~args _loc =
			if !enabled then
			begin
				let (f, l) = Location.get _loc in
				let s = List.fold_left (fun tl e -> <:expr< $tl$ $e$ >>) <:expr< Printf.sprintf $str:fmt$ >> args in
				<:expr<
					Le_log.log ~filename:$str:f$ ~line:$int:l$ $s$
				>>
			end
			else
				<:expr< () >>
	end

	module DefaultValue = struct
		let name = "__default_value__"

		let register _loc =
			<:expr< $lid:name$ >>
	end

	module Each = struct
		let register ~modname ~fname ~lst ~name ~f _loc =
			let vname = match name with
				| None -> DefaultValue.name
				| Some s -> s
			in
			<:expr<
				$uid:modname$.$lid:fname$ (fun $lid:vname$ -> $f$) $lst$
			>>
	end

	module EachL = struct
		let register = Each.register ~modname:"List" ~fname:"iter"
	end

	module EachA = struct
		let register = Each.register ~modname:"Array" ~fname:"iter"
	end

	module MapL = struct
		let register = Each.register ~modname:"List" ~fname:"map"
	end

	module Id = struct
		let fname = "pa_le.id"

		type t = {
			mutable m : int;
			h : (Loc.t, int) Hashtbl.t;
		}

		let get _loc =
			let v =
				try
					let ch = open_in fname in
					let r = input_value ch in
					close_in ch;
					r
				with
					| _ -> {
							h = Hashtbl.create 100;
							m = 0;
						}
			in
			let id =
				try
					Hashtbl.find v.h _loc
				with
					| _ ->
						let r = v.m + 1 in
						v.m <- r;
						Hashtbl.add v.h _loc r;
						r
			in
			let ch = open_out fname in
			output_value ch v;
			close_out ch;
			string_of_int id

		let register_int _loc =
			let id = get _loc in
			<:expr< $int:id$ >>

		let register_string _loc =
			let id = get _loc in
			<:expr< $str:id$ >>

	end

	EXTEND Gram

	GLOBAL: expr;

		expr: LEVEL "." [
			[ "__" -> DefaultValue.register _loc ]
			| [ LIDENT "int_id" -> Id.register_int _loc ]
			| [ LIDENT "string_id" -> Id.register_string _loc ]
		];

		expr: LEVEL "top" [
			[ LIDENT "debug"; fmt = STRING; args = LIST0 [ n = expr LEVEL "." -> n ] -> Debug.register ~fmt ~args _loc ]
			| [ LIDENT "location" -> Location.register _loc ]
			| [ LIDENT "eachl"; lst = expr LEVEL "."; name = OPT [ s = LIDENT -> s ]; "("; f = expr LEVEL ";"; ")" -> EachL.register ~lst ~name ~f _loc ]
			| [ LIDENT "eacha"; lst = expr LEVEL "."; name = OPT [ s = LIDENT -> s ]; "("; f = expr LEVEL ";"; ")" -> EachA.register ~lst ~name ~f _loc ]
			| [ LIDENT "mapl"; lst = expr LEVEL "."; name = OPT [ s = LIDENT -> s ]; "("; f = expr LEVEL ";"; ")" -> MapL.register ~lst ~name ~f _loc ]
		];
	END;

	Options.add "-DEBUG" (Arg.Unit (fun _ -> Debug.enabled := true))
		"Enable debug"
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
