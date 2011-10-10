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
			h : (string, int) Hashtbl.t;
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
			let key = Loc.to_string _loc in
			let id =
				try
					Hashtbl.find v.h key
				with
					| _ ->
						let r = v.m + 1 in
						v.m <- r;
						Hashtbl.add v.h key r;
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

	module ObfuscatePassword = struct
		let _ =
			Random.self_init ()

		let random sz =
			let r = String.create sz in
			for i=0 to sz-1 do
				r.[i] <- Char.chr ((Random.int 254)+1)
			done;
			r

		let string ~s _loc =
			let s = String.escaped s in
			<:expr< $str:s$ >>

		let funcs = ref [| |]

		let make ~s ~deepness _loc =
			if deepness = 0 then
				string ~s _loc
			else
				let l = Array.length !funcs in
				let f = (!funcs).(Random.int l) in
				f ~s ~deepness:(deepness-1) _loc

		let add_f f =
			funcs := Array.append !funcs [|f|]

		let add_xor =
			let f ~s ~deepness _loc =
				let l = String.length s in
				let xor = random ((Random.int 10) + 1) in
				let xorl = String.length xor in
				let s =
					let r = String.create l in
					let rec loop pos =
						if pos = -1 then
							()
						else
						begin
							let posx = pos mod xorl in
							r.[pos] <- Char.chr ((Char.code s.[pos]) lxor (Char.code xor.[posx]));
							loop (pos-1)
						end
					in
					loop (l-1);
					r
				in
				let expr = make ~s ~deepness _loc in
				let xor_expr = make ~s:xor ~deepness _loc in
				<:expr<
					let s = $expr$ in
					let xor = $xor_expr$ in
					let r = String.create $int:string_of_int l$ in
					let rec loop pos =
						if pos = -1 then
							()
						else
						begin
							let posx = pos mod $int:string_of_int xorl$ in
							r.[pos] := Char.chr ((Char.code s.[pos]) lxor (Char.code xor.[posx]));
							loop (pos-1)
						end
					in
					begin
						loop ($int:string_of_int l$-1);
						r
					end
				>>
			in
			add_f f;
			add_f f;
			add_f f;
			add_f f

		let add_rev =
			add_f (fun ~s ~deepness _loc ->
				let l = String.length s in
				let r = String.create l in
				for i=0 to l-1 do
					r.[i] <- s.[l-1-i];
				done;
				let expr = make ~s:r ~deepness _loc in
				<:expr<
					let s = $expr$ in
					let l = $int:string_of_int l$ in
					let r = String.create l in
					begin
						for i=0 to l-1 do
							r.[i] := s.[l-1-i];
						done;
						r
					end
				>>
			)

		let add_bitorder =
			add_f (fun ~s ~deepness _loc ->
				let l = String.length s in
				let r = String.create l in
				for i=0 to l-1 do
					let b1 = (Char.code s.[i]) land 0b1111 in
					let b2 = (Char.code s.[i]) lsr 4 in
					r.[i] <- Char.chr ((b1 lsl 4) + b2);
				done;
				let expr = make ~s:r ~deepness _loc in
				<:expr<
					let s = $expr$ in
					let l = $int:string_of_int l$ in
					let r = String.create l in
					begin
						for i=0 to l-1 do
							let b1 = (Char.code s.[i]) land 0b1111 in
							let b2 = (Char.code s.[i]) lsr 4 in
							r.[i] := Char.chr ((b1 lsl 4) + b2);
						done;
						r
					end
				>>
			)

		let add_split =
			add_f (fun ~s ~deepness _loc ->
				let l = String.length s in
				if l>0 then
				begin
					let p = Random.int l in
					let s1 = String.sub s 0 p in
					let s2 = String.sub s p (l-p) in
					let expr1 = make ~s:s1 ~deepness _loc in
					let expr2 = make ~s:s2 ~deepness _loc in
					<:expr<
						String.concat "" [$expr1$; $expr2$]
					>>
				end
				else
					make ~s ~deepness:(deepness+1) _loc
			)

		let add_prefix =
			add_f (fun ~s ~deepness _loc ->
				let l = String.length s in
				let prefix_len = (Random.int 10)+3 in
				let prefix = random prefix_len in
				let s = prefix ^ s in
				let expr = make ~s ~deepness _loc in
				<:expr<
					String.sub $expr$ $int:string_of_int prefix_len$ $int:string_of_int l$
				>>
			)

		let add_suffix =
			add_f (fun ~s ~deepness _loc ->
				let l = String.length s in
				let suffix = random ((Random.int 10)+3) in
				let s = s ^ suffix in
				let expr = make ~s ~deepness _loc in
				<:expr<
					String.sub $expr$ 0 $int:string_of_int l$
				>>
			)

		let register ~pwd ~deepness _loc =
			let deepness = int_of_string deepness in
			let expr = make ~s:pwd ~deepness _loc in
			<:expr<
				fun () -> $expr$
			>>
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
			| [ LIDENT "obfuscate_password"; pwd = STRING; deepness = INT -> ObfuscatePassword.register ~pwd ~deepness _loc ]
		];
	END;

	Options.add "-DEBUG" (Arg.Unit (fun _ -> Debug.enabled := true))
		"Enable debug"
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
