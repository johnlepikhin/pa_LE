open Camlp4
open Camlp4.PreCast
open Syntax

let error s = raise (Failure s)

module Debug = struct
	let enabled = ref false

	let register ~fmt ~args _loc =
		if !enabled then
		begin
			let filename = Loc.file_name _loc in
			let line = Loc.start_line _loc in
			let line = string_of_int line in
			let s = List.fold_left (fun tl e -> <:expr< $tl$ $e$ >>) <:expr< Printf.sprintf $str:fmt$ >> args in
			<:expr<
				Le_log.log ~filename:$str:filename$ ~line:$int:line$ $s$
			>>
		end
		else
			<:expr< () >>
end

EXTEND Gram

GLOBAL: expr;

	expr: LEVEL "top" [
		[ LIDENT "debug"; fmt = STRING; args = LIST0 [ n = expr LEVEL "." -> n ] -> Debug.register ~fmt ~args _loc ]
	];

END;

Options.add "-DEBUG" (Arg.Unit (fun _ -> Debug.enabled := true))
	"Enable debug"
