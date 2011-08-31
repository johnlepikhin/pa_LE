
let debug =
	debug "string arg1=%s arg2=%i" "arg1" 2

let location =
	let (f, n) = location in
	Printf.printf "%s:%i\n" f n

let eachl =
	let lst = ["a"; "b"; "c"] in
	eachl lst (print_endline __)

let id =
	print_int int_id
