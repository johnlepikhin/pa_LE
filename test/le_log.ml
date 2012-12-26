

let log ?arg ~filename ~line s =
	Printf.printf "%s:%i %s\n" filename line s
