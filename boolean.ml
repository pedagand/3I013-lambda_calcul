open Lambda

(** * Booleans *)

let ctrue = read "(lambda (tru fls) tru)"
let cfalse = read "(lambda (tru fls) fls)"
let cifthenelse = read "(lambda (b ifTru ifFls) (b ifTru ifFls))"

(* a supprimer quand tout sera finis
let () = Printf.printf "%s \n" (lambda_term_to_string(ctrue)) *)


