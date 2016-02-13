open Lambda

(** * Booleans *)

let ctrue = read "(lambda (tru fls) tru)"
let cfalse = read "(lambda (tru fls) fls)"
let cifthenelse = read "(lambda (b ifTru ifFls) (b ifTru ifFls))"


			
let test2 = Appl(Appl(cifthenelse, cfalse),FreeVar "y")
  
let test = Appl(Appl(Appl(cifthenelse,cfalse),FreeVar "y"),FreeVar "x")
(*let test3 = Appl(lambda_if_else, *)

(* let () = Printf.printf "%s \n" (lambda_term_to_string lambda_true)
let () = Printf.printf "%s \n" (lambda_term_to_string lambda_false)
let () = Printf.printf "%s \n" (lambda_term_to_string lambda_if_else)
let () = Printf.printf "%s \n" (lambda_term_to_string test)
let () = Printf.printf "\n"
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation test2))
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation test))

let () = Printf.printf "\n"
let arguments3 = Appl(Appl(Abs(BoundVar 0),Abs(BoundVar 0)),FreeVar "y")
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation arguments3)) *)
