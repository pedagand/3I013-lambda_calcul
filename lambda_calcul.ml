type lambda_term =
  | FreeVar of string 
  | BoundVar of int 
  | Abs of lambda_term
  | Appl of (lambda_term * lambda_term)


let rec lambda_term_to_string t = 
  match t with
  | FreeVar v -> v
  | BoundVar v -> string_of_int v        
  | Abs x -> "[]." ^ lambda_term_to_string x 
  | Appl (x,y) -> "(" ^ lambda_term_to_string x ^ " " ^ lambda_term_to_string y ^ ")"

let rec substitution t var tsub= 
match t with 
| FreeVar v -> FreeVar v 
| BoundVar v -> if v = var then tsub else BoundVar v
| Abs x -> Abs(substitution x (var+1) tsub)
| Appl (x,y) -> Appl(substitution x var tsub,substitution y var tsub)



let alpha_equiv terme_un terme_deux = 
(lambda_term_to_string terme_un) = (lambda_term_to_string terme_deux)


let reduction t = 
match t with
| FreeVar v -> FreeVar v
| BoundVar v -> BoundVar v
| Abs x -> Abs(x)
| Appl(Abs(x),y) -> substitution x 0 y
| Appl(x,y) -> failwith "erreur"


let rec evaluation t = 
match t with 
| FreeVar v -> FreeVar v 
| BoundVar v -> BoundVar v 
| Abs x -> Abs x
| Appl(Abs(x),y) -> evaluation(reduction t)
| Appl(BoundVar x,y) -> Appl(BoundVar x,y)
| Appl(FreeVar x,y) -> Appl(FreeVar x,y)
| Appl(x,y) -> evaluation(Appl(evaluation x, y))



let x = Appl(Abs(Appl(BoundVar 0,BoundVar 0)),Abs(BoundVar 0))
let y = Appl(Abs(Appl(BoundVar 0, FreeVar "y")),FreeVar "a")
let w = Appl(Appl(Abs(Abs(BoundVar 0)),FreeVar "x"),Abs(FreeVar "u"))
let z = Appl(Abs(Appl(BoundVar 0,BoundVar 0)),FreeVar "u")
let test = Appl(Appl(Abs(BoundVar 0),FreeVar "u"),Abs(BoundVar 0))

(* let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation x))
let () = Printf.printf "%s \n" (lambda_term_to_string (x))
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation x))
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation z))
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation test)) *)

let lambda_true = Abs(Abs(BoundVar 1))
let lambda_false = Abs(Abs(BoundVar 0))		      
(* let lambda_if_else = Abs(Abs(Abs(Appl(BoundVar 2,(Appl(BoundVar 1,BoundVar 0)))))) *)
let lambda_if_else = Abs(Abs(Abs(Appl(Appl(BoundVar 2, BoundVar 1),BoundVar 0))))

(* a faire mais c'est une appl d'une appl d'une appl *)
			
let test2 = Appl(Appl(lambda_if_else,lambda_false),FreeVar "y")
  
let test = Appl(Appl(Appl(lambda_if_else,lambda_false),FreeVar "y"),FreeVar "x")
(*let test3 = Appl(lambda_if_else, *)

let () = Printf.printf "%s \n" (lambda_term_to_string lambda_true)
let () = Printf.printf "%s \n" (lambda_term_to_string lambda_false)
let () = Printf.printf "%s \n" (lambda_term_to_string lambda_if_else)
let () = Printf.printf "%s \n" (lambda_term_to_string test)
let () = Printf.printf "\n"
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation test2))
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation test))

let () = Printf.printf "\n"
let arguments3 = Appl(Appl(Abs(BoundVar 0),Abs(BoundVar 0)),FreeVar "y")
let () = Printf.printf "%s \n" (lambda_term_to_string (evaluation arguments3))
