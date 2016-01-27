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
