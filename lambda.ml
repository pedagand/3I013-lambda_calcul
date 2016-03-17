open Sexplib 


(* je garde la représentation que l'on avait de l'abstraction, avec la variable en string au début *)

type inTm = 
  | Abs of string * inTm
  | Inv of exTm
and exTm = 
  | Ann of inTm * inTm 
  | Star 
  | Pi of inTm * inTm 
  | BVar of int 
  | FVar of string 
  | Appl of exTm * inTm 


(* ici on va crée le parseur lisp avec le pretty printing *)
let rec parse env t = 
  let rec lookup_var env n v = 
    match env with 
    | [] -> FVar v 
    | w :: env when v = w -> BVar n
    | _ :: env -> lookup_var env (n+1) v
  in 
  match t with 
  | 
		     


