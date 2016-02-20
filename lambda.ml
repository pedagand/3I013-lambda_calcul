open Sexplib

(*
  To load in the OCaml toplevel:
  
  #use "topfind";;
  #require "sexplib";;
 
*)

type lambda_term =
  | FreeVar of string 
  | BoundVar of int 
  | Abs of lambda_term
  | Appl of (lambda_term * lambda_term)
  | True | False | IfThenElse of lambda_term * lambda_term * lambda_term
(*  | Zero | Suc of lambda_term | Iter of lambda_term * lambda_term * lambda_term *)

type typ = 
| Bool
| Nat 
| Fleche of typ * typ

(* TODO: remember the name of the abstractions, for pretty-printing *)
(* TODO: rajouter constructeur des vrais ect... *)

(** * A simple parser *)

let rec parse env t 
    = let rec lookup_var env n v
        = match env with
        | [] -> FreeVar v
        | w :: env when v = w -> BoundVar n
        | _ :: env -> lookup_var env (n+1) v 
      in
      match t with
      | Sexp.List [Sexp.Atom "lambda"; Sexp.Atom var; body] -> 
         Abs (parse (var :: env) body)
      | Sexp.List [Sexp.Atom "lambda"; Sexp.List vars; body] -> 
         let vars = List.map (function 
           | Sexp.Atom v -> v
           | _ -> failwith "Parser: invalid variable") vars 
         in
         List.fold_right 
           (fun var b -> Abs b)
           vars
           (parse (List.append (List.rev vars) env) body)
      | Sexp.Atom v -> lookup_var env 0 v
      | Sexp.List (f :: args) -> 
         List.fold_left 
           (fun x y -> Appl (x, y))
           (parse env f) 
           (List.map (parse env) args)
      | _ -> failwith "Parser: ill-formed input."

let read t = parse [] (Sexp.of_string t)


(** * A simple printer *)

(* TODO: print S-expression instead. *)
let rec lambda_term_to_string t = 
  match t with
  | FreeVar v -> v
  | BoundVar v -> string_of_int v        
  | Abs x -> "[]." ^ lambda_term_to_string x 
  | Appl (x,y) -> "(" ^ lambda_term_to_string x ^ " " ^ lambda_term_to_string y ^ ")"
							    

(** * Reduction *)

let rec substitution t var tsub 
    = match t with 
    | FreeVar v -> FreeVar v 
    | BoundVar v when v = var -> tsub
    | BoundVar v -> BoundVar v
    | Abs x -> Abs(substitution x (var+1) tsub)
    | Appl (x,y) -> Appl(substitution x var tsub,substitution y var tsub)


(* XXX: Unnecessarily complex: it is enough to compare the raw terms *)
let alpha_equiv terme1 terme2 = 
  lambda_term_to_string terme1 = lambda_term_to_string terme2

let reduction t 
    = match t with
    | FreeVar v -> FreeVar v
    | BoundVar v -> BoundVar v
    | Abs x -> Abs(x)
    | Appl(Abs(x),y) -> substitution x 0 y
    | Appl(x,y) -> failwith "erreur reduction"


let rec evaluation t 
    = match t with 
    | FreeVar v -> FreeVar v 
    | BoundVar v -> BoundVar v 
    | Abs x -> Abs x
    | Appl(Abs(x),y) -> evaluation(reduction t)
    | Appl(BoundVar x,y) -> Appl(BoundVar x,y)
    | Appl(FreeVar x,y) -> Appl(FreeVar x,y)
    | Appl(x,y) -> evaluation(Appl(evaluation x, y))

(* let rec reduction_forte t = 
match t with 
| FreeVar v -> FreeVar v 
| BoundVar v -> BoundVar v
| Abs x -> Abs(reduction_forte x)
| Appl(Abs(x),y) -> reduction_forte(substitution x 0 y)
| Appl(x,y) -> (Appl(reduction_forte x, reduction_forte y)) *)


(* let rec reduction_forte t = 
match t with
| FreeVar v -> FreeVar v 
| BoundVar v -> BoundVar v
| Abs x -> Abs(reduction_forte x)
| Appl(BoundVar x,y) -> Appl(BoundVar x, reduction_forte y)
| Appl(FreeVar x,y) -> Appl(FreeVar x, reduction_forte y)
| Appl(Abs(x),y) -> reduction_forte(substitution x 0 (reduction_forte y))
| Appl(x,y) -> reduction_forte(Appl(reduction_forte x ,reduction_forte y)) *)

let rec relie_libre i bv t 
    = match t with 
    | BoundVar v -> BoundVar v
    | FreeVar v when v = string_of_int i -> BoundVar bv
    | FreeVar v -> FreeVar v
    | Abs(x) -> Abs(relie_libre i (bv + 1) x)
    | Appl(x,y) -> Appl(relie_libre i bv x,relie_libre i bv y)

let rec reduction_forte t i 
    = match t with 
    | FreeVar v -> FreeVar v
    | BoundVar v -> BoundVar v
    | Abs x -> Abs(relie_libre i 0 (reduction_forte (substitution x 0 (FreeVar (string_of_int i))) (i+1)))
    | Appl(FreeVar x,y) -> Appl(FreeVar x, reduction_forte y i)
    | Appl(Abs(x),y) -> reduction_forte(substitution x 0 y) i
    | Appl(x,y) -> reduction_forte (Appl((reduction x ),y)) i

(* | Appl(Abs(x),Appl(y,z)) -> Appl(Abs(x),(Appl(y,z))) *)
(* | Appl(Abs(x),Appl(y,z)) -> reduction_forte (Appl(Abs(x),(reduction_forte(Appl(y,z)) i))) i *)
	       

(* fonction de check de typage *)

(* le contexte est du type liste de (var,type) *)
let rec type_var_contexte contexte var = 
match contexte with
| [] -> failwith "Contexte vide"
| (x,y)::[] -> if x = var then y else failwith "Pas dans le context"
| (x,y)::z ->  if x = var then y else type_var_contexte z var




(* ici pas besoin de donner un type on sait que c'est booleen *)
let verif_bool contexte terme = 
if terme = true || terme = false then true else false

(* esque les fonctions ont un effet de bord pour le contexte dans le cas ici présent car j'ai l'impréssion que il faut que celui ci se fasse remplir par les différentes fonctions  *)

let verif_abs contexte terme typ = 
match typ with 
| Nat -> failwith "Une abstraction est de type a*a"
| Bool -> failwith "Une abstraction est de type a*a"
| Fleche(x,y) -> 




 








						 


(*test pour la fonction relie libre *)

(* let x = Abs(FreeVar "0")
let () = Printf.printf "%s \n" (lambda_term_to_string(x))
let () = Printf.printf "%s \n" (lambda_term_to_string(relie_libre 0 x))
let y = Abs(Abs(Appl(FreeVar "1",Appl(FreeVar "0",BoundVar 0)))) *)

(*tests pour la fonction reduction_forte *)





