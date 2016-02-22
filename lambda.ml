open Sexplib

(*
  To load in the OCaml toplevel:
  
  #use "topfind";;
  #require "sexplib";;
 
*)
type typ = 
| Bool
| Nat 
| Fleche of typ * typ


type lambda_term =
  | FreeVar of (string * typ)
  | BoundVar of int
  | Abs of (lambda_term * typ)
  | Appl of (lambda_term * lambda_term * typ)
  | True | False | IfThenElse of (lambda_term * lambda_term * lambda_term * typ)
(*  | Zero | Suc of lambda_term | Iter of lambda_term * lambda_term * lambda_term *)


(* TODO: remember the name of the abstractions, for pretty-printing *)
(* TODO: rajouter constructeur des vrais ect... *)

(** * A simple parser *)

(* let rec parse env t 
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

let read t = parse [] (Sexp.of_string t) *)


(** * A simple printer *)

(* TODO: print S-expression instead. *)
let rec type_to_string typ = 
match typ with
| Bool -> "Bool"
| Nat -> "Nat" 
| Fleche (x,y) -> type_to_string x ^ " -> " ^ type_to_string y

let rec lambda_term_to_string t = 
  match t with
  | FreeVar (v,t) -> v
  | BoundVar v -> string_of_int v
  | Abs (x,t) -> "([]." ^ lambda_term_to_string x ^" :"  ^ type_to_string t ^ ")"
  | Appl (x,y,t) -> "(" ^ lambda_term_to_string x ^ " " ^ lambda_term_to_string y ^ "):" ^ type_to_string t
  | True -> "True"
  | False -> "False"
  | IfThenElse (x,y,z,t) -> "If " ^ lambda_term_to_string x ^ " Then " ^ lambda_term_to_string y ^ " Else " ^ lambda_term_to_string z ^ ": " ^ type_to_string t
  
let () = Printf.printf "%s \n" (lambda_term_to_string(Appl(Abs(BoundVar 0,Bool),True,Bool)))

(** * Reduction *)

let rec substitution t var tsub 
    = match t with 
    | FreeVar v -> FreeVar v 
    | BoundVar v when v = var -> tsub
    | BoundVar v -> BoundVar v
    | Abs (x,t) -> Abs(substitution x (var+1) tsub,t)
    | Appl (x,y,t) -> Appl(substitution x var tsub,substitution y var tsub,t)
    | True -> True
    | False -> False
    | IfThenElse (x,y,z,t) -> IfThenElse (x,y,z,t)
			       



(* XXX: Unnecessarily complex: it is enough to compare the raw terms *)
let alpha_equiv terme1 terme2 = 
  lambda_term_to_string terme1 = lambda_term_to_string terme2

let reduction t 
    = match t with
    | FreeVar v -> FreeVar v
    | BoundVar v -> BoundVar v
    | Abs x -> Abs(x)
    | Appl(Abs(x,t),y,t2) -> substitution x 0 y
    | Appl(x,y,t) -> failwith "erreur reduction"
    | True -> True
    | False -> False
    | IfThenElse(x,y,z,t) -> IfThenElse(x,y,z,t)


let rec evaluation terme 
    = match terme with 
    | FreeVar v -> FreeVar v 
    | BoundVar v -> BoundVar v 
    | Abs x -> Abs x
    | Appl(Abs(x),y,t) -> evaluation(reduction terme)
    | Appl(BoundVar x,y,t) -> Appl(BoundVar x,y,t)
    | Appl(FreeVar x,y,t) -> Appl(FreeVar x,y,t)
    | Appl(x,y,t) -> evaluation(Appl(evaluation x, y,t))
    | True -> True
    | False -> False
    | IfThenElse (x,y,z,t) when x = True -> y
    | IfThenElse (x,y,z,t) when x = False -> z
    | IfThenElse (x,y,z,t) -> reduction((IfThenElse ((reduction x), y, z,t)))

let () = Printf.printf "%s \n" (lambda_term_to_string(evaluation(IfThenElse(True,True,False,Bool))))

(* Fonctions de type checking *)
(* Le contexte est une liste de tuples de la forme [(var,type)] *)

let rec var_type_in_contexte contexte var t = 
  match contexte with 
  | [] -> failwith "N'est pas dans le contexte Ou type faux " 
  | (x,y)::z -> if x = var && y = t then true else var_type_in_contexte z var t

let contexte = [("x",Bool);("y",Nat);("w",(Fleche(Nat,Bool)))]
let () = Printf.printf "var_type_in_contexte %b \n" (var_type_in_contexte contexte "x" Bool )

(*ici le i sert pour garder l'index des variables libérées lors du check de l'abstraction *)
(* La derniere regle va correspondre a la règle B-syn afin d'executer une synthèse sur le terme *)

(*
(* Ici on ne demande pas un type a checker ni de contexte puisque l'on sait que c'est le type Bool *)
let check_bool term t = 
  match (term,t) with 
  | (True,Bool) -> true  
  | (False,Bool) -> true
  | (lambda_term,typ) -> failwith "Ce n'est pas un booleen erreur"
		       
		     

(* ici la variable binder par le lambda devient libre il faut donc trouver un moyen de garder l'info
dans le contexte sur cette variable *)
(* on va essayer une méthode en libérant la variable comme dans la reduction forte *)
let check_abs context term t i =
  match (term,t) with
  | (Abs(x,ty),(a,b)) -> check ((string_of_int i,a)::context) (substitution x 0 (FreeVar (string_of_int i))) b (i+1)
  | lambda_term -> failwith "Ce n'est pas une abstracion erreur ou alors le type n'est pas le bon" 

 *)

(* ici le problème il n'arrive pas a inférer le type de contexte il me semble *) 
let rec check context term t i =
  match term with
  | True -> check_bool term t
  | False -> check_bool term t
  | Abs(x,t) -> check_abs context term t i
  | lambda_term -> failwith "Cas pas encore traite"
  and check_abs context term t i =
    match (term,t) with
    | (Abs(x,ty),(a,b)) -> check ((string_of_int i,a)::context) (substitution x 0 (FreeVar (string_of_int i))) b (i+1)
    | lambda_term -> failwith "Ce n'est pas une abstracion erreur ou alors le type n'est pas le bon"
  and check_bool term t = 
    match (term,t) with 
    | (True,Bool) -> true  
    | (False,Bool) -> true
    | (lambda_term,typ) -> failwith "Ce n'est pas un booleen erreur"
			    
let () = Printf.printf "%b \n" (check_bool True) 
(*
let b_Abs context term t = 
match term t with 
| Abs(x,ty),(a,b) -> if 
| lambda_term,ty -> failwith "Ce n'est pas une abstraction"
 *)




				     
(*

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


 *)


