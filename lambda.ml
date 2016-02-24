open Sexplib


type typ = 
| Bool
| Nat 
| Fleche of typ * typ

type iN = 
  | Abs of string * iN
  | INv of ex
and ex = 
  | Var of string
  | Appl of ex * iN

let rec ex_to_string t = 
match t with
| Var x -> x 
| Appl(x,y) -> ex_to_string x ^ " " ^ iN_to_string y
and iN_to_string t = 
match t with 
| Abs (x,y) -> "[]." ^ x ^ iN_to_string y
| INv x -> ex_to_string x

let x = Abs("f",Abs("a",INv(Appl(Var "f",INv(Var "a")))))

let () = Printf.printf "%s \n" (iN_to_string x)

(* Fonctions préliminaires pour le type checking *)
let rec retourne_type contexte var = 
match contexte with 
| [] -> failwith "La variable n'est pas dans le contexte"
| (x,y)::z -> if x = var then y else retourne_type z var

let is_a_Fleche f = 
  match f with 
  | Nat -> false 
  | Bool -> false 
  | Fleche(x,y) -> true 

(* C'est moche mais je ne sais pas faire autrement  *)
let typ_gauche_Fleche t =
match t with 
| Nat -> failwith "Ce n'est pas une fleche" 
| Bool -> failwith "Ce n'est pas une fleche"
| Fleche(x,y) -> x

let typ_droit_Fleche t =
match t with 
| Nat -> failwith "Ce n'est pas une fleche" 
| Bool -> failwith "Ce n'est pas une fleche"
| Fleche(x,y) -> y


let x = Fleche(Bool,Nat)
let () = Printf.printf "Fleche %b \n" (is_a_Fleche Bool)


(* i:iN et t:typ e:ex  *)
let rec check contexte i t = 
match i with
| INv(Appl(x,y)) -> check contexte y (typ_gauche_Fleche(synth contexte x)) 
| INv(Var x) -> if t = (synth contexte (Var x)) then true else false
| Abs(x,y) -> if (is_a_Fleche t) && check ((x,typ_gauche_Fleche t)::contexte) y (typ_droit_Fleche t) then true else false
and synth contexte e = 
match e with 
| Var x -> retourne_type contexte x 
| Appl(x,y) -> failwith "Cela ne devrait pas arriver"

let x = Abs("f",Abs("g",INv(Appl(Var "f",INv(Var "g")))))
let t = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))
let () = Printf.printf "resultat type check %b \n" (check [] x t)

