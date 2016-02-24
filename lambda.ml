(* open Sexplib *)


type typ = 
| Bool
| Nat 
| Fleche of typ * typ

type inTm = 
  | Abs of string * inTm
  | Inv of exTm
and exTm = 
  | Var of string
  | Appl of exTm * inTm

let rec exTm_to_string t = 
match t with
| Var x -> x 
| Appl(x,y) -> exTm_to_string x ^ " " ^ inTm_to_string y
and inTm_to_string t = 
match t with 
| Abs (x,y) -> "[]." ^ x ^ inTm_to_string y
| Inv x -> exTm_to_string x

let x = Abs("f",Abs("a",Inv(Appl(Var "f",Inv(Var "a")))))

let () = Printf.printf "%s \n" (inTm_to_string x)

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


(* i:inTm et t:typ e:exTm  *)
let rec check contexte i t = 
match i with
| Inv(Appl(x,y)) -> check contexte y (typ_gauche_Fleche(synth contexte x)) 
| Inv(Var x) -> if t = (synth contexte (Var x)) then true else false
| Abs(x,y) -> if (is_a_Fleche t) && check ((x,typ_gauche_Fleche t)::contexte) y (typ_droit_Fleche t) then true else false
and synth contexte e = 
match e with 
| Var x -> retourne_type contexte x 
| Appl(x,y) -> failwith "Cela ne devrait pas arriver"

let x = Abs("f",Abs("g",Inv(Appl(Var "f",Inv(Var "g")))))
let t = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))
let () = Printf.printf "resultat type check %b \n" (check [] x t)

