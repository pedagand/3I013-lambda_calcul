(* open Sexplib *)


type typ = 
| Bool
| Nat 
| Fleche of typ * typ

(* XXX: Use [FVar] and [BVar], parse terms as [BVar] *)
type inTm = 
  | Abs of string * inTm
  | Inv of exTm
and exTm = 
  | FVar of string
  | BVar of int
  | Appl of exTm * inTm
  | Ann of inTm * typ

(* XXX: Implement alpha-equivalence/equality of [inTm] and [exTm] *)
(* test: alphaEq (lambda x x) (lambda y y) = true *)

let rec typ_to_string t = 
  match t with 
  | Bool -> "B"
  | Nat -> "N"
  | Fleche(x,y) -> typ_to_string x ^ "->" ^ typ_to_string y

let rec exTm_to_string t = 
match t with
| FVar x -> x 
| BVar x -> string_of_int x
| Appl(x,y) -> exTm_to_string x ^ " " ^ inTm_to_string y
| Ann(x,y) -> inTm_to_string x ^ ":" ^ typ_to_string y
and inTm_to_string t = 
match t with 
| Abs (x,y) -> "([]" ^ x  ^ "." ^ inTm_to_string y ^ ")"
| Inv x -> exTm_to_string x


(* XXX: turn into unit test *)
let x = Abs("f",Abs("a",Inv(Appl(BVar 1,Inv(BVar 0)))))
let () = Printf.printf "%s \n" (inTm_to_string x)

(* XXX: re-implement substitution and evaluation *)
let rec substitution_inTm t tsub var = 
  match t with 
  | Inv x -> Inv(substitution_exTm x tsub var)
  | Abs(x,y) -> Abs(x,(substitution_inTm y tsub (var+1)))
and substitution_exTm  t tsub var = 
  match t with 
  | FVar x -> FVar x
  | BVar x -> if x = var then tsub else BVar x
  | Appl(x,y) -> Appl((substitution_exTm x tsub var),(substitution_inTm y tsub var))
  | Ann(x,y) -> Ann((substitution_inTm x tsub var),y)


let rec reduction_inTm t = 
  match t with 
    | Inv x -> Inv(reduction_exTm x)
    | Abs(x,y) -> Abs(x,reduction_inTm y)
and reduction_exTm t = 
  match t with 
    | FVar x -> FVar x
    | BVar x -> BVar x 
    | Appl(Ann(Abs(s,x),Fleche(ty,y)),Inv z) -> Ann((substitution_inTm x z 0),y)
    | Appl(x,y) -> Appl(x,y)
    | Ann(x,y) -> Ann(x,y)

let y = Appl((Ann(x,(Fleche(Bool,Fleche(Bool,Bool))))),Inv(FVar "k"))
let() = Printf.printf "%s \n" (inTm_to_string(substitution_inTm x (FVar "w") 0))
let() = Printf.printf "%s \n\n" (inTm_to_string(substitution_inTm x (Ann(Abs("y",Inv(BVar 0)),Fleche(Bool,Bool))) 0))
let () = Printf.printf "%s \n" (exTm_to_string y)  
let () = Printf.printf "%s \n" (exTm_to_string(reduction_exTm y))  

(* i:inTm et t:typ e:exTm  *)
(* ici le compt doit etre supérieur a toutes les variables liées déja présentes *)
let rec check contexte inT ty compt
    = match inT with
    | Abs(x, b) -> 
       begin
         match ty with
         | Fleche(s, t) -> 
            (* XXX: open the de Bruijn binder *)
            check (((string_of_int compt), s) :: contexte) (substitution_inTm b (FVar(string_of_int compt)) 0) t (compt + 1)
         | _ -> failwith "Abstraction forced into a non-functional type"
       end
    | Inv(t) -> 
       let tyT = synth contexte t compt in
       tyT = ty
and synth contexte exT compt
    = match exT with
    | Ann(tm, ty) ->
       if check contexte tm ty compt then
         ty 
       else
         failwith "Wrong annotation"
    | FVar(x) -> List.assoc x contexte
    | BVar x -> failwith "Bvar is not possible at this moment"
    | Appl(f, s) -> 
       let fTy = synth contexte f compt in
       begin
         match fTy with
         | Fleche(a, b) -> 
            if check contexte s a compt then
              b
            else 
              failwith "Argument type invalid"
         | _ -> failwith "Function type invalid"
       end

(* XXX: turn into unit tests *)
(* [https://en.wikipedia.org/wiki/B,_C,_K,_W_system] *)
let x = Abs("f",Abs("g",Inv(Appl(BVar 1,Inv(BVar 0)))))
let t = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))
let y = Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))),(Abs("y",(Inv(BVar 0))))))
let u = Fleche(Bool,Bool)

let () = 
  Printf.printf "truc a checker %s \n" (inTm_to_string x);
  Printf.printf "resultat type check %b \n" (check [] x t 2);
  Printf.printf "truc a checker %s \n" (inTm_to_string y);
  Printf.printf "resultat type check %b \n" (check [] y u 2);

