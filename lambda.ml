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
  | Var of string
  | Appl of exTm * inTm
  | Ann of inTm * typ

(* XXX: Implement alpha-equivalence/equality of [inTm] and [exTm] *)
(* test: alphaEq (lambda x x) (lambda y y) = true *)

let rec exTm_to_string t = 
match t with
| Var x -> x 
| Appl(x,y) -> exTm_to_string x ^ " " ^ inTm_to_string y
| _ -> (* XXX: *) failwith "TBD"
and inTm_to_string t = 
match t with 
| Abs (x,y) -> "[]." ^ x ^ inTm_to_string y
| Inv x -> exTm_to_string x

(* XXX: turn into unit test *)
let x = Abs("f",Abs("a",Inv(Appl(Var "f",Inv(Var "a")))))
let () = Printf.printf "%s \n" (inTm_to_string x)

(* XXX: re-implement substitution and evaluation *)

(* i:inTm et t:typ e:exTm  *)
let rec check contexte inT ty 
    = match inT with
    | Abs(x, b) -> 
       begin
         match ty with
         | Fleche(s, t) -> 
            (* XXX: open the de Bruijn binder *)
            check ((x, s) :: contexte) b t
         | _ -> failwith "Abstraction forced into a non-functional type"
       end
    | Inv(t) -> 
       let tyT = synth contexte t in
       tyT = ty
and synth contexte exT 
    = match exT with
    | Ann(tm, ty) ->
       if check contexte tm ty then
         ty 
       else
         failwith "Wrong annotation"
    | Var(x) -> List.assoc x contexte
    | Appl(f, s) -> 
       let fTy = synth contexte f in
       begin
         match fTy with
         | Fleche(a, b) -> 
            if check contexte s a then
              b
            else 
              failwith "Argument type invalid"
         | _ -> failwith "Function type invalid"
       end

(* XXX: turn into unit tests *)
(* [https://en.wikipedia.org/wiki/B,_C,_K,_W_system] *)
let x = Abs("f",Abs("g",Inv(Appl(Var "f",Inv(Var "g")))))
let t = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))
let y = Abs("f",Abs("g",Inv(Appl(Var "g",Inv(Var "f")))))
let u = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))

let () = 
  Printf.printf "resultat type check %b \n" (check [] x t);
  Printf.printf "resultat type check %b \n" (check [] y u);

