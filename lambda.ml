(* open Sexplib *)


type typ = 
| Bool
| Nat 
| Fleche of typ * typ

type inTm = 
  | Abs of string * inTm
  | True | False 
  | Inv of exTm
  | Zero 
  | Succ of inTm
(* XXX: You've forgotten the iterator for natural numbers *)
and exTm = 
  | FVar of string
  | BVar of int
  | Appl of exTm * inTm
  | Ifte of inTm * exTm * exTm
  | Ann of inTm * typ

type lambda_term =
  | SFVar of string 
  | SBVar of int 
  | SAbs of lambda_term
  | SAppl of (lambda_term * lambda_term)
  | STrue | SFalse | SIfte of lambda_term * lambda_term * lambda_term
  | SZero | SSucc of lambda_term
      

(* XXX: Implement alpha-equivalence/equality of [inTm] and [exTm] *)
(* test: alphaEq (lambda x x) (lambda y y) = true *)

let rec typ_to_string t = 
  match t with 
  | Bool -> "B"
  | Nat -> "N"
  | Fleche(x,y) -> typ_to_string x ^ "->" ^ typ_to_string y


(* on va faire un truc moche mais pour allez vite en attendant de trouver un truc plus élégant *)
(* ça passe c'est quand meme assez élégant je trouve *)
let rec findVar x l = 
  (* XXX: use [nth] instead of reimpleminting it here *)
  match (x,l) with 
  | (x,[]) -> failwith "Var pas dans la liste" 
  | (0,y::z) -> y 
  | (x,y::z) -> findVar (x-1) z

(* XXX: resurrect the Lisp parser *)
(* XXX: pretty print to the Lisp syntax *)
(* t un terme et l une liste de nom de variable générer avec les binder *)
let rec exTm_to_string t l = 
match t with
| FVar x -> x 
| BVar x -> findVar x l	      
| Appl(x,y) -> exTm_to_string x l ^ " " ^ inTm_to_string y l 
| Ann(x,y) -> inTm_to_string x l ^ ":" ^ typ_to_string y
| Ifte(x,y,z) -> "if " ^ inTm_to_string x l ^ " then " ^ exTm_to_string y l ^ " else " ^ exTm_to_string z l
and inTm_to_string t l = 
match t with 
| Abs (x,y) -> "([]" ^ x  ^ "." ^ inTm_to_string y (x::l) ^ ")"
| Inv x -> exTm_to_string x l
| True -> "True"
| False -> "False"
| Zero -> "Zero"
| Succ x -> "Succ(" ^ inTm_to_string x [] ^ ")"

let rec lambda_term_to_string t = 
  match t with
  | SFVar v -> v
  | SBVar v -> string_of_int v        
  | SAbs x -> "[]." ^ lambda_term_to_string x 
  | SAppl (x,y) -> "(" ^ lambda_term_to_string x ^ " " ^ lambda_term_to_string y ^ ")"
  | STrue -> "True"
  | SFalse -> "False"
  | SIfte (x,y,z) -> "if " ^ lambda_term_to_string x ^ " then " ^ lambda_term_to_string y ^ " else " ^ lambda_term_to_string z 
  | SZero -> "Zero"
  | SSucc x -> "Succ( " ^ lambda_term_to_string x ^ ")" 


(* XXX: turn into unit test *)
let x = Abs("f",Abs("a",Inv(Appl(BVar 1,Inv(BVar 0)))))
let () = Printf.printf "%s \n" (inTm_to_string x [])

let rec substitution_inTm t tsub var = 
  match t with 
  | Inv x -> Inv(substitution_exTm x tsub var)
  | Abs(x,y) -> Abs(x,(substitution_inTm y tsub (var+1)))
  | True -> True
  | False -> False 
  | Zero -> Zero
  | Succ x -> Succ x
and substitution_exTm  t tsub var = 
  match t with 
  | FVar x -> FVar x
  | BVar x when x = var -> tsub
  | BVar x -> BVar x
  | Appl(x,y) -> Appl((substitution_exTm x tsub var),(substitution_inTm y tsub var))
  | Ann(x,y) -> Ann((substitution_inTm x tsub var),y)
  | Ifte(x,y,z) -> Ifte(x,y,z)


let rec substitution t var tsub 
    = match t with 
    | SFVar v -> SFVar v 
    | SBVar v when v = var -> tsub
    | SBVar v -> SBVar v
    | SAbs x -> SAbs(substitution x (var+1) tsub)
    | SAppl (x,y) -> SAppl(substitution x var tsub,substitution y var tsub)
    | STrue -> STrue
    | SFalse -> SFalse
    | SIfte (x,y,z) -> 
       (* XXX: the substitution must go through [SIfte] too! *)
       SIfte (x,y,z)
    | SZero -> SZero
    | SSucc x -> 
       (* XXX: the substitution must go through [SSucc] too! *)
       SSucc x

let rec relie_libre i bv t =
  match t with 
  | SBVar v -> SBVar v
  | SFVar  v when v = string_of_int i -> SBVar bv
  | SFVar  v -> SFVar  v
  | SAbs(x) -> SAbs(relie_libre i (bv + 1) x)
  | SAppl(x,y) -> SAppl(relie_libre i bv x,relie_libre i bv y)
  | STrue-> STrue
  | SFalse -> SFalse
  | SIfte(x,y,z) -> 
     (* XXX: must go through [SIfte] too! *)
     SIfte(x,y,z)
  | SZero -> SZero
  | SSucc x -> 
     (* XXX: must go through [SSucc] too! *)
     SSucc x
	

(* XXX: not verified, run (many) tests first *)
let rec reduction_forte t i  = 
  match t with 
    | SFVar  v -> SFVar  v
    | SBVar v -> SBVar v
    | SAbs x -> SAbs(relie_libre i 0 (reduction_forte (substitution x 0 (SFVar  (string_of_int i))) (i+1)))
    | SAppl(SAbs(x),y) -> reduction_forte(substitution x 0 y) i
    | SAppl(x,y) -> 
       begin 
	 match reduction_forte x i with 
	 | SFVar  z -> SAppl(x,(reduction_forte y i))
	 | SAbs z -> reduction_forte (SAppl(SAbs(z),y)) i 
	 | neutre -> SAppl(neutre,reduction_forte y i)				   
       end 
    | STrue -> STrue
    | SFalse -> SFalse
    | SIfte (x,y,z) when x = STrue -> reduction_forte y i
    | SIfte (x,y,z) when x = SFalse -> reduction_forte z i
    | SIfte (x,y,z) -> 
       begin 
	 match reduction_forte x i with
	 | STrue-> reduction_forte y i
	 | SFalse -> reduction_forte z i
	 | _ -> SIfte(x,y,z)
       end  
    | SZero -> SZero
    | SSucc x -> SSucc x 

(* fonction d'iteration d'une fonction n fois *)
let rec iter n f a = 
  match n with
    | Zero -> a
    | Succ(t) -> iter t f (reduction_forte (SAppl(f,a)) 0)
    | _ -> 
       (* XXX: hehe, careful here: you could be running against 
           [(lambda x (ifte (S x) not false))]
          which should reduce to
           [(lambda x (ifte x not (not false)))]
          ending with
           [(lambda x (ifte x not true))] *)
       failwith "first arg must be a Nat" 
	       
let rec typed_to_simple_inTm t = 
  match t with 
    | Abs(x,y) -> SAbs (typed_to_simple_inTm y)
    | Inv(x) -> typed_to_simple_exTm x
    | True-> STrue
    | False -> SFalse
    | Zero -> SZero
    | Succ x -> SSucc (typed_to_simple_inTm x)
and typed_to_simple_exTm t = 
  match t with 
    | BVar x -> SBVar x 
    | FVar x -> SFVar x
    | Appl(x,y) -> SAppl((typed_to_simple_exTm x),(typed_to_simple_inTm y))
    | Ann(x,y) -> typed_to_simple_inTm x 
    | Ifte(x,y,z) -> SIfte((typed_to_simple_inTm x),(typed_to_simple_exTm y),(typed_to_simple_exTm z))

let y = Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))),(Abs("y",(Inv(BVar 0))))))
let () = Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_inTm x));
	 Printf.printf "%s \n" (inTm_to_string y []);
	 Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_inTm y))

let () = Printf.printf "C'est moche \n \n" 

let y = Appl((Ann(x,(Fleche(Bool,Fleche(Bool,Bool))))),Inv(FVar "k"))
(* let() = Printf.printf "%s \n" (inTm_to_string(substitution_inTm x (FVar "w") 0) [])
let() = Printf.printf "%s \n" (inTm_to_string(substitution_inTm x (Ann(SAbs("y",Inv(BVar 0)),Fleche(Bool,Bool))) 0) [] ) *)
let () = Printf.printf "C'est moche \n \n" 
let () = Printf.printf "%s \n" (exTm_to_string y [])  
let () = Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_exTm y))
  

let gensym =
  let c = ref 0 in
  fun () -> incr c; "x" ^ string_of_int !c

(* i:inTm et t:typ e:exTm  *)
(* ici le compt doit etre supérieur a toutes les variables liées déja présentes *)

let rec check contexte inT ty
    = match inT with
    | Abs(x, b) -> 
       begin
         match ty with
         | Fleche(s, t) -> 
            (* XXX: open the de Bruijn binder *)
            let freshVar = gensym () in
            check ((freshVar, s) :: contexte) (substitution_inTm b (FVar freshVar) 0) t
         | _ -> failwith "SAbstraction forced into a non-functional type"
       end
    | Inv(t) -> 
       let tyT = synth contexte t in
       begin 
       tyT = ty
       end 
    | True -> if ty = Bool then true else false
    | False -> if ty = Bool then true else false
    | Zero -> if ty = Nat then true else false 
    | Succ x -> if ty = Nat then check contexte x Nat else false
and synth contexte exT 
    = match exT with
    | Ann(tm, ty) ->
       if check contexte tm ty then
         ty 
       else
         failwith "Wrong annotation"
    | FVar(x) -> List.assoc x contexte
    | BVar x -> failwith "Bvar is not possible at this moment"
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
    | Ifte(x,y,z) -> if check contexte x Bool then 
		       begin 
			 let ttrue = synth contexte y in 
			 let tfalse = synth contexte z in 
			 if ttrue = tfalse then ttrue 
			 else failwith "Ifte type of argument not the same"
		       end 
		     else failwith "Ifte first param need to be a bool"
		       
					 
		     
 
(* XXX: turn into unit tests *)
(* [https://en.wikipedia.org/wiki/B,_C,_K,_W_system] *)
let x = Abs("f",Abs("g",Inv(Appl(BVar 1,Inv(BVar 0)))))
let t = Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))
let y = Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))),(Abs("y",(Inv(BVar 0))))))
let u = Fleche(Bool,Bool)

let () = 
  Printf.printf "truc a checker %s \n" (inTm_to_string x []);
  Printf.printf "resultat type check %b \n" (check [] x t);
  Printf.printf "truc a checker %s \n" (inTm_to_string y []);
  Printf.printf "resultat type check %b \n" (check [] y u);
  Printf.printf "reduction %s \n" (lambda_term_to_string(reduction_forte (typed_to_simple_inTm y) 0))


let n = Succ(Succ(Succ(Zero)))
let () = 
  Printf.printf "\n test sur les bool \n";
  Printf.printf "%s \n" (inTm_to_string n []);
  Printf.printf "resultat type check %b \n" (check [] n Nat)
 

let zero = SAbs(SAbs(SBVar 0))
let succ = SAbs(SAbs(SAbs(SAppl(SBVar 1,SAppl(SAppl(SBVar 2,SBVar 1),SBVar 0)))))
let () = Printf.printf "\n test iter \n";
	 Printf.printf "%s \n" (lambda_term_to_string(zero));
	 Printf.printf "%s \n" (lambda_term_to_string(iter n succ zero))
