open Sexplib 


(* je garde la représentation que l'on avait de l'abstraction, avec la variable en string au début *)

type inTm = 
  | Abs of string * inTm
  | Inv of exTm
  | Pi of inTm * inTm 
  | Star
and exTm = 
  | Ann of inTm * inTm 
  | BVar of int 
  | FVar of string 
  | Appl of exTm * inTm

type value = 
  | VLam of (value -> value)
  | VNeutral of neutral 
  | VStar 
  | VPi of (value -> value)
and neutral = 
  | NFree of string 
  | NApp of neutral * value 
and env = Env of value list 


(* ici on va crée le parseur lisp avec le pretty printing *)


(* fonction de réduction ect *)


let vfree name = VNeutral(NFree name)





let gensym =
  let c = ref 0 in
  fun () -> incr c; "x" ^ string_of_int !c
(*
(* Premiere remarque, sur le t je ne vois pas comment on va faire pour *)
let rec check contexte inT ty = 
  match inT with 
  | Abs(x,y) -> 
     begin 
     match ty with      
       (* ici il faut faire la substitution dans y avec la freshVar *)
       | Pi(s,t) -> let freshVar = gensym () in
		    check ((freshVar,s)::contexte) y t
       | _ -> failwith "a trouver le message d'erreur" 
     end 
(* ici il va falloir faire une fonction de réduction avant de pouvoir type checker tout ça réellement *)
  |  Inv(t) -> 
      let tyT = synth contexte t in
      begin 
	tyT = ty
      end
  | Pi(s,t) when check contexte s Star -> let freshVar = gensym () in 
					  check ((freshVar,s)::contexte) t Star
  | Star -> 
     begin 
      match ty with 
	| Star -> true 
	| _ -> failwith "ty must be a Star"
     end
  | _ -> failwith "term not typable" 
and synth contexte exT =
  match exT with 
  | BVar *)


