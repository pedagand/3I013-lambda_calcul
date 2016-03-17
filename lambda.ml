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



let gensym =
  let c = ref 0 in
  fun () -> incr c; "x" ^ string_of_int !c

(* Premiere remarque, sur le t je ne vois pas comment on va faire pour 
let rec check contexte inT ty = 
(*  match inT with 
  | Abs(x,y) -> 
     begin 
     match ty with      
       | Pi(s,t) -> let freshVar = gensym () in
		    check ((freshVar,s)::contexte) y t
       | _ -> failwith "a trouver le message d'erreur" 
     end *)


