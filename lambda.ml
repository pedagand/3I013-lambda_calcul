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
  | VPi of value * (value -> value)
and neutral = 
  | NFree of string 
  | NApp of neutral * value 
and env = Env of value list 


(* ici on va crée le parseur lisp avec le pretty printing *)
let rec parse_term env t = 
      match t with   
      | Sexp.List [Sexp.Atom "lambda"; Sexp.Atom var; body] -> 
	 Abs(var,(parse_term (var::env) body)) 
      | Sexp.List [Sexp.Atom "lambda"; Sexp.List vars ; body] -> 
	 let vars = List.map (function 
			       | Sexp.Atom v -> v
			       | _ -> failwith "Parser: invalid variable") vars
	 in 
	 List.fold_right 
           (fun var b -> Abs(var,b))
           vars
           (parse_term (List.append (List.rev vars) env) body)      
      | Sexp.List [Sexp.Atom "Pi"; s ; t] -> 
	 Pi((parse_term env s),(parse_term env t))
      | Sexp.Atom "*" -> Star      
      | _ -> Inv(parse_exTm env t)
and parse_exTm env t = 
  let rec lookup_var env n v
    = match env with
    | [] -> FVar v
        | w :: env when v = w -> BVar n
        | _ :: env -> lookup_var env (n+1) v 
  in
  match t with 
  | Sexp.List [Sexp.Atom ":" ;x; t] -> 
     Ann((parse_term env x),(parse_term env t))
  | Sexp.Atom v -> lookup_var env 0 v 
  | Sexp.List (f::args) -> 
     List.fold_left 
       (fun x y -> Appl(x, y))
       (parse_exTm env f)
       (List.map (parse_term env) args)
  | _ -> failwith "erreur de parsing" 





(* fonction de substitution et de réduction ect *)
(* cette fonction est "normalement bonne" *)
let rec substitution_inTm t tsub var = 
  match t with 
  | Inv x -> Inv(substitution_exTm x tsub var)
  | Abs(x,y) -> Abs(x,(substitution_inTm y tsub (var+1)))
  | Star -> Star
  | Pi(x,y) -> Pi ((substitution_inTm x tsub var),(substitution_inTm x tsub (var+1)))
and substitution_exTm  t tsub var = 
  match t with 
  | FVar x -> FVar x
  | BVar x when x = var -> tsub
  | BVar x -> BVar x
  | Appl(x,y) -> Appl((substitution_exTm x tsub var),(substitution_inTm y tsub var))
  | Ann(x,y) -> Ann((substitution_inTm x tsub var),(substitution_inTm y tsub var))



let vfree name = VNeutral(NFree name)


let rec big_step_eval_exTm t envi = 
  match t with
    | Ann(x,_) -> big_step_eval_inTm x envi
    | FVar v -> vfree v 
    | BVar v -> List.nth envi v
    | Appl(x,y) -> vapp((big_step_eval_exTm x envi),(big_step_eval_inTm y envi))
and vapp v = 
  match v with 
  | ((VLam f),v) -> f v
  | ((VNeutral n),v) -> VNeutral(NApp(n,v))
  | _ -> failwith "TBD"
and big_step_eval_inTm t envi = 
  match t with 
  | Inv(i) -> big_step_eval_exTm i envi
  | Abs(x,y) -> VLam(function arg -> (big_step_eval_inTm y (arg :: envi)))
  | Star -> VStar
  | Pi (x,y) -> VPi ((big_step_eval_inTm x envi),(function arg -> (big_step_eval_inTm y (arg :: envi))))


let read t = parse_term [] (Sexp.of_string t)


let rec value_to_inTm i v =
  match v with 
  | VLam(f) -> Abs((string_of_int(i)),(value_to_inTm (i+1) (f(vfree(string_of_int (-i))))))
  | VNeutral(x) -> Inv(neutral_to_exTm i x)
  | VStar -> Star
  | VPi(x,f) -> Pi((value_to_inTm i x),(value_to_inTm (i+1) (f(vfree(string_of_int (-i))))))
and neutral_to_exTm i v = 
  match v with 
  | NFree x -> let k = int_of_string x in
	       if k <= 0 then BVar(i + k - 1)
	       else FVar x
  | NApp(n,x) -> Appl((neutral_to_exTm i n),(value_to_inTm i x))

	 
let gensym =
  let c = ref 0 in
  fun () -> incr c; "x" ^ string_of_int !c

(* Premiere remarque, sur le t je ne vois pas comment on va faire pour *)
let rec check contexte inT ty = 
  match inT with 
  | Abs(x,y) -> 
     begin 
     match ty with      
       (* ici il faut faire la substitution dans y avec la freshVar *)
       | Pi(s,t) -> let freshVar = gensym () in
		    check ((freshVar,s)::contexte) (substitution_inTm y (FVar(freshVar)) 0) t
       | _ -> failwith "a trouver le message d'erreur" 
     end 
(* ici il va falloir faire une fonction de réduction avant de pouvoir type checker tout ça réellement il faut réduire ty et tyT  *)
  |  Inv(t) -> 
      let tyT = synth contexte t in
      begin 	
	tyT = ty
      end
  | Pi(s,t) when check contexte s Star -> let freshVar = gensym () in 
					  check ((freshVar,s)::contexte) (substitution_inTm t (FVar(freshVar)) 0) Star
  | Star -> 
     begin 
      match ty with 
	| Star -> true 
	| _ -> failwith "ty must be a Star"
     end
  | _ -> failwith "term not typable" 
and synth contexte exT =
  match exT with 
  | BVar x -> failwith "Pas possible de trouver une boundVar a synthétiser"
  | FVar x -> List.assoc x contexte
  | Ann(tm, ty) ->
       if check contexte ty Star &&  check contexte tm ty then 
         ty 
       else
         failwith "Wrong annotation"
  | Appl(x,y) -> 
     let pTy = synth contexte x in 
     begin 
       match pTy with 
       | Pi(s,t) -> if check contexte y s 
		    then (substitution_inTm t (Ann(y,s)) 0)
		    else failwith "mauvais type d'argument pour l'application"
       | _ -> failwith "Mauvais annotation" 				  
     end




