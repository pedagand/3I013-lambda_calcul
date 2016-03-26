open Sexplib 


(* je garde la représentation que l'on avait de l'abstraction, avec la variable en string au début *)

type inTm = 
  | Abs of string * inTm
  | Inv of exTm
  | Pi of string * inTm * inTm 
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


(* ici on va crée le parseur lisp avec le pretty printing *)
let rec parse_term env t = 
      match t with   
      | Sexp.List [Sexp.Atom "lambda"; Sexp.Atom var; body] -> 
	 Abs(var,(parse_term (var::env) body)) 
      | Sexp.List [Sexp.Atom "lambda"; Sexp.List vars ; body] -> 
	 let vars = List.map (function 
			       | Sexp.Atom v -> v
			       | _ -> failwith "Parser: Lambdainvalid variable") vars
	 in 
	 List.fold_right 
           (fun var b -> Abs(var,b))
           vars
           (parse_term (List.append (List.rev vars) env) body)      
      | Sexp.List [Sexp.Atom "->"; s ; t ] -> 
	 Pi("NO",(parse_term env s),(parse_term env t))
      | Sexp.List [Sexp.Atom "pi"; Sexp.Atom var ; s ; t] -> 
	 Pi(var,(parse_term env s),(parse_term (var::env) t))        
      | Sexp.List [Sexp.Atom "pi";Sexp.List vars; s; t] -> 
	 let vars = List.map (function 
			       | Sexp.Atom v -> v
			       | _ -> failwith "Parser pi invalide variable") vars 
	 in 
	 List.fold_right
	   (fun var b -> Pi(var,(parse_term (List.append (List.rev vars) env) s),b))
	   vars 
	   (parse_term (List.append (List.rev vars) env) t)
      | Sexp.Atom "*!" -> Star      
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

let rec pretty_print_inTm t l = 
  match t with 
  | Abs(str,x) -> "(lambda " ^ str ^ " " ^ pretty_print_inTm x (str :: l) ^ ")"
  | Inv (x) ->  pretty_print_exTm x l
  | Pi (str,s,t) -> "(pi " ^ str ^ " " ^ pretty_print_inTm s l ^ " " ^ pretty_print_inTm t (str :: l) ^ ")"
  | Star -> "*!"
and pretty_print_exTm t l =
  match t with 
  | Ann(x,y) ->  "(: " ^ pretty_print_inTm x l ^ " " ^ pretty_print_inTm y l ^ ")"
  | BVar(x) -> List.nth l x 
  | FVar (x) -> x
  | Appl(x,y) -> "(" ^ pretty_print_exTm x l ^ " " ^ pretty_print_inTm y l ^ ")"

    
      




(* fonction de substitution et de réduction ect *)
(* cette fonction est "normalement bonne" *)
let rec substitution_inTm t tsub var = 
  match t with 
  | Inv x -> Inv(substitution_exTm x tsub var)
  | Abs(x,y) -> Abs(x,(substitution_inTm y tsub (var+1)))
  | Star -> Star
  | Pi(v,x,y) -> Pi(v,(substitution_inTm x tsub var),(substitution_inTm y tsub (var+1)))
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
  | Pi (v,x,y) -> VPi ((big_step_eval_inTm x envi),(function arg -> (big_step_eval_inTm y (arg :: envi))))


(* il me semble qu'il me faut une fonction de relie libre avant de lancer big step eval dans le check pour que celui ci puisse faire le travail 
le contexte que l'on va utiliser est de la forme ("nom var",inTm)*)
let rec relie_free_context_inTm  contexte t = 
  match t with 
  | Abs(x,y) -> Abs(x,relie_free_context_inTm contexte y)
  | Pi (v,s,z) -> Pi(v,relie_free_context_inTm contexte s,relie_free_context_inTm contexte z)
  | Star -> Star 
  | Inv(Ann(x,y)) -> Inv(Ann(relie_free_context_inTm contexte x,relie_free_context_inTm contexte y))
  | Inv(BVar(v)) -> Inv(BVar(v))
  | Inv(FVar (v)) -> List.assoc v contexte
  | Inv(Appl (x,y)) -> Inv(Appl(Ann((relie_free_context_inTm contexte (Inv(x))),Star),relie_free_context_inTm contexte y))



let read t = parse_term [] (Sexp.of_string t)

let gensym =
  let c = ref 0 in
  fun () -> incr c; "x" ^ string_of_int !c

let rec value_to_inTm i v =
  match v with 
  | VLam(f) -> Abs((string_of_int(i)),(value_to_inTm (i+1) (f(vfree(string_of_int (-i))))))
  | VNeutral(x) -> Inv(neutral_to_exTm i x)
  | VStar -> Star
  | VPi(x,f) -> let var = gensym () in 
		begin
		Pi(var,(value_to_inTm i x),(value_to_inTm (i+1) (f(vfree(string_of_int (-i))))))
		end
and neutral_to_exTm i v = 
  match v with 
  | NFree x -> let k = int_of_string x in
	       if k <= 0 then BVar(i + k - 1)
	       else FVar x
  | NApp(n,x) -> Appl((neutral_to_exTm i n),(value_to_inTm i x))

	
(* fonctions pour le debug *)
let rec contexte_to_string contexte l= 
  match contexte with 
  | [] -> "|" 	    
  | (s,w) :: tail -> "(" ^ s ^ "," ^ pretty_print_inTm w l ^ ");" ^ contexte_to_string tail l  


(* ^ idée faire un moyen de backtraquer avec une liste en argument ou a chaque fois que on effectue une opération il faut la mettre dans cette liste *) 
(* let rec check contexte inT ty debug ldebug= 
  match inT with 
  | Abs(x,y) -> 
     begin 
     match ty with      
     | Pi(v,s,t) -> let freshVar = gensym () in
		    check ((freshVar,s)::contexte) (substitution_inTm y (FVar(freshVar)) 0) t ((pretty_print_inTm (Abs(x,y)) (freshVar :: ldebug )) ^ ";" ^ debug)  (freshVar :: ldebug )
     | _ -> failwith ("Abs must be of type " ^ debug  )
     end 
  | Inv(t) -> 
     let tyT = synth contexte t ((pretty_print_inTm (Inv(t)) ldebug) ^ ";" ^ debug) ldebug in
     begin 	
       (big_step_eval_inTm tyT []) = (big_step_eval_inTm ty [])
     end
  | Pi(v,s,t) when (check contexte s Star ((pretty_print_inTm (Pi(v,s,t)) ldebug) ^ ";" ^ debug) ldebug) ->
     let freshVar = gensym () in 
     begin 
     check ((freshVar,s)::contexte) (substitution_inTm t (FVar(freshVar)) 0) Star ((pretty_print_inTm (Pi(v,s,(substitution_inTm t (FVar(freshVar)) 0))) (freshVar :: ldebug)) ^ ";" ^ debug) (freshVar :: ldebug)
     end
  | Pi(v,s,t) -> failwith ("Pi s must be of type star !!" ^ pretty_print_inTm inT ldebug ^ "!! contexte: " ^ contexte_to_string contexte "" [])
  | Star -> 
     begin 
      match ty with 
	| Star -> true 
	| _ -> failwith ("ty must be a Star" ^ debug)
     end
 (*  | _ -> failwith ("term not typable !!" ^ pretty_print_inTm inT ldebug ^ "!!"  ^ debug ) *)
and synth contexte exT debug ldebug =
  match exT with 
  | BVar x -> failwith ("Pas possible de trouver une boundVar a synthétiser " ^ debug) 
  | FVar x -> List.assoc x contexte
  | Ann(tm, ty) ->
       if check contexte ty Star ((pretty_print_exTm (Ann(tm,ty)) ldebug) ^ ";" ^ debug) ldebug 
	  &&  check contexte tm ty ((pretty_print_exTm (Ann(tm,ty)) ldebug) ^ ";" ^ debug) ldebug then 
         ty 
       else
         failwith ("Wrong annotation" ^ debug)
  | Appl(x,y) -> 
     let pTy = synth contexte x ((pretty_print_exTm (Appl(x,y)) ldebug) ^ ";" ^ debug) ldebug in 
     begin 
       match pTy with 
       | Pi(v,s,t) -> if check contexte y s ((pretty_print_exTm (Appl(x,y)) ldebug) ^ ";" ^ debug) ldebug
		    then (substitution_inTm t (Ann(y,s)) 0)
		    else failwith ("mauvais type d'argument pour l'application" ^ debug)
       | _ -> failwith ("Mauvais annotation" ^ debug)
     end
 *)
										       
let rec check contexte inT ty debug ldebug= 
  match inT with 
  | Abs(x,y) -> 
     begin 
     match ty with      
     | Pi(v,s,t) -> let freshVar = gensym () in
		    check ((freshVar,s)::contexte) (substitution_inTm y (FVar(freshVar)) 0) t ""  (freshVar :: ldebug)
     | _ -> failwith ("Abs must be of type " ^ debug  )
     end 
  | Inv(t) -> 
     let tyT = (synth contexte t "" ldebug) in
     begin       
     (big_step_eval_inTm (relie_free_context_inTm contexte tyT) []) = (big_step_eval_inTm (relie_free_context_inTm contexte ty) [])
     end
  | Pi(v,s,t) ->
     (* nouveau problème dans le type checker si s est une variable libre et que on veut tester si c'est une star ça ne vas pas marcher si c'est un pi *)
     let freshVar = gensym () in 
     begin 
       if  (check contexte  s Star "" ldebug) then 
	 check ((freshVar,s)::contexte) (substitution_inTm t (FVar(freshVar)) 0) Star "" (freshVar :: ldebug)
       else failwith ("Pi s must be of type star !!"^ pretty_print_inTm s [] ^ "!! contexte !!"  ^ contexte_to_string contexte  [])
     end
  | Star -> 
     begin 
      match ty with 
	| Star -> true
	| _ -> failwith ("ty must be a Star" ^ debug)
     end
and synth contexte exT debug ldebug =
  match exT with 
  | BVar x -> failwith ("Pas possible de trouver une boundVar a synthétiser " ^ "!! contexte: " ^ contexte_to_string contexte [])
  | FVar x -> read(pretty_print_inTm(List.assoc x contexte) [] )
  | Ann(tm, ty) ->
       if check contexte ty Star "" ldebug 
	  &&  check contexte tm ty "" ldebug then 
         ty 
       else
         failwith ("Wrong annotation" ^ debug)
  | Appl(x,y) -> 
     let pTy = synth contexte x "" ldebug in 
     begin 
       match pTy with 
       | Pi(v,s,t) -> if (check contexte y s "" ldebug)
		    then (substitution_inTm t (Ann(y,s)) 0)
		    else failwith ("mauvais type d'argument pour l'application" ^ "Appl" ^ pretty_print_exTm (Appl(x,y)) [] ^ " pty :  " ^     pretty_print_inTm pTy [] ^ "contexte !!!" ^  contexte_to_string contexte [])
       | _ -> failwith ("Mauvais annotation" ^ debug)
     end




