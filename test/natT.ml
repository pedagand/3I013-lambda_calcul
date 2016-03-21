open OUnit2

open Lambda
open Nat

let testsucc = Appl(Ann(succ,(Fleche (Nat,(Fleche (Nat,(Fleche (Nat,Nat))))))),(value_to_inTm 0 (int_to_value 0)))
(*let succ_test = Abs(Abs(Abs(Appl(BoundVar 1,Appl(Appl(BoundVar 2,BoundVar 1),BoundVar 0)))))
let plus_test = Appl(Appl(plus,(int_to_lambda_term 2)),(int_to_lambda_term 2)) *)

let () = Printf.printf "test eval derniere chance \n";
	 Printf.printf "%s \n" (inTm_to_string (value_to_inTm 0 ( (big_step_eval_exTm testsucc [] )))[]);
	 Printf.printf "%s \n" (inTm_to_string (value_to_inTm 0 (int_to_value 1)) [])

let test1 test_ctxt = assert_equal (BVar 0) (BVar 0)
let test2 test_ctxt = assert_equal (big_step_eval_exTm testsucc [] ) (int_to_value 1)

(* poser la question du test qui ne marche pas  *)

let tests = 
["test 1">:: test1;
 "test 2">:: test2]

(* XXX: turn those 'printf's into actual tests. Cf. [booleanT] for examples *)

(* let tests = ["successeur" >:: fun ctxt -> assert_equal (BoundVar 1) (BoundVar 1);] *)



(*
(* let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte succtest 0)) *)
let () = Printf.printf "%d \n" (lambda_term_to_int(reduction_forte testsucc 0))		

let () = Printf.printf "%s \n" (lambda_term_to_string(plus_test))
let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte plus_test 0)) 
(*
let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte testsucc))
let () = Printf.printf "%d \n" (lambda_term_to_int(reduction_forte testsucc))
 *)
(*let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte plus_test)) *) 
						 
(*let () = Printf.printf "%s \n" (lambda_term_to_string(plus_test))
let () = Printf.printf "%s \n" (lambda_term_to_string(evaluation(plus_test))) *)


(*
let () = Printf.printf "%s \n" (lambda_term_to_string (int_to_lambda_term 0))
let () = Printf.printf "%s \n" (lambda_term_to_string (int_to_lambda_term 1))
let () = Printf.printf "%s \n" (lambda_term_to_string (int_to_lambda_term 2))
    

let () = Printf.printf "%d \n" (lambda_term_to_int(int_to_lambda_term 2))
let () = Printf.printf "%d \n" (lambda_term_to_int(int_to_lambda_term 1))
let () = Printf.printf "%d \n" (lambda_term_to_int(int_to_lambda_term 0)) *)
		       


 *)
