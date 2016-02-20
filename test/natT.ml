open OUnit2

open Lambda
open Nat

let testsucc = Appl(succ,(int_to_lambda_term 0))
let succ_test = Abs(Abs(Abs(Appl(BoundVar 1,Appl(Appl(BoundVar 2,BoundVar 1),BoundVar 0)))))
let plus_test = Appl(Appl(plus,(int_to_lambda_term 2)),(int_to_lambda_term 2))

let test1 fun ctxt = assert_equal (BoundVar 0) (BoundVar 0);;
let test2 fun ctxt = assert_equal (FreeVar "y") (FreeVar "y");;

let tests = "test" >:::
["test 1">:: test1;
 "test 2">:: test2]

(* XXX: turn those 'printf's into actual tests. Cf. [booleanT] for examples *)

(* let tests = ["successeur" >:: fun ctxt -> assert_equal (BoundVar 1) (BoundVar 1);] *)




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
		       

