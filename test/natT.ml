open OUnit2

open Lambda
open Nat

let testsucc = Appl(succ,(int_to_lambda_term 0))
let succ_test = Abs(Abs(Abs(Appl(BoundVar 1,Appl(Appl(BoundVar 2,BoundVar 1),BoundVar 0)))))
let plus_test = Appl(Appl(plus,(int_to_lambda_term 2)),(int_to_lambda_term 2))

let test1 test_ctxt = assert_equal (FreeVar "y") (FreeVar "y")
let test2 test_ctxt = assert_equal (BoundVar 0) (BoundVar 0)

(* batterie de test sur les nats *)

let test3 test_ctxt = assert_equal (reduction_forte succ_test 0) (int_to_lambda_term 1)
let test4 test_ctxt = assert_equal (reduction_forte plus_test) (int_to_lambda_term 4) 
(* let test5 test_ctxt = assert_equal
let test6 test_ctxt = assert_equal
let test7 test_ctxt = assert_equal
let test8 test_ctxt = assert_equal
let test9 test_ctxt = assert_equal
let test10 test_ctxt = assert_equal *)

let tests = 
["test 1">:: test1; 
 "test 2">:: test2;
"test 3">:: test3;
"test 4">:: test4
(*"test 5">:: test5;
"test 6">:: test6;
"test 7">:: test7;
"test 8">:: test8;
"test 9">:: test9;
"test 10">:: test10*)]


(* let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte succtest 0)) *)
(* let () = Printf.printf "%d \n" (lambda_term_to_int(reduction_forte testsucc 0))		

let () = Printf.printf "%s \n" (lambda_term_to_string(plus_test))
let () = Printf.printf "%s \n" (lambda_term_to_string(reduction_forte plus_test 0))  *)
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
		       

