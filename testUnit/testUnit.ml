open OUnit


let test1 test_ctxt = assert_equal Appl(Abs(BoundVar 0),FreeVar v) (Lambda_calcul.evaluation (FreeVar v));;
let test2 test_ctxt = assert_equal Appl(Abs(BoundVar 0),Abs(BoundVar 0)) (Lambda_calcul.evaluation (Abs(BoundVar 0)));;

let eval = 
"eval">:::
["test 1">:: test1;
 "test 2">:: test2]

;;
let () = 
  run_test_tt_main eval

;;
