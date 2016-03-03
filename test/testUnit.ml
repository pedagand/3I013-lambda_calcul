open OUnit





let eval = 
"eval">:::
["test 1">:: test1;
 "test 2">:: test2;
 "test 3">:: test3;]

;;
let () = 
  run_test_tt_main eval

;;
