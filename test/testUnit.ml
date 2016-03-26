open OUnit2
open Lambda


(*test de check terme *)

let test1x = (Pi("A",Star,Pi("B",(Pi ("x", Inv(BVar 2),Star)),Pi("C",(Pi ("x", Inv(BVar 3),Star)), (Pi ("1",(Pi ("2", (Pi("a",Star,Pi("b",(Inv(Appl(BVar 5 ,Inv(BVar 1)))),Inv(Appl(BVar 4, Inv(BVar 1)))))),(Pi ("a",Inv(BVar 5),Inv(Appl(BVar 4,Inv(BVar 0))))))),(Pi ("a",Inv(BVar 4),Inv(Appl(BVar 2,Inv(BVar 0)))))))))))

let test1y = "(pi A *! (pi B (pi x A *!) (pi C (pi x A *!) (pi 1 (pi 2 (pi a *! (pi b (B a) (C a))) (pi a A (B a))) (pi a A (C a))))))"

let testcheck3x = "(pi A *! (pi B (pi x A *!) *!))"


let () = Printf.printf "\n test final %b \n" (check [] (read test1y) Star "" [])



(* ------------------------- test check ----------------------------------*)


(* let testcheck1 text_ctxt = assert_equal (check [] (test1x) Star "" []) (true) *)
let testcheck2 test_ctxt = assert_equal (check [] (Pi("x",Star,Star)) Star "" []) (true)
let testcheck1 test_ctxt = assert_equal (check [] (read testcheck3x) Star "" []) (true) 


(* ------------------------- test pretty_print_inTm ----------------------- *)
let test_pretty1 test_ctxt = assert_equal 
			(read(pretty_print_inTm (Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Star),Inv(FVar "y")))) []))
			(read "((: (lambda x x) *!) y)")
let () = Printf.printf "POURQUOI %s" (pretty_print_inTm (Abs("x",Abs("y",Abs("z",Inv(Appl(BVar 2, Inv(Appl(BVar 1, Inv(BVar 0))))))))) [] )
let test_pretty2 test_ctxt = assert_equal
			 (read(pretty_print_inTm (Abs("x",Abs("y",Abs("z",Inv(Appl(BVar 2, Inv(Appl(BVar 1, Inv(BVar 0))))))))) [] ))
			 (read "(lambda (x y z) (x (y z)))")
let test_pretty3 test_ctxt = assert_equal 
			 (read(pretty_print_inTm (Inv(Ann(Abs("x",Inv(BVar 0)),Pi("x",Star,Star)))) []))
			 (read "(: (lambda x x) (pi x *! *!))")
let test_pretty4 text_ctxt = assert_equal 
			       (*(read (pretty_print_inTm (test1x) [])) 
			       (read (test1y)) *) () () 
let test_pretty5 text_ctxt = assert_equal 
			       (read (pretty_print_inTm ((Pi("x",Star,Pi("y",Star,Pi("z",Star,Star))))) [])) 
			       (read "(pi (x y z ) *! *!)") 		      

			       


let eval = 
[(* "testcheck1">:: testcheck1; *)
 "test_pretty1">:: test_pretty1;
 "test_pretty2">:: test_pretty2;
 "test_pretty3">:: test_pretty3;
 "test_pretty4">:: test_pretty4;
 "test_pretty5">:: test_pretty5;]

