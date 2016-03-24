open OUnit2
open Lambda


(*test de check terme *)

let x = (Pi("A",Star,Pi("B",(Pi ("x", Inv(BVar 0),Star)),Pi("C",(Pi ("x", Inv(BVar 0),Star)), (Pi ("1",(Pi ("2", (Pi("a",Star,Pi("b",(Inv(Appl(BVar 5 ,Inv(BVar 1)))),Inv(Appl(BVar 4, Inv(BVar 1)))))),(Pi ("a",Inv(BVar 5),Inv(Appl(BVar 4,Inv(BVar 0))))))),(Pi ("a",Inv(BVar 4),Inv(Appl(BVar 3,Inv(BVar 0)))))))))))



let test1 text_ctxt = assert_equal (check [] (x) Star) (true)
let test2 text_ctxt = assert_equal () ()
let test3 text_ctxt = assert_equal () ()		      


let eval = 
["test 1">:: test1;
 "test 2">:: test2;
 "test 3">:: test3;]

