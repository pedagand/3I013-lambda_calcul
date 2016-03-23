open OUnit2
open Lambda 


(* test de checker  *)
let test_check1 test_ctxt = assert_equal 
			      (check [] (Abs("f",Abs("g",Inv(Appl(BVar 1,Inv(BVar 0)))))) (Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool)))) 
			      (true)
let test_check2 test_ctxt = assert_equal 
			      (check [] (Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))),(Abs("y",(Inv(BVar 0))))))) (Fleche(Bool,Bool)) ) 
			      (true)
let test_check3 test_ctxt = assert_equal 
			      (check [] (Inv(Iter(Succ(Zero),Abs("x",Inv(BVar 0)),Ann(Zero,Nat)))) Nat) 
			      (true) 
			      


(* test de reduction forte  *)
let test3 test_ctxt = assert_equal 
			(reduction_forte (SAppl(SAbs(SBVar 0),SAbs(SBVar 0)))  0 )
			(SAbs(SBVar 0))



(* test de typed_to_simple_inTm  *)
(* let x = Abs("x",Inv(Appl(BVar 0,Inv(FVar "0")))) *)
(* let y = Appl((Ann(x,(Fleche(Bool,Fleche(Bool,Bool))))),Inv(FVar "k")) *)
let test4 test_ctxt = assert_equal 
			(typed_to_simple_inTm (Abs("x",Inv(Appl(BVar 0,Inv(FVar "0"))))))
			(SAbs(SAppl(SBVar 0,SFVar "0")))
let test5 test_ctxt = assert_equal 
			(typed_to_simple_inTm (Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Fleche(Fleche(Bool,Bool),Fleche(Bool,Bool))),(Abs("y",(Inv(BVar 0))))))))
			(SAppl(SAbs(SBVar 0),SAbs(SBVar 0)))


(* test de substitution_inTm *)
(* le -1 pour la substitution pour dire que l'on veut substituter la bound var 0 car normallement on appelle cette fonction sans passer au travers du premier lambda *)
let test6 text_ctxt = assert_equal 
			 (substitution_inTm (Abs("x",Inv(Appl(BVar 0,Inv(FVar "0"))))) (FVar "y") (-1))
			 ((Abs("x",Inv(Appl(FVar "y",Inv(FVar "0"))))))

let test7 test_ctxt = assert_equal
			(substitution_inTm (Abs("x",Inv(Appl(BVar 0,Inv(FVar "0"))))) (Ann(Abs("y",Inv(BVar 0)),Fleche(Bool,Bool))) (-1))
			((Abs("x",Inv(Appl((Ann(Abs("y",Inv(BVar 0)),Fleche(Bool,Bool))),Inv(FVar "0")))))) 




let eval = 
["test 1">:: test_check1;
 "test 2">:: test_check2;
 "test 4">:: test_check3;
 "test 3">:: test3;
 "test 4">:: test4;
 "test 5">:: test5;
 "test 6">:: test6;
 "test 7">:: test7;]
