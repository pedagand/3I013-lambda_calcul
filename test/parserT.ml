open OUnit2
open Lambda


let test1x = (Pi("A",Star,Pi("B",(Pi ("x", Inv(BVar 0),Star)),Pi("C",(Pi ("x", Inv(BVar 1),Star)), (Pi ("1",(Pi ("2", (Pi("a",Star,Pi("b",(Inv(Appl(BVar 2 ,Inv(BVar 1)))),Inv(Appl(BVar 2, Inv(BVar 1)))))),(Pi ("a",Inv(BVar 3),Inv(Appl(BVar 3,Inv(BVar 0))))))),(Pi ("a",Inv(BVar 3),Inv(Appl(BVar 2,Inv(BVar 0)))))))))))

let test1y = "(pi A * (pi B (pi x A *) (pi C (pi x A *) (pi 1 (pi 2 (pi a * (pi b (B a) (C a))) (pi a A (B a))) (pi a A (C a))))))"

let inputs
    = [("(lambda x x)", Abs("x",Inv(BVar 0)));
       ("(lambda x y)", Abs("x",Inv(FVar "y")));
       ("(x y z)", Inv(Appl(Appl(FVar "x", Inv(FVar "y")), Inv(FVar "z"))));
       ("(lambda (x y z) (x (y z)))", Abs("x",Abs("y",Abs("z",Inv(Appl(BVar 2, Inv(Appl(BVar 1, Inv(BVar 0)))))))));
       ("(lambda (x y z) (x y z))", Abs("x",Abs("y",Abs("z",Inv(Appl (Appl (BVar 2, Inv(BVar 1)), Inv(BVar 0)))))));
       ("((: (lambda x x) *) y)", Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Star),Inv(FVar "y"))));
      ("(pi x * *)",Pi("x",Star,Star));
      ("(pi x (pi y * *) *)", Pi("x",Pi("y",Star,Star),Star));
      ("(pi (x y z) * (lambda w w))",Pi("x",Star,Pi("y",Star,Pi("z",Star,Abs("w",Inv(BVar 0))))));
      ("(pi A * (pi B (pi x A *) *))",Pi("A",Star,Pi("B",(Pi ("x" ,Inv(BVar 0), Star)),Star)));
      ("(-> * *)",Pi("NO",Star,Star));
      ("(succ (succ zero))",Succ(Succ(Zero)));
      ("(: (succ zero) N)",Inv(Ann(Succ(Zero),Nat)));
      ("(iter N N N N)",Inv(Iter(Nat,Nat,Nat,Nat)));
      (* ( (pretty_print_inTm test1x []),(test1x)); *)
      (* (test1y),(test1x) ;*)]




let tests
    = List.map (fun (term, res) -> term >:: fun ctxt -> assert_equal (read term) res) inputs
