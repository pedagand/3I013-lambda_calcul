open OUnit2
open Lambda

let () = Printf.printf "parse %s \n" (inTm_to_string (read "((: (lambda x x) N) y)") [])

let inputs
    = [("(lambda x x)", Abs("x",Inv(BVar 0)));
       ("(lambda x y)", Abs("x",Inv(FVar "y")));
       ("(x y z)", Inv(Appl(Appl(FVar "x", Inv(FVar "y")), Inv(FVar "z"))));
       ("(lambda (x y z) (x (y z)))", Abs("x",Abs("y",Abs("z",Inv(Appl(BVar 2, Inv(Appl(BVar 1, Inv(BVar 0)))))))));
       ("(lambda (x y z) (x y z))", Abs("x",Abs("y",Abs("z",Inv(Appl (Appl (BVar 2, Inv(BVar 1)), Inv(BVar 0)))))));
       ("((: (lambda x x) N) y)", Inv(Appl(Ann(Abs("x",Inv(BVar 0)),Nat),Inv(FVar "y")))) ]

let tests
    = List.map (fun (term, res) -> term >:: fun ctxt -> assert_equal (read term) res) inputs
