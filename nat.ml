open Lambda

(** * Les entiers de church *)

(* Fonctions de manipulation *)

		       

(* Défintions des termes *)

let zero = Abs("x",Abs("y",Inv(BVar 0)))
let succ = Abs("n",Abs("f",Abs("x",Inv(Appl(BVar 1,(Inv(Appl(Appl(BVar 2,Inv(BVar 1)),Inv(BVar 0)))))))))
let testsucc = Appl(Ann(succ,Nat),zero)
let testmegasucc = Appl(Ann(succ,Nat),Inv(Appl(Ann(succ,Nat),Inv(Appl(Ann(succ,Nat),zero)))))
let lzero = SAbs(SAbs(SBVar 0))
let lsucc = SAbs(SAbs(SAbs(SAppl(SBVar 1,SAppl(SAppl(SBVar 2,SBVar 1),SBVar 0)))))
let () = Printf.printf "\n test iter \n";
	 Printf.printf "%s \n" (lambda_term_to_string(lzero));
	 Printf.printf "%s \n" (lambda_term_to_string(iter n lsucc lzero));
	 Printf.printf "\n test big step eval avec les nat \n";
	 Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_exTm(testsucc)));
	 Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_inTm (value_to_inTm 0 (big_step_eval_exTm testsucc []))));
	 Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_exTm(testmegasucc)));
	 Printf.printf "%s \n" (lambda_term_to_string(typed_to_simple_inTm (value_to_inTm 0 (big_step_eval_exTm testmegasucc []))))

