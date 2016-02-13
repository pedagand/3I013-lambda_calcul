open OUnit2

let suite =
  test_list [ "Parser tests" >::: ParserT.tests ]

let () =
  run_test_tt_main suite
