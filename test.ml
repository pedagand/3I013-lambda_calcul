open OUnit2

let suite =
  test_list [ "Parser tests" >::: ParserT.tests
            ; "Boolean test" >::: BooleanT.tests ]

let () =
  run_test_tt_main suite
