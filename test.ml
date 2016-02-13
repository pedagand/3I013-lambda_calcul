open OUnit2

let suite =
  "Parser tests" >::: Parser.tests

let () =
  run_test_tt_main suite
