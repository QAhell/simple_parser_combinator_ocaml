open Simple_parser_combinator

let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;
module Test =
struct
  module Test_error_info =
  struct
    type t = (Utf8_stream.Decoded_string_input.t * string) option
    let default_error = None
    let is_default_error = function
      | None -> true
      | _ -> false
    let is_real_error e = not (is_default_error e)
    let merge e1 e2 =
      match e1, e2 with
        | None, e2 -> e2
        | e1, None -> e1
        | (Some ((i1, _), _), Some ((i2, _), _)) ->
            if i1 < i2 then e2 else e1
    let make_error message input = Some (input, message)
  end

  module P = Recursive_descent_parser (Trampoline.Not_a_trampoline) (Test_error_info) ;;
  module C = P.Code_point_parsers (Utf8_stream.Decoded_string_input) (Test_error_info) ;;
  let exec p s =
    P.execute p
      (fun result input error -> Either.Left (result, input, error))
      (fun error -> Either.Right error) (Utf8_stream.Decoded_string_input.of_string s) ;;

  let test_error () =
    let i = (Utf8_stream.Decoded_string_input.of_string "test") in
    let e = Test_error_info.make_error "test" i in
    let result = P.execute (P.error e) (fun _ _ _ -> None) (fun e -> Some e)
      (Utf8_stream.Decoded_string_input.of_string "abc") in
    assertTrue (Some e = result) "Expected error parser to return the error provided by its argument"

  (* test recurse *)

  let test_and_then () =
    let p = C.expect_string "A" in
    let q = C.expect_string "B" in
    let f s = exec (P.and_then p q) s in
    assertTrue (match f "ABc" with
        | Either.Left (("A", "B"), (2, _), _) -> true
        | _ -> false) "and_then should first parse A and then parse B" ;
    assertTrue (match f "AcB" with
        | Either.Right _ -> true
        | _ -> false) "and_then should fail if second parser fails" ;
    assertTrue (match f "cAB" with
        | Either.Right _ -> true
        | _ -> false) "and_then should fail if first parser fails"

  let test_expect () =
    let p = P.expect "name" (fun input ->
        match Utf8_stream.Decoded_string_input.get input with
          | Some (c, input) when c = int_of_char 'y' -> Either.Left (c, input)
          | _ -> Either.Right (Some (input, "error"))) in
    let f s = exec p s in
    assertTrue (match f "yx" with
                | Either.Left (c, (1, _), _) when c = int_of_char 'y' -> true
                | _ -> false) "expect should return true if argument returns Left" ;
    assertTrue (match f "xy" with
                | Either.Right (Some (_, "error")) -> true
                | _ -> false) "expect should return false if argument returns Right"

  let test_convert () =
    let p = P.convert (C.expect_natural_number) int_of_string in
    let f s = exec p s in
    assertTrue (match f "123" with
                | Either.Left (123, (3, _), _) -> true
                | _ -> false) "convert should correctly use conversion function"

  let test_or_else () =
    let p = C.expect_string "A" in
    let q = C.expect_string "B" in
    let f s = exec (P.or_else p q) s in
    assertTrue (match f "Ax" with
                  | Either.Left ("A", (1, _), _) -> true
                  | _ -> false) "or_else should accept first parse" ;
    assertTrue (match f "Bx" with
                  | Either.Left ("B", (1, _), _) -> true
                  | _ -> false) "or_else should accept second parse" ;
    assertTrue (match f "Cx" with
                  | Either.Right _ -> true
                  | _ -> false) "or_else should fail if both alternatives fail"

  let test_bind () =
    let p = C.expect_not_code_point [ int_of_char 'C' ] in
    let q c = C.expect_code_point c in
    let f s = exec (P.bind p q) s in
    assertTrue (match f "BBw" with
                | Either.Left (b, (2, _), _) when int_of_char 'B'= b -> true
                | _ -> false) "bind should succeed if both parsers succeed" ;
    assertTrue (match f "ABw" with
                | Either.Right _ -> true
                | _ -> false) "bind should fail if second parser fails" ;
    assertTrue (match f "CCw" with
                | Either.Right _ -> true
                | _ -> false) "bind should fail if first parser fails"

  let test_repeat_and_fold_left () =
    let p = P.repeat_and_fold_left (C.expect_not_code_point [ int_of_char 'C' ])
                [] (fun xs x -> x :: xs) in
    let f s = exec p s in
    assertTrue (match f "ABC" with
                | Either.Left ([ b ; a ], (2, _), _) when b = int_of_char 'B' && a = int_of_char 'A' -> true
                | _ -> false) "repeat_and_fold_left should read until failed parse" ;
    assertTrue (match f "ABD" with
                | Either.Left ([ d ; b ; a ], (3, _), _)
                    when d = int_of_char 'D' &&
                         b = int_of_char 'B' &&
                         a = int_of_char 'A' -> true
                | _ -> false) "repeat_and_fold_left should read until end of input" ;
    assertTrue (match f "C" with
                | Either.Left ([], (0, _), _) -> true
                | _ -> false) "repeat_and_fold_left should accept zero parses" ;
    assertTrue (match f "" with
                | Either.Left ([], (0, _), _) -> true
                | _ -> false) "repeat_and_fold_left should accept empty input"

  let test_repeated () =
    let p = P.repeated (C.expect_not_code_point [ int_of_char 'C' ]) in
    let f s = exec p s in
    assertTrue (match f "ABC" with
                | Either.Left ([ a ; b ], (2, _), _) when b = int_of_char 'B' && a = int_of_char 'A' -> true
                | _ -> false) "repeated should read until failed parse" ;
    assertTrue (match f "ABD" with
                | Either.Left ([ a ; b ; d ], (3, _), _)
                    when d = int_of_char 'D' &&
                         b = int_of_char 'B' &&
                         a = int_of_char 'A' -> true
                | _ -> false) "repeated should read until end of input" ;
    assertTrue (match f "C" with
                | Either.Left ([], (0, _), _) -> true
                | _ -> false) "repeated should accept zero parses" ;
    assertTrue (match f "" with
                | Either.Left ([], (0, _), _) -> true
                | _ -> false) "repeated should accept empty input"

  let test_once_or_more_and_fold_left () =
    let p = P.once_or_more_and_fold_left (C.expect_not_code_point [ int_of_char 'C' ])
                [] (fun xs x -> x :: xs) in
    let f s = exec p s in
    assertTrue (match f "ABC" with
                | Either.Left ([ b ; a ], (2, _), _) when b = int_of_char 'B' && a = int_of_char 'A' -> true
                | _ -> false) "once_or_more_and_fold_left should read until failed parse" ;
    assertTrue (match f "ABD" with
                | Either.Left ([ d ; b ; a ], (3, _), _)
                    when d = int_of_char 'D' &&
                         b = int_of_char 'B' &&
                         a = int_of_char 'A' -> true
                | _ -> false) "once_or_more_and_fold_left should read until end of input" ;
    assertTrue (match f "C" with
                | Either.Right _ -> true
                | _ -> false) "once_or_more_and_fold_left should reject zero parses" ;
    assertTrue (match f "" with
                | Either.Right _ -> true
                | _ -> false) "once_or_more_and_fold_left should reject empty input"

  let test_once_or_more () =
    let p = P.once_or_more (C.expect_not_code_point [ int_of_char 'C' ]) in
    let f s = exec p s in
    assertTrue (match f "ABC" with
                | Either.Left ([ a ; b ], (2, _), _) when b = int_of_char 'B' && a = int_of_char 'A' -> true
                | _ -> false) "once_or_more should read until failed parse" ;
    assertTrue (match f "ABD" with
                | Either.Left ([ a ; b ; d ], (3, _), _)
                    when d = int_of_char 'D' &&
                         b = int_of_char 'B' &&
                         a = int_of_char 'A' -> true
                | _ -> false) "once_or_more should read until end of input" ;
    assertTrue (match f "C" with
                | Either.Right _ -> true
                | _ -> false) "once_or_more should reject zero parses" ;
    assertTrue (match f "" with
                | Either.Right _ -> true
                | _ -> false) "once_or_more should reject empty input"

  let test_optional () =
    let p = P.optional (C.expect_code_point (int_of_char 'A')) in
    let f s = exec p s in
    assertTrue (match f "AB" with
                | Either.Left (Some a, (1, _), _) when a = int_of_char 'A' -> true
                | _ -> false) "optional should return some value if parse successful" ;
    assertTrue (match f "B" with
                | Either.Left (None, (0, _), _) -> true
                | _ -> false) "optional should return None if parse failed" ;
    assertTrue (match f "" with
                | Either.Left (None, (0, _), _) -> true
                | _ -> false) "optional should return None at end of input"

  let test_name () =
    let p = P.name "fu" (C.expect_code_point (int_of_char 'A')) in
    let f s = exec p s in
    assertTrue (match f "A" with
                  | Either.Left (a, (1, _), _) -> a = int_of_char 'A'
                  | _ -> false) "name should succeed if argument succeeds" ;
    assertTrue (match f "B" with
                  | Either.Right _ -> true
                  | _ -> false) "name should fail if argument fails"

  let test_first () =
    let p = P.first (P.and_then (C.expect_string "A") (C.expect_string "B")) in
    let f s = exec p s in
    assertTrue (match f "AB" with
                  | Either.Left ("A", (2, _), _) -> true
                  | _ -> false) "first should return the first component of parse result"

  let test_second () =
    let p = P.second (P.and_then (C.expect_string "A") (C.expect_string "B")) in
    let f s = exec p s in
    assertTrue (match f "AB" with
                  | Either.Left ("B", (2, _), _) -> true
                  | _ -> false) "second should return the second component of parse result"

  (* other Parser_combinator elements *)

  let test_expect_several () =
    let p = C.expect_several "expect_several"
      (fun c -> c = int_of_char 'a')
      (fun c -> c = int_of_char 'b') in
    let f s = P.execute p
      (fun result input error -> Some (result, input, error))
      (fun _ -> None) (Utf8_stream.Decoded_string_input.of_string s) in
    assertTrue (f "" = None) "expect_several should fail on empty input" ;
    assertTrue (f "x" = None) "expect_several should reject wrong start character" ;
    assertTrue (match f "a" with
                  | Some ("a", (1, _), _) -> true
                  | _ -> false) "expect_several should parse start character correctly" ;
    assertTrue (match f "abbc" with
                  | Some ("abb", (3, _), _) -> true
                  | _ -> false) "expect_several should parse following characters correctly" ;;

  let test_expect_code_point () =
    let p = C.expect_code_point 126980 in (* 🀄 *)
    let f s = exec p s in
    assertTrue (match f "\xF0\x9F\x80\x84\xF0\x9F\x80\x84" with
                | Either.Left (a, (4, _), _) when 126980 = a -> true
                | _ -> false) "expect_code_point should parse code point correctly" ;
    assertTrue (match f "b" with
                | Either.Right _ -> true
                | _ -> false) "expect_code_point should reject wrong code point" ;;

  let test_expect_code_points () =
    let p = C.expect_code_points [126980; 126981] in
    let f s = exec p s in
    assertTrue (match f "\xF0\x9F\x80\x84\xF0\x9F\x80\x85ab" with
                | Either.Left ([a;b], (8, _), _) when 126980 = a && 126981 = b -> true
                | _ -> false) "expect_code_points should parse code points correctly" ;
    assertTrue (match f "a\xF0\x9F\x80\x85b" with
                | Either.Right _ -> true
                | _ -> false) "expect_code_points should reject wrong code point at beginning" ;
    assertTrue (match f "\xF0\x9F\x80\x84ab" with
                | Either.Right _ -> true
                | _ -> false) "expect_code_points should reject wrong code point" ;;

  let test_expect_not_code_point () =
    let p = C.expect_not_code_point [126980; 126981] in
    let f s = exec p s in
    assertTrue (match f "a\xF0\x9F\x80\x84\xF0\x9F\x80\x85ab" with
                | Either.Left (a, (1, _), _) when int_of_char 'a' = a -> true
                | _ -> false) "expect_not_code_point should parse code point correctly" ;
    assertTrue (match f "\xF0\x9F\x80\x85b" with
                | Either.Right _ -> true
                | _ -> false) "expect_code_points should reject wrong code point" ;;

  let test_expect_string () =
    let p = C.expect_string "a\xF0\x9F\x80\x84b" in
    let f s = exec p s in
    assertTrue (match f "a\xF0\x9F\x80\x84b\xF0\x9F\x80\x85b" with
                | Either.Left ("a\xF0\x9F\x80\x84b", (6, _), _) -> true
                | _ -> false) "expect_string should parse code points correctly" ;
    assertTrue (match f "a\xF0\x9F\x80\x85b\xF0\x9F\x80\x85b" with
                | Either.Right _ -> true
                | _ -> false) "expect_string should reject wrong code points" ;;

  let test_expect_identifier () =
    let p = C.expect_identifier in
    let f s = exec p s in
    assertTrue (match f "ab42$" with
                | Either.Left ("ab42", (4, _), _) -> true
                | _ -> false) "expect_identifier should parse identifier correctly" ;
    assertTrue (match f "%ab42" with
                | Either.Right _ -> true
                | _ -> false) "expect_identifier should reject non-identifier characters" ;;

  let test_expect_natural_number () =
    let p = C.expect_natural_number in
    let f s = exec p s in
    assertTrue (match f "01234a" with
                | Either.Left ("01234", (5, _), _) -> true
                | _ -> false) "expect_natural_number should parse number correctly" ;
    assertTrue (match f "a01234" with
                | Either.Right _ -> true
                | _ -> false) "expect_natural_number should reject non-digit characters" ;;

  let test_expect_spaces () =
    let p = C.expect_spaces in
    let f s = exec p s in
    assertTrue (match f "" with
                | Either.Right _ -> true
                | _ -> false) "expect_spaces should reject empty input" ;
    assertTrue (match f " \n\r\tx" with
                | Either.Left (" \n\r\t", (4, _), _) -> true
                | _ -> false) "expect_spaces should parse spaces correctly" ;
    assertTrue (match f "a  " with
                | Either.Right _ -> true
                | _ -> false) "expect_spaces should reject non-space characters" ;;

  let test_optional_spaces_before () =
    let p = C.optional_spaces_before (C.expect_string "x") in
    let f s = exec p s in
    assertTrue (match f "x" with
                | Either.Left ("x", _, _) -> true
                | _ -> false) "optional_spaces_before should accept zero spaces" ;
    assertTrue (match f " \n\r\tx" with
                | Either.Left ("x", _, _) -> true
                | _ -> false) "optional_spaces_before should accept spaces" ;
    assertTrue (match f "y x" with
                | Either.Right _ -> true
                | _ -> false) "optional_spaces_before should fail if argument parser fails" ;;

  let test () = 
    test_error () ;
    test_and_then () ;
    test_expect () ;
    test_convert () ;
    test_or_else () ;
    test_bind () ;
    test_repeat_and_fold_left () ;
    test_repeated () ;
    test_once_or_more_and_fold_left () ;
    test_once_or_more () ;
    test_optional () ;
    test_name () ;
    test_first () ;
    test_second () ;
    test_expect_several () ;
    test_expect_code_point () ;
    test_expect_code_points () ;
    test_expect_not_code_point () ;
    test_expect_string () ;
    test_expect_identifier () ;
    test_expect_natural_number () ;
    test_expect_spaces () ;
    test_optional_spaces_before () ;;

end ;;

Test.test() ;;
