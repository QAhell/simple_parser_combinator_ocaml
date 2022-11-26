module type Error_info =
sig
  type t
  val default_error : t
  val is_default_error : t -> bool
  val is_real_error : t -> bool
  val merge : t -> t -> t
end

module type Parser_combinator =
sig
  module Trampoline : Trampoline.Trampoline
  module Error_info : Error_info

  type ('input, 'output) t

  val execute :
    ('input, 'output) t ->
    ('output -> 'input -> Error_info.t option -> 'result) ->
    (Error_info.t -> 'result) ->
    'input -> 'result

  val error : Error_info.t -> ('input, 'output) t

  val recurse :
    (('input, 'output) t -> ('input, 'output) t) ->
    ('input, 'output) t

  val and_then :
    ('input, 'a) t ->
    ('input, 'b) t ->
    ('input, 'a * 'b) t

  val expect :
    string ->
    ('input -> ('output * 'input, Error_info.t) Either.t) ->
    ('input, 'output) t

  val convert :
    ('input, 'a) t ->
    ('a -> 'b) ->
    ('input, 'b) t

  val or_else :
    ('input, 'output) t ->
    ('input, 'output) t ->
    ('input, 'output) t

  val bind :
    ('input, 'a) t ->
    ('a -> ('input, 'b) t) ->
    ('input, 'b) t

  val repeat_and_fold_left :
    ('input, 'a) t ->
    'b ->
    ('b -> 'a -> 'b) ->
    ('input, 'b) t

  val repeated :
    ('input, 'output) t ->
    ('input, 'output list) t

  val once_or_more_and_fold_left :
    ('input, 'a) t ->
    'b ->
    ('b -> 'a -> 'b) ->
    ('input, 'b) t

  val once_or_more :
    ('input, 'output) t ->
    ('input, 'output list) t

  val optional :
    ('input, 'output) t ->
    ('input, 'output option) t

  val name :
    string ->
    ('input, 'output) t ->
    ('input, 'output) t

  val first :
    ('input, 'a * 'b) t ->
    ('input, 'a) t

  val second :
    ('input, 'a * 'b) t ->
    ('input, 'b) t

  module Code_point_parsers :
    functor (Input : Utf8_stream.Code_point_input) ->
    functor (_ : sig val make_error : string -> Input.t -> Error_info.t end) ->
      sig
        val expect_several :
          string ->
          (int -> bool) ->
          (int -> bool) ->
          (Input.t, string) t

        val expect_code_point : int -> (Input.t, int) t

        val expect_code_points : int list -> (Input.t, int list) t

        val expect_not_code_point : int list -> (Input.t, int) t

        val expect_string : string -> (Input.t, string) t

        val expect_identifier : (Input.t, string) t

        val expect_natural_number : (Input.t, string) t

        val expect_spaces : (Input.t, string) t

        val optional_spaces_before : (Input.t, 'a) t -> (Input.t, 'a) t
      end

end

module Parser_combinator_type =
  functor (Trampoline : Trampoline.Trampoline) ->
  functor (Error_info : Error_info) ->
    struct
      module type T =
        Parser_combinator with
          module Trampoline = Trampoline and
          module Error_info = Error_info
    end

module Recursive_descent_parser (Trampoline : Trampoline.Trampoline) (Error_info : Error_info) =
struct
  module Trampoline = Trampoline
  module Error_info = Error_info

  type ('input, 'output) t =
    'input -> (('output * 'input) option * Error_info.t) Trampoline.t

  let execute parsr on_success on_error toks =
    match Trampoline.execute (parsr toks) with
      | (Some (result, input), error) ->
          on_success result input
            (if Error_info.is_real_error error then Some error else None)
      | (None, error) -> on_error error

  let error e _ = Trampoline.return (None, e)

  let rec recurse f = f (fun i -> recurse f i)

  let and_then p q input =
    Trampoline.bind (p input) (function
        | (Some (result_p, input), error_p) ->
            Trampoline.map (function
              | (Some (result_q, input), error_q) ->
                  (Some ((result_p, result_q), input), Error_info.merge error_p error_q)
              | (None, error_q) ->
                  (None, Error_info.merge error_p error_q)) (q input)
        | (None, error_p) -> Trampoline.return (None, error_p))

  let expect _ f input = (* name of the parser not used with simple parser combinators *)
    match f input with
      | Either.Left (output, input) ->
          Trampoline.return (Some (output, input), Error_info.default_error)
      | Either.Right error -> Trampoline.return (None, error)

  let convert parsr converter input =
    Trampoline.map (function
        | (Some (output, input), error) -> (Some (converter output, input), error)
        | (None, error) -> (None, error)) (parsr input)

  let or_else p q input =
    Trampoline.bind (p input) (function
        | (Some (_, _), _) as result -> Trampoline.return result
        | (None, error_p) ->
            Trampoline.map (function
              | (Some (result_q, input), error_q) ->
                  (Some (result_q, input), Error_info.merge error_p error_q)
              | (None, error_q) ->
                  (None, Error_info.merge error_p error_q)) (q input))

  let bind p f input =
    Trampoline.bind (p input) (function
      | (Some (output_p, input), error_p) ->
          Trampoline.map (fun (result, error_q) ->
                            (result, Error_info.merge error_p error_q))
            (f output_p input) 
      | (None, error_p) -> Trampoline.return (None, error_p))

  let rec repeat_and_fold_left p acc f input =
    Trampoline.bind (p input) (function
      | (Some (output, input), _) ->
          repeat_and_fold_left p (f acc output) f input
      | (None, error) ->
          Trampoline.return (Some (acc, input), error))

  let repeated p = convert (repeat_and_fold_left p [] (fun xs x -> x :: xs)) List.rev

  let once_or_more_and_fold_left p acc f input =
    bind p (fun result -> repeat_and_fold_left p (f acc result) f) input

  let once_or_more p =
    convert (once_or_more_and_fold_left p [] (fun xs x -> x :: xs)) List.rev

  let optional p input =
    Trampoline.map (function
      | (Some (output, input), error) -> (Some (Some output, input), error)
      | (None, error) -> (Some (None, input), error)) (p input)

  let name _ p = p

  let first p = convert p (fun (x, _) -> x)

  let second p = convert p (fun (_, y) -> y)

  module Code_point_parsers =
    functor (Input : Utf8_stream.Code_point_input) ->
    functor (Make_error : sig val make_error : string -> Input.t -> Error_info.t end) ->
      struct
        let expect_several name is_first_code_point is_later_code_point input =
          match Input.get input with
            | Some (first_code_point, input) when is_first_code_point first_code_point ->
                let rec parse_later_chars input acc =
                  match Input.get input with
                    | Some (later_code_point, input) when is_later_code_point later_code_point ->
                        parse_later_chars input (Utf8_stream.Encoded_string_output.put acc later_code_point)
                    | _ ->
                        Trampoline.return (Some (Utf8_stream.Encoded_string_output.to_string acc, input), Error_info.default_error) in
              parse_later_chars input (Utf8_stream.Encoded_string_output.put (Utf8_stream.Encoded_string_output.empty ()) first_code_point)
          | Some (code_point, input) -> Trampoline.return (None, Make_error.make_error (
                  "Expected identifier <" ^ name ^ ">, found code point " ^
                  string_of_int code_point) input)
          | None -> Trampoline.return (None, Make_error.make_error (
                  "Expected identifier <" ^ name ^ ">, found end of input") input)

        let expect_code_point code_point input =
          match Input.get input with
            | Some (same_code_point, input) when code_point = same_code_point ->
                Trampoline.return (Some (code_point, input), Error_info.default_error)
            | Some (other_code_point, input) ->
                Trampoline.return (None, Make_error.make_error (
                  "Expected code point " ^ string_of_int code_point ^
                  ", found code point " ^ string_of_int other_code_point) input)
            | None ->
                Trampoline.return (None, Make_error.make_error (
                  "Expected code point " ^ string_of_int code_point ^
                  ", found end of input") input)

        let expect_code_points code_points input =
          let rec expect_rec code_points acc input =
            match code_points, Input.get input with
              | (code_point :: code_points, Some (same_code_point, input))
                  when code_point = same_code_point ->
                  expect_rec code_points (code_point :: acc) input
              | (code_point :: _, Some (other_code_point, input)) ->
                  Trampoline.return (None, Make_error.make_error (
                    "Expected code point " ^ string_of_int code_point ^
                    ", found code point " ^ string_of_int other_code_point) input)
              | (code_point :: _, None) ->
                  Trampoline.return (None, Make_error.make_error (
                    "Expected code point " ^ string_of_int code_point ^
                    ", found end of input") input)
              | [], _ ->
                  Trampoline.return (Some (List.rev acc, input), Error_info.default_error) in
          expect_rec code_points [] input

        let expect_not_code_point code_points input =
          let rec string_of_code_points = function
            | code_point :: code_points ->
                string_of_int code_point ^ ", " ^ string_of_code_points code_points
            | [] -> "" in
          match Input.get input with
            | Some (other_code_point, input)
                when not (List.mem other_code_point code_points) ->
                Trampoline.return (Some (other_code_point, input), Error_info.default_error)
            | Some (code_point, input) ->
                Trampoline.return (None,
                    Make_error.make_error ("Expected code point not in [" ^
                      string_of_code_points code_points ^ "], found code point " ^
                      string_of_int code_point) input)
            | None ->
                Trampoline.return (None,
                    Make_error.make_error ("Expected code point not in [" ^
                      string_of_code_points code_points ^ "], found end of input") input)

        let expect_string text =
          convert (expect_code_points (Utf8_stream.code_points_of_string text))
            Utf8_stream.string_of_code_points

        let expect_identifier =
          let is_first_character c =
            int_of_char 'a' <= c && c <= int_of_char 'z' ||
            int_of_char '0' <= c && c <= int_of_char '9' in
          let is_later_character c =
            is_first_character c ||
            int_of_char 'A' <= c && c <= int_of_char 'Z' in
          expect_several "identifier" is_first_character is_later_character

        let expect_digit =
          expect "digit" (fun input ->
            match Input.get input with
              | Some (digit, input)
                    when int_of_char '0' <= digit && digit <= int_of_char '9' ->
                  Either.Left (digit - int_of_char '0', input)
              | Some (other, input) ->
                  Either.Right (Make_error.make_error (
                      "Expected digit, found code point " ^ string_of_int other) input)
              | None ->
                  Either.Right (Make_error.make_error (
                      "Expected digit, found end of input") input))

        let expect_natural_number =
          once_or_more_and_fold_left expect_digit
            "" (fun acc digit -> acc ^ string_of_int digit)

        let expect_spaces =
          let is_space c =
            int_of_char ' ' = c || int_of_char '\n' = c ||
            int_of_char '\r' = c || int_of_char '\t' = c in
          expect_several "spaces" is_space is_space

        let optional_spaces_before p =
          second (and_then (optional expect_spaces) p)
      end

end
