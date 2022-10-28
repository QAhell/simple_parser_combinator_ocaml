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

  (*val repeat_and_fold_left :
    ('input, 'a) t ->
    'b ->
    ('b -> 'a -> 'b) ->
    ('input, 'b) t

  val repeated :
    ('input, 'output) t ->
    ('input, 'output list) t

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
      sig
        val expect_several :
          string ->
          (int -> bool) ->
          (int -> bool) ->
          (Input.t, string) t

        val expect_code_point : int -> (Input.t, int) t

        val expect_code_points : int list -> (Input.t, int) t

        val expect_not_code_point : int list -> (Input.t, int) t

        val expect_sring : string -> (Input.t, string) t

        val expect_identifier : (Input.t, string) t

        val expect_natural_number : (Input.t, string) t

        val expect_spaces : (Input.t, string) t

        val optional_spaces_before : (Input.t, 'a) t -> (Input.t, 'a) t
      end
      *)

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
end
