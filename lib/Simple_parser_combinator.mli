(** error information for parsers *)
module type Error_info =
sig

  (** the type of the error information *)
  type t

  (** an empty error message *)
  val default_error : t

  (** returns true iff the error is the default error and not a real error *)
  val is_default_error : t -> bool

  (** returns true iff the error is a real error and not the default error *)
  val is_real_error : t -> bool

  (** if two alternative parses fail, their errors have to be merged somehow *)
  val merge : t -> t -> t
end

(** TODO: copy comments from Kotlin version *)

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
      end *)

end

module Parser_combinator_type :
  functor (Trampoline : Trampoline.Trampoline) ->
  functor (Error_info : Error_info) ->
    sig
      module type T =
        Parser_combinator with
          module Trampoline = Trampoline and
          module Error_info = Error_info
    end

module Recursive_descent_parser :
  functor (Trampoline : Trampoline.Trampoline) ->
  functor (Error_info : Error_info) ->
    Parser_combinator_type (Trampoline) (Error_info).T
