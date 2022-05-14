type t =
  | T of Terminal.t
  | NT of Non_terminal.t
[@@deriving compare, sexp]
