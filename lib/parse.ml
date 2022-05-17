open Core

type t =
  | T of Terminal.t
  | NT of Non_terminal.t * t list
[@@deriving sexp]