open Core

type t =
  | S
  | NP
  | VP
  | PP
  | N
  | V
  | P
[@@deriving compare, sexp]

include Comparator.S with type t := t
