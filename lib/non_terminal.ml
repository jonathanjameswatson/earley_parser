open Core

module T = struct
  type t =
    | S
    | NP
    | VP
    | PP
    | N
    | V
    | P
  [@@deriving compare, sexp]
end

include T
include Comparator.Make (T)
