open Core

type t = Rewrite_rule.t * Span.t [@@deriving compare, sexp]

include Comparator.S with type t := t
