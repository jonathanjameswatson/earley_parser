open Core

module T = struct
  type t = Rewrite_rule.t * Span.t [@@deriving compare, sexp]
end

include T
include Comparator.Make (T)
