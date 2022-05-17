type t [@@deriving sexp]

val create : Terminal.t array -> Production_rules.t -> Edge.t -> t
val fill_chart : t -> t
val get_parses : t -> Parse.t list
