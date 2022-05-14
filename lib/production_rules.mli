type t [@@deriving sexp]

val empty : t
val add_production_rule : t -> Production_rule.t -> t
val get_production_rules : t -> Non_terminal.t -> Production_rule.t list
