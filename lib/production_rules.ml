open Core

type t = Meta_string.t list Map.M(Non_terminal).t [@@deriving sexp]

let empty = Map.empty (module Non_terminal)

let add_production_rule production_rules (non_terminal, meta_string) =
  (Map.add_multi production_rules ~key:non_terminal ~data:meta_string :> t)
;;

let get_production_rules production_rules non_terminal =
  List.map (Map.find_multi production_rules non_terminal) ~f:(fun meta_string ->
      non_terminal, meta_string)
;;
