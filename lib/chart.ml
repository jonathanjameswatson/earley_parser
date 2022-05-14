open Core

type t = Set.M(Edge).t * Terminal.t array * Production_rules.t * Non_terminal.t
[@@deriving sexp]

let create sentence production_rules (((end_non_terminal, _), _) as start_edge) =
  ( Set.add (Set.empty (module Edge)) start_edge
  , sentence
  , production_rules
  , end_non_terminal )
;;

let predict (set, sentence, production_rules, end_non_terminal) =
  ( Set.fold set ~init:set ~f:(fun current_set ((_, (_, postfix_string)), (_, j)) ->
        match postfix_string with
        | Meta_symbol.NT non_terminal_1 :: _ ->
          List.fold
            (Production_rules.get_production_rules production_rules non_terminal_1)
            ~init:current_set
            ~f:(fun current_current_set (non_terminal_2, meta_string) ->
              Set.add current_current_set ((non_terminal_2, ([], meta_string)), (j, j)))
        | _ -> current_set)
  , sentence
  , production_rules
  , end_non_terminal )
;;

let scan (set, sentence, production_rules, end_non_terminal) =
  ( Set.fold
      set
      ~init:set
      ~f:(fun current_set ((non_terminal, (prefix_string, postfix_string)), (i, j)) ->
        match prefix_string, postfix_string with
        | [], Meta_symbol.T terminal :: postfix_string_remainder ->
          if j < Array.length sentence && Terminal.compare sentence.(j + 1) terminal = 0
          then
            Set.add
              current_set
              ( ( non_terminal
                , (prefix_string @ [ Meta_symbol.T terminal ], postfix_string_remainder)
                )
              , (i, j + 1) )
          else current_set
        | _ -> current_set)
  , sentence
  , production_rules
  , end_non_terminal )
;;

let complete (set, sentence, production_rules, end_non_terminal) =
  ( Set.fold
      set
      ~init:set
      ~f:(fun set_1 ((non_terminal_1, (prefix_string_1, postfix_string)), (i, k_1)) ->
        match postfix_string with
        | Meta_symbol.NT non_terminal_2 :: postfix_string_remainder ->
          Set.fold
            set
            ~init:set
            ~f:(fun set_2 ((non_terminal_3, (prefix_string_2, _)), (k_2, j)) ->
              match prefix_string_2 with
              | [] ->
                if k_1 = k_2 && Non_terminal.compare non_terminal_2 non_terminal_3 = 0
                then
                  Set.add
                    set_2
                    ( ( non_terminal_1
                      , ( prefix_string_1 @ [ Meta_symbol.NT non_terminal_2 ]
                        , postfix_string_remainder ) )
                    , (i, j) )
                else set_2
              | _ -> set_2)
        | _ -> set_1)
  , sentence
  , production_rules
  , end_non_terminal )
;;

let is_done (set, sentence, _, end_non_terminal) =
  Set.exists set ~f:(fun ((non_terminal, (_, postfix_string)), (i, j)) ->
      match postfix_string with
      | [] ->
        i = 0
        && j = Array.length sentence - 1
        && Non_terminal.compare non_terminal end_non_terminal = 0
      | _ -> false)
;;

let update chart : t = complete (scan (predict chart))

let fill_chart chart =
  let rec fill_chart_util chart last_length =
    let new_chart = update chart in
    is_done new_chart
    ||
    let set, _, _, _ = new_chart in
    let new_length = Set.length set in
    if new_length = last_length then false else fill_chart_util new_chart new_length
  in
  fill_chart_util chart 0
;;
