open Core

module X = struct
  module Y = struct
    type t = Edge.t list [@@deriving compare, sexp]
  end

  include Y
  include Comparator.Make (Y)
end

module W = struct
  module Z = struct
    type t = Set.M(X).t [@@deriving compare, sexp]
  end

  include Z
  include Comparator.Make (Z)
end

type t =
  Set.M(Edge).t
  * Terminal.t array
  * Production_rules.t
  * Non_terminal.t
  * W.t Map.M(Edge).t
[@@deriving sexp]

let create sentence production_rules (((end_non_terminal, _), _) as start_edge) =
  (( Set.add (Set.empty (module Edge)) start_edge
   , sentence
   , production_rules
   , end_non_terminal
   , Map.empty (module Edge) )
    :> t)
;;

let predict (set, sentence, production_rules, end_non_terminal, histories) =
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
  , end_non_terminal
  , histories )
;;

let scan (set, sentence, production_rules, end_non_terminal, histories) =
  ( Set.fold
      set
      ~init:set
      ~f:(fun current_set ((non_terminal, (prefix_string, postfix_string)), (i, j)) ->
        match prefix_string, postfix_string with
        | [], Meta_symbol.T terminal :: postfix_string_remainder ->
          if j < Array.length sentence && Terminal.compare sentence.(j) terminal = 0
          then (
            let new_edge =
              ( (non_terminal, ([ Meta_symbol.T terminal ], postfix_string_remainder))
              , (i, j + 1) )
            in
            Set.add current_set new_edge)
          else current_set
        | _ -> current_set)
  , sentence
  , production_rules
  , end_non_terminal
  , histories )
;;

let complete (set, sentence, production_rules, end_non_terminal, histories) =
  let new_set, new_histories =
    Set.fold
      set
      ~init:(set, histories)
      ~f:(fun
           (set_1, histories_1)
           (((non_terminal_1, (prefix_string, postfix_string_1)), (i, k_1)) as edge_1)
         ->
        match postfix_string_1 with
        | Meta_symbol.NT non_terminal_2 :: postfix_string_remainder ->
          Set.fold
            set_1
            ~init:(set_1, histories_1)
            ~f:(fun
                 (set_2, histories_2)
                 (((non_terminal_3, (_, postfix_string_2)), (k_2, j)) as edge_2)
               ->
              match postfix_string_2 with
              | [] ->
                if k_1 = k_2 && Non_terminal.compare non_terminal_2 non_terminal_3 = 0
                then (
                  let edge_3 =
                    ( ( non_terminal_1
                      , ( prefix_string @ [ Meta_symbol.NT non_terminal_2 ]
                        , postfix_string_remainder ) )
                    , (i, j) )
                  in
                  ( Set.add set_2 edge_3
                  , Map.set
                      histories_2
                      ~key:edge_3
                      ~data:
                        (let edge_1_paths =
                           match Map.find histories_2 edge_1 with
                           | Some history_set -> history_set
                           | None -> Set.singleton (module X) []
                         in
                         let new_history_paths =
                           Set.map
                             (module X)
                             edge_1_paths
                             ~f:(fun edge_1_path -> edge_2 :: edge_1_path)
                         in
                         match Map.find histories_2 edge_3 with
                         | Some history_set -> Set.union history_set new_history_paths
                         | None -> new_history_paths) ))
                else set_2, histories_2
              | _ -> set_2, histories_2)
        | _ -> set_1, histories_1)
  in
  new_set, sentence, production_rules, end_non_terminal, new_histories
;;

let update chart : t = complete (scan (predict chart))

let fill_chart chart =
  let rec fill_chart_util chart last_length =
    let new_chart = update chart in
    let set, _, _, _, _ = new_chart in
    let new_length = Set.length set in
    if new_length = last_length then new_chart else fill_chart_util new_chart new_length
  in
  fill_chart_util chart 0
;;

let get_parses (set, sentence, _, end_non_terminal, histories) =
  match
    Set.find set ~f:(fun ((non_terminal, (_, postfix_string)), (i, j)) ->
        match postfix_string with
        | [] ->
          i = 0
          && j = Array.length sentence
          && Non_terminal.compare non_terminal end_non_terminal = 0
        | _ -> false)
  with
  | None -> []
  | Some edge ->
    let rec count_parses_util (((non_terminal, (prefix_string, _)), _) as edge) =
      match Map.find histories edge with
      | None ->
        (match prefix_string with
        | Meta_symbol.T terminal :: _ -> [ Parse.T terminal ]
        | _ -> [])
      | Some edge_paths ->
        Set.fold edge_paths ~init:[] ~f:(fun parses edge_path ->
            List.append
              parses
              (let path_parse_lists =
                 List.fold edge_path ~init:[ [] ] ~f:(fun path_parse_lists edge ->
                     let inner_parses = count_parses_util edge in
                     List.fold
                       path_parse_lists
                       ~init:[]
                       ~f:(fun path_parse_lists path_parse_list ->
                         List.append
                           path_parse_lists
                           (List.fold
                              inner_parses
                              ~init:[]
                              ~f:(fun path_parse_lists inner_parse ->
                                (inner_parse :: path_parse_list) :: path_parse_lists))))
               in
               List.map path_parse_lists ~f:(fun path_parse_list ->
                   Parse.NT (non_terminal, path_parse_list))))
    in
    count_parses_util edge
;;