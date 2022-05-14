open Core
open Earley_parser

let handle_input input =
  let terminals = Array.of_list (String.split input ~on:' ') in
  let production_rules =
    List.fold
      [ ( Non_terminal.S
        , [ [ Meta_symbol.NT Non_terminal.NP; Meta_symbol.NT Non_terminal.VP ] ] )
      ; ( Non_terminal.NP
        , [ [ Meta_symbol.NT Non_terminal.N; Meta_symbol.NT Non_terminal.PP ]
          ; [ Meta_symbol.NT Non_terminal.N ]
          ] )
      ; ( Non_terminal.PP
        , [ [ Meta_symbol.NT Non_terminal.P; Meta_symbol.NT Non_terminal.NP ] ] )
      ; ( Non_terminal.VP
        , [ [ Meta_symbol.NT Non_terminal.VP; Meta_symbol.NT Non_terminal.PP ]
          ; [ Meta_symbol.NT Non_terminal.V; Meta_symbol.NT Non_terminal.VP ]
          ; [ Meta_symbol.NT Non_terminal.V; Meta_symbol.NT Non_terminal.NP ]
          ; [ Meta_symbol.NT Non_terminal.V ]
          ] )
      ; ( Non_terminal.N
        , [ [ Meta_symbol.T "can" ]
          ; [ Meta_symbol.T "fish" ]
          ; [ Meta_symbol.T "rivers" ]
          ] )
      ; Non_terminal.P, [ [ Meta_symbol.T "in" ] ]
      ; Non_terminal.V, [ [ Meta_symbol.T "can" ]; [ Meta_symbol.T "fish" ] ]
      ]
      ~init:Production_rules.empty
      ~f:(fun production_rules (non_terminal, meta_strings) ->
        List.fold
          meta_strings
          ~init:production_rules
          ~f:(fun production_rules meta_string ->
            Production_rules.add_production_rule
              production_rules
              (non_terminal, meta_string)))
  in
  let chart =
    Chart.create
      terminals
      production_rules
      ( ( Non_terminal.S
        , ([], [ Meta_symbol.NT Non_terminal.NP; Meta_symbol.NT Non_terminal.VP ]) )
      , (0, 0) )
  in
  let result = Chart.fill_chart chart in
  Out_channel.print_string
    (if result then "Sentence in toy grammar" else "Sentence not in toy grammar");
  Out_channel.newline stdout
;;

let command =
  Command.basic
    ~summary:
      "Output whether a given sentence is in the language defined by the toy grammar"
    Command.Let_syntax.(
      let%map_open input = anon ("input" %: string) in
      fun () -> handle_input input)
;;

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
