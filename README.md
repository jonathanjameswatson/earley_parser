# Earley parser

Earley parse written for formal models of language supervision work.

## Installation

1. Install `ocaml`, `dune`, `core`
2. Run `dune build`
3. Run `dune install`

## Formatting

Run `dune build @fmt` and `dune promote` to format the code

## Usage

```txt
Output whether a given sentence is in the language defined by the toy grammar

  earley_parser INPUT
=== flags ===
  [-build-info]  print info about this build and exit
  [-version]     print the version of this build and exit
  [-help]        print this help text and exit
                 (alias: -?)
```
