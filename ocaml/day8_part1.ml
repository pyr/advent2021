(** once opam is installed, run with coretop day8_part1.ml **)
(** https://adventofcode.com/2021/day/8 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Counters = Map.Make(Int)
module Digit = Set.Make(Char)

(** parsing **)
let as_digit elem = elem |> String.to_seq |> Digit.of_seq

let drop n xs =
  let rec inner_drop i = function
    | []  -> []
    | xss -> if i >= n then xss else inner_drop (i + 1) (List.tl xss) in
  inner_drop 0 xs

let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.concat_map (String.split_on_char '|')
  |> List.filter (fun x -> (String.length x) > 0)
  |> List.map as_digit

(** solving **)
let solver line =
  let valid_length x = (x == 2) || (x == 3) || (x == 4) || (x == 7) in
  line
  |> drop 10
  |> List.filter (fun x -> valid_length (Digit.cardinal x))
  |> List.length

(** main **)
let () =
  let input = "../resources/input8.txt"
              |> L.lines_of_file
              |> List.map parse_line in
  let adder x y = x + y in
  printf "day8_part1: %d\n" (input |> List.map solver |> List.fold_left adder 0);
