(** once opam is installed, run with coretop day6_part2.ml **)
(** https://adventofcode.com/2021/day/6 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Counters = Map.Make(Int)

(** parsing **)
let parse_positions s =
  s |> String.split_on_char ',' |> List.map int_of_string

(** solving **)
let cost_to x y =
  let n = abs (x - y) in
  (n * (n - 1)) / 2

let domain_for positions =
  let minval = List.fold_left (fun x y -> min x y) max_int positions in
  let maxval = List.fold_left (fun x y -> max x y) 0 positions in
  let rec domain_by res curr = if curr > maxval then res else domain_by (curr::res) (curr + 1) in
  domain_by [] minval

let frequencies xs =
  let updater = function
    | (Some x) -> Some (x + 1)
    | None     -> Some 1 in
  List.fold_left (fun acc v -> Counters.update v updater acc) Counters.empty xs

let compute_costs positions destination =
  let updater k v acc = acc + ((cost_to k destination) * v) in
  Counters.fold updater positions 0

let best_position input =
  let domain = domain_for input in
  let positions = frequencies input in
  domain
  |> List.map (compute_costs positions)
  |> List.fold_left (fun x y -> (min x y)) max_int

(** main **)
let () =
  let input = "../resources/input7.txt" |> L.lines_of_file |> List.hd |> parse_positions in
  printf "day7_part2: %d\n" (best_position input)
