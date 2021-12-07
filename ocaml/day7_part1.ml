(** once opam is installed, run with coretop day6_part2.ml **)
(** https://adventofcode.com/2021/day/6 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Distances = Map.Make(Int)

(** parsing **)
let parse_positions s =
  s |> String.split_on_char ',' |> List.map int_of_string

(** solving **)
let distance_between x y = (max x y) - (min x y)

let single_budget start_pos domain =
  let updater m k = Distances.add k (distance_between start_pos k) m in
  List.fold_left updater Distances.empty domain

let all_budgets domain positions =
  List.map (fun pos -> single_budget pos domain) positions

let update_budget k v budgets =
  let updater = function
    | (Some x) -> (Some (x + v))
    | None -> (Some v) in
  Distances.update k updater budgets

let combine_budget budgets budget =
  Distances.fold update_budget budget budgets

let combine_budgets budgets =
  List.fold_left combine_budget Distances.empty budgets

let domain_for positions =
  let minval = List.fold_left (fun x y -> min x y) max_int positions in
  let maxval = List.fold_left (fun x y -> max x y) 0 positions in
  let rec domain_by res curr = if curr > maxval then res else domain_by (curr::res) (curr + 1) in
  domain_by [] minval

let best_position domain positions=
  all_budgets domain positions
  |> combine_budgets
  |> Distances.bindings
  |> List.map snd
  |> List.fold_left (fun x y -> (min x y)) max_int

(** main **)
let () =
  let positions = "../resources/input7.txt" |> L.lines_of_file |> List.hd |> parse_positions in
  let domain = domain_for positions in
  printf "day7_part1: %d\n" (best_position domain positions)
