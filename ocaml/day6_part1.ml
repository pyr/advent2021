(** once opam is installed, run with coretop day6_part1.ml **)
(** https://adventofcode.com/2021/day/6 **)
#require "line_oriented"

open Printf
module L = Line_oriented

(** parsing **)
let parse_timers s =
  s |> String.split_on_char ',' |> List.map int_of_string

(** solving **)
let tick_fish x =
  if x == 0 then [6; 8] else [x - 1]

let rec solver fishes days =
  if days > 0 then
    let fishes' = List.concat_map tick_fish fishes in
    solver fishes' (days - 1)
  else
    List.length fishes

(** main **)
let () =
  let fishes = "../resources/input6.txt" |> L.lines_of_file |> List.hd |> parse_timers in
  let fish_count = solver fishes 80 in
  printf "day6_part1: %d\n" fish_count
