(** once opam is installed, run with coretop day6_part2.ml **)
(** https://adventofcode.com/2021/day/6 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Counters = Map.Make(Int)

(** parsing **)
let parse_timers s =
  s |> String.split_on_char ',' |> List.map int_of_string

(** solving **)
let tick k v m =
  let updater = function
    | (Some x) -> Some (x + v)
    | None     -> Some v in

  if k == 0 then
    m |> Counters.update 6 updater |> Counters.update 8 updater
  else
    Counters.update (k - 1) updater m

let frequencies fishes =
  let updater = function
    | (Some x) -> Some (x + 1)
    | None     -> Some 1 in
  List.fold_left (fun acc v -> Counters.update v updater acc) Counters.empty fishes

let solver days input =
  let rec ticker day fishes =
    if day < days then
      ticker (day + 1) (Counters.fold tick fishes Counters.empty)
    else
      Counters.fold (fun _ v acc -> v + acc) fishes 0 in
  ticker 0 (frequencies input)

(** main **)
let () =
  let fishes = "../resources/input6.txt" |> L.lines_of_file |> List.hd |> parse_timers in
  let fish_count = solver 80 fishes in
  printf "day6_part1: %d\n" fish_count
