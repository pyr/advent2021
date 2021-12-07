(** once opam is installed, run with coretop day1_part1.ml **)
(** https://adventofcode.com/2021/day/1 **)
#require "line_oriented"

open Printf
module L = Line_oriented

let growth_factor x y = if (y - x) > 0 then 1 else 0

let rec count_growing acc = function
  | [] | [_]     -> acc
  | x :: y :: tl -> count_growing (acc + growth_factor x y) (y::tl)

let () =
  let depths = L.map "../resources/input1.txt" int_of_string in
  printf "day1_part1: %d\n" (count_growing 0 depths)
