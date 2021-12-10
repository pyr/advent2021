(** once opam is installed, run with coretop day9_part2.ml **)
(** https://adventofcode.com/2021/day/9 **)
#require "line_oriented"

open Printf
module L = Line_oriented
let compare_pos (x,y) (x',y') =
  let xdiff = x - x' in
  if (xdiff == 0) then y - y' else xdiff
module HeightMap = Map.Make(
                       struct
                         type t = (int * int)
                         let compare = compare_pos
                       end)
(** parsing **)

(** solving **)
let range n =
  let rec inner_range res = function
    | 0 -> res
    | i -> inner_range ((i - 1)::res) (i - 1) in
  inner_range [] n

let build_heightmap lines =
  let height c = (Char.code c) - (Char.code '0') in
  let as_pos y x c = ((x,y),(height c)) in
  let line_as_positions y line =
    line
    |> String.to_seq
    |> List.of_seq
    |> List.map2 (as_pos y) (range (String.length line)) in
  let line_range = lines |> List.length |> range in
  lines
  |> List.map2 line_as_positions line_range
  |> List.concat_map (fun x -> x)
  |> List.to_seq
  |> HeightMap.of_seq

let height_at heightmap pos = match (HeightMap.find_opt pos heightmap) with
  | (Some x) -> x
  | None -> 10

let upward_height find base_height pos =
  (find pos) <= base_height

let is_low_point find adjacents base_pos base_height =
  let near_positions = adjacents base_pos in
  (near_positions
   |> List.filter (upward_height find base_height)
   |> List.length) == 0

let rec explore_basin find adjacents pos =
  let nearby_positions = adjacents pos in
  let is_eligible x = ((find pos) < (find x)) && ((find x) < 9) in
  nearby_positions
  |> List.filter is_eligible
  |> List.concat_map (explore_basin find adjacents)
  |> List.fold_left (fun acc x -> x::acc) [pos]
  |> List.sort_uniq compare_pos

(** main **)
let () =
  let lines = "../resources/input9.txt" |> L.lines_of_file in
  let max_x = String.length (List.hd lines) in
  let max_y = List.length lines in
  let is_valid (x,y) = ((x >= 0) && (x < max_x) && (y >= 0) && (y < max_y)) in
  let adjacents_for (x,y) = [(x-1),y; (x+1),y; x,(y-1); x,(y+1)] in
  let adjacents pos = pos |> adjacents_for |> List.filter is_valid in
  let heights = build_heightmap lines in
  let low_points = heights
                   |> HeightMap.filter (is_low_point (height_at heights) adjacents)
                   |> HeightMap.bindings in
  let basins = low_points
               |> List.map fst
               |> List.map (explore_basin (height_at heights) adjacents)
               |> List.map List.length
               |> List.sort (fun x y -> y - x) in
  let score = match basins with
    | (a::b::c::_) -> a * b * c
    | _ -> -1 in
  printf "day9_part2: %d\n" score
