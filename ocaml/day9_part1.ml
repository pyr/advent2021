(** once opam is installed, run with coretop day9_part1.ml **)
(** https://adventofcode.com/2021/day/9 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module HeightMap = Map.Make(
                       struct
                         type t = (int * int)
                         let compare (x,y) (x',y') =
                           let xdiff = x - x' in
                           if (xdiff == 0) then y - y' else xdiff
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

let upward_height heightmap base_height pos =
  (height_at heightmap pos) <= base_height

let is_low_point heightmap adjacents base_pos base_height =
  let near_positions = adjacents base_pos in
  (near_positions
   |> List.filter (upward_height heightmap base_height)
   |> List.length) == 0

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
                   |> HeightMap.filter (is_low_point heights adjacents)
                   |> HeightMap.bindings in
  let adder x y = x + y + 1 in

  (**
  (** show the matrix for debugging purposes **)

  let width = lines |> List.hd |> String.length in
  List.iter (fun y ->
      List.iter (fun x -> let v = HeightMap.find (x,y) heights in
                          let low_point = is_low_point heights adjacents (x,y) v in
                          printf (if low_point then "[%d]" else " %d ") v)
        (range width);
      printf "\n")
    (range (List.length lines));
   **)
  printf "day9_part1: %d\n" (low_points |> List.map snd |> List.fold_left adder 0)
