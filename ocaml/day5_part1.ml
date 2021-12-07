(** once opam is installed, run with coretop day5_part1.ml **)
(** https://adventofcode.com/2021/day/5 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module C = Core

(** parsing **)
let print_vent_line (x1,y1,x2,y2) =
  printf "%2d %2d -> %2d %2d\n" x1 y1 x2 y2

let parse_pos s =
  let nums = s |> String.split_on_char ',' |> List.map int_of_string in
  match nums with
  | (x::y::[]) -> (x,y)
  | _          -> assert false

let parse_vent_line line =
  let start_pos = line |> String.split_on_char '-' |> List.hd |> String.trim in
  let end_pos = line |> String.split_on_char '>' |> List.rev |> List.hd |> String.trim in
  let (x1,y1) = parse_pos start_pos in
  let (x2,y2) = parse_pos end_pos in
  (x1,y1,x2,y2)

(** solving **)
let is_row_or_col (x1,y1,x2,y2) = ((x1 == x2) || (y1 == y2))

let as_coords (x1,y1,x2,y2) =
  if (y1 == y2) then
    C.List.range (min x1 x2) ((max x1 x2) + 1)
    |> List.map (fun x -> (x,y1))
  else
    C.List.range (min y1 y2) ((max y1 y2) + 1)
    |> List.map (fun y -> (x1,y))

let mark_position positions coordinates =
  let count = match Hashtbl.find_opt positions coordinates with
    | (Some x) -> x + 1
    | None     -> 1 in
  Hashtbl.replace positions coordinates count;
  positions

(** main **)
let () =
  let input = L.map "../resources/input5.txt" parse_vent_line in
  let positions = Hashtbl.create 128 in
  let vent_count = input
                   |> List.filter is_row_or_col
                   |> List.concat_map as_coords
                   |> List.fold_left mark_position positions
                   |> Hashtbl.to_seq_values
                   |> List.of_seq
                   |> List.filter (fun count -> count >= 2)
                   |> List.length in
  printf "day5_part1: %d\n" vent_count
