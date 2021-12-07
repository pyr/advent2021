(** once opam is installed, run with coretop day2_part2.ml **)
(** https://adventofcode.com/2021/day/2 **)
#require "line_oriented"

open Printf
module L = Line_oriented

type command = | Forward | Down | Up

let command_of_string s =
  if      s = "forward" then Forward
  else if s = "down"    then Down
  else if s = "up"      then Up
  else                       assert false

let parse_line line =
  match String.split_on_char ' ' line with
  | [command;amount] -> (command_of_string command, int_of_string amount)
  |  _               -> assert false

let process_command (aim,pos,depth) line =
  match (parse_line line) with
  | (Forward,amount) -> (aim, pos + amount, depth + (amount * aim))
  | (Up,amount)      -> (aim - amount, pos, depth)
  | (Down,amount)    -> (aim + amount, pos, depth)

let compute_pos_depth lines =
  let (_, pos,depth) = List.fold_left process_command (0,0,0) lines in
  (pos * depth)

let () =
  let lines = L.lines_of_file "../resources/input2.txt" in
  printf "day2_part2: %d\n" (compute_pos_depth lines)
