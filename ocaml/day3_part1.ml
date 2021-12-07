(** once opam is installed, run with coretop day3_part1.ml **)
(** https://adventofcode.com/2021/day/3 **)
#require "line_oriented"

open Printf
module L = Line_oriented

let print_line l =
  printf "line = %s\n" l

let bitwidth = function
  | (x::tl) -> List.length x
  | _ -> assert false

let bitseq s =
    let int_of_char2 c =  (int_of_char c) - (int_of_char '0') in
    s |> String.to_seq |> List.of_seq |> List.map int_of_char2

let make_num x =
  let scat s1 s2 = s1 ^ s2 in
  List.fold_left scat "" (List.map string_of_int x)

let init_acc width =
  let make_zero _ = 0 in List.init width make_zero

let combine_bitseqs line_count x y =
  let adder x y = x + y in List.map2 adder x y

let int_of_bits bits =
  let rec inner_int_of_bits exp acc = function
    | [] -> acc
    | x::tl -> if x == 0
               then inner_int_of_bits (exp + 1) acc tl
               else inner_int_of_bits (exp + 1) (acc + Int.shift_left 1 exp) tl in
  inner_int_of_bits 0 0 (List.rev bits)

let () =
  let lines = L.lines_of_file "../resources/input3.txt" in
  let line_count = List.length lines in
  let bitseqs = List.map bitseq (List.sort String.compare lines) in
  let width = bitwidth bitseqs in
  let bit_occurences = List.fold_left (combine_bitseqs line_count) (init_acc width) bitseqs in
  let to_bit x = if (line_count - x) <= (line_count / 2) then 0 else 1 in
  let epsilon = List.map to_bit bit_occurences in
  let flip_bit x = (x + 1) mod 2 in
  let gamma = List.map flip_bit epsilon in
  printf "day3_part1: %d\n" ((int_of_bits epsilon) * (int_of_bits gamma))
