(** once opam is installed, run with coretop day3_part1.ml **)
(** https://adventofcode.com/2021/day/3 **)
#require "line_oriented"

open Printf
module L = Line_oriented

let print_line l =
  printf "line = %s\n" l

let bitseq s =
    let int_of_char2 c =  (int_of_char c) - (int_of_char '0') in
    s |> String.to_seq |> List.of_seq |> List.map int_of_char2

let int_of_bits bits =
  let rec inner_int_of_bits exp acc = function
    | [] -> acc
    | x::tl -> if x == 0
               then inner_int_of_bits (exp + 1) acc tl
               else inner_int_of_bits (exp + 1) (acc + Int.shift_left 1 exp) tl in
  inner_int_of_bits 0 0 (List.rev bits)

let split_by_bit pos bitseq = (List.nth bitseq pos) == 0

let rating decider bitseqs =
  let rec inner_rating pos = function
    | [final] -> final
    | candidates -> let partitioner = (split_by_bit pos) in
                    let (g1,g2) = List.partition partitioner candidates in
                    let group = decider pos g1 g2 in
                    inner_rating (pos + 1) group in
  inner_rating 0 bitseqs

let msb_decider pos group1 group2 =
  match ((List.length group1) == (List.length group2),
         (List.length group1 < List.length group2)) with
  | (true,_)      -> group2
  | (false,true)  -> group2
  | (false,false) -> group1

let lsb_decider pos group1 group2 =
  match ((List.length group1) == (List.length group2),
         (List.length group1 < List.length group2)) with
  | (true,_)      -> group1
  | (false,true)  -> group1
  | (false,false) -> group2

let () =
  let lines = L.lines_of_file "../resources/input3.txt" in
  let bitseqs = List.map bitseq (List.sort String.compare lines) in
  let oxygen_rating = rating msb_decider bitseqs in
  let co2_rating = rating lsb_decider bitseqs in
  printf "day3_part2: %d\n" ((int_of_bits oxygen_rating) * (int_of_bits co2_rating))
