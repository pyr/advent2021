(** once opam is installed, run with coretop day8_part2.ml **)
(** https://adventofcode.com/2021/day/8 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Digit = Set.Make(Char)
module Patterns = Map.Make(String)

(** parsing **)
let as_digit elem =
  elem |> String.to_seq |> Digit.of_seq

let as_string d =
  d |> Digit.to_seq |> String.of_seq

let drop n xs =
  let rec inner_drop i = function
    | []  -> []
    | xss -> if i >= n then xss else inner_drop (i + 1) (List.tl xss) in
  inner_drop 0 xs

let take n xs =
  let rec inner_take i res = function
    | []      -> []
    | (x::xs) -> if i >= n then (List.rev res) else inner_take (i + 1) (x::res) xs in
  inner_take 0 [] xs

let sorter x y =
  (Digit.cardinal x) - (Digit.cardinal y)

let parse_line line =
  line
  |> String.split_on_char ' '
  |> List.concat_map (String.split_on_char '|')
  |> List.filter (fun x -> (String.length x) > 0)
  |> List.map as_digit

let superset x y =
  Digit.is_empty (Digit.diff y x)

let range n =
  let rec inner_range res = function
    | 0 -> res
    | i -> inner_range ((i - 1)::res) (i - 1) in
  inner_range [] n

let pow10 n =
  range n |> List.fold_left (fun acc _ -> acc * 10) 1

(** solving **)
let resolve_patterns = function
  | (one::seven::four::p3::p4::p5::p6::p7::p8::eight::[]) ->
     let s6    = [p6;p7;p8] in
     let nine  = s6 |> List.filter (Digit.subset four) |> List.hd in
     let six   = s6 |> List.filter (fun x -> not (Digit.subset one x)) |> List.hd in
     let zero  = s6 |> List.filter (fun x -> ((x != nine) && (x != six))) |> List.hd in

     let s5    = [p3;p4;p5] in
     let three = s5 |> List.filter (Digit.subset one) |> List.hd in
     let five  = s5 |> List.filter (superset six) |> List.hd in
     let two   = s5 |> List.filter (fun x -> ((x != three) && (x != five))) |> List.hd in

     [as_string zero,  0;
      as_string one,   1;
      as_string two,   2;
      as_string three, 3;
      as_string four,  4;
      as_string five,  5;
      as_string six,   6;
      as_string seven, 7;
      as_string eight, 8;
      as_string nine,  9]
     |> List.to_seq
     |> Patterns.of_seq
  | _ -> assert false

let solver line =
  let patterns = line |> take 10 |> List.sort sorter |> resolve_patterns in
  let adder x y = x + y in
  let digits = line |> drop 10 |> List.rev in
  let line_res = List.map2 (fun d n -> (Patterns.find (as_string d) patterns) * (pow10 n))
                   digits
                   (range 4)
                 |> List.fold_left adder 0 in
  line_res

(** main **)
let () =
  let input = "../resources/input8.txt"
              |> L.lines_of_file
              |> List.map parse_line in
  let adder x y = x + y in
  let res = (input |> List.map solver |> List.fold_left adder 0) in
  printf "day8_part2: %d\n" res
