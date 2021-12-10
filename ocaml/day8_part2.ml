(** once opam is installed, run with coretop day8_part2.ml **)
(** https://adventofcode.com/2021/day/8 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module Digit = Set.Make(Char)
module Patterns = Map.Make(Digit)

(** parsing **)
let as_digit elem =
  elem |> String.to_seq |> Digit.of_seq

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

let rec pow10 = function
  | 0 -> 1
  | n -> 10 * (pow10 (n - 1))

let superset x y =
  Digit.is_empty (Digit.diff y x)

let range n =
  let rec inner_range res = function
    | 0 -> res
    | i -> inner_range (n::res) (i - 1) in
  inner_range [] n

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
     [zero,0;one,1;two,2;three,3;four,4;five,5;six,6;seven,7;eight,8;nine,9]
     |> List.to_seq |> Patterns.of_seq
  | _ -> assert false

let as_string d =
  d |> Digit.to_seq |> String.of_seq

let print_line patterns digits res =
  let plist = patterns |> Patterns.to_seq |> List.of_seq in

  let finder x =
    match (plist |> List.filter (fun (k,v) -> x == v)) with
    | [] -> Digit.empty
    | (x::_) -> (fst x) in

  let zero = finder 0 in
  let one = finder 1 in
  let two = finder 2 in
  let three= finder 3 in
  let four = finder 4 in
  let five = finder 5 in
  let six = finder 6 in
  let seven = finder 7 in
  let eight = finder 8 in
  let nine = finder 9 in
  let d1 = List.nth digits 3 in
  let d2 = List.nth digits 2 in
  let d3 = List.nth digits 1 in
  let d4 = List.nth digits 0 in
  printf " %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7d\n"
    (as_string zero)
    (as_string one)
    (as_string two)
    (as_string three)
    (as_string four)
    (as_string five)
    (as_string six)
    (as_string seven)
    (as_string eight)
    (as_string nine)
    (as_string d1)
    (as_string d2)
    (as_string d3)
    (as_string d4)
    res

let solver line =
  let patterns = line |> take 10 |> List.sort sorter |> resolve_patterns in
  let adder x y = x + y in
  let digits = line |> drop 10 |> List.rev in
  let line_res = List.map2 (fun digit n -> (Patterns.find digit patterns) * (pow10 n))
                   digits
                   (List.rev (range 4))
                 |> List.fold_left adder 0 in
  print_line patterns digits line_res;
  line_res

(** main **)
let () =
  let input = "../resources/input8_sample.txt"
              |> L.lines_of_file
              |> List.map parse_line in
  let adder x y = x + y in
  printf " %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s | %7s\n" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "w" "x" "y" "z" "res";
  printf "---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------\n";
  let res = (input |> List.map solver |> List.fold_left adder 0) in
  printf "day8_part2: %d\n" res
