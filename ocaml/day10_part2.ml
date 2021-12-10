(** once opam is installed, run with coretop day10_part2.ml **)
(** https://adventofcode.com/2021/day/10 **)
#require "line_oriented"

open Printf
module L = Line_oriented
module CMap = Map.Make(Char)
module IMap = Map.Make(Int)
module CSet = Set.Make(Char)
type state = Complete | Incomplete | Corrupt

let closers = [']'; ')'; '}'; '>'] |> List.to_seq |> CSet.of_seq
let is_closing c = CSet.mem c closers
let closing_equivs = ['[',']'; '(',')'; '{','}'; '<','>'] |> List.to_seq |> CMap.of_seq
let closing_equiv c = CMap.find c closing_equivs
let completion_costs = [')',1; ']',2; '}',3; '>',4] |> List.to_seq |> CMap.of_seq
let completion_cost c = CMap.find c completion_costs
let is_empty xs = (List.length xs) == 0
let is_corrupt = function | (Corrupt,_,_) -> true | _ -> false
let has_stack_drop c stack = (is_closing c) && (List.length stack) > 0 && ((List.hd stack) == c)

(** solving **)
let rec parse_line stack = function
  | []    -> (match stack with
              | []    -> (Complete,[])
              | stack -> (Incomplete,stack))
  | c::cs -> (if (has_stack_drop c stack) then
                (parse_line (List.tl stack) cs)
              else if (is_closing c) then
                (Corrupt,[])
              else
                (parse_line ((closing_equiv c)::stack) cs))

let get_cost s = List.fold_left (fun acc x -> (acc * 5) + (completion_cost x)) 0 s

let total_completion_cost = function
  | (Incomplete,stack) -> Some (get_cost stack)
  | _                  -> None

(** main **)
let () =
  let costs = "../resources/input10.txt"
              |> L.lines_of_file
              |> List.map String.to_seq
              |> List.map List.of_seq
              |> List.map (parse_line [])
              |> List.filter_map total_completion_cost
              |> List.sort (fun x y -> x - y) in
  let median = ((List.length costs) / 2) in
  let best_cost = List.nth costs median in
  printf "day10_part2: %d\n" best_cost
