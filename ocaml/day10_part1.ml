(** once opam is installed, run with coretop day10_part1.ml **)
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
let corruption_costs = [')',3; ']',57; '}',1197; '>',25137] |> List.to_seq |> CMap.of_seq
let corruption_cost c = CMap.find c corruption_costs
let completion_costs = [')',1; ']',2; '}',3; '>',4] |> List.to_seq |> CMap.of_seq
let completion_cost c = CMap.find c completion_costs
let is_empty xs = (List.length xs) == 0
let is_corrupt = function | (Corrupt,_,_) -> true | _ -> false
let has_stack_drop c stack = (is_closing c) && (List.length stack) > 0 && ((List.hd stack) == c)

(** solving **)
let rec parse_line stack = function
  | [] -> (match stack with
           | [] -> (Complete,0,"")
           | stack -> (Incomplete,0,(stack |> List.to_seq |> String.of_seq)))
  | c::cs -> (if (has_stack_drop c stack) then
                (parse_line (List.tl stack) cs)
              else if (is_closing c) then
                (Corrupt,(corruption_cost c),"")
              else
                (parse_line ((closing_equiv c)::stack) cs))

let add_freq = function
  | (Some v) -> Some (v + 1)
  | None -> Some 1

let compute_freqs freqs = function
  | (Corrupt,cost,_) -> IMap.update cost add_freq freqs
  | _ -> freqs

(** main **)
let () =
  let corruptions = "../resources/input10.txt"
                    |> L.lines_of_file
                    |> List.map String.to_seq
                    |> List.map List.of_seq
                    |> List.map (parse_line [])
                    |> List.fold_left compute_freqs IMap.empty in
  let score = IMap.fold (fun k v res -> res + (k * v)) corruptions 0 in
  printf "day10_part1: %d\n" score
