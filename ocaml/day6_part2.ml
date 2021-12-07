(** once opam is installed, run with coretop day6_part2.ml **)
(** https://adventofcode.com/2021/day/6 **)
#require "line_oriented"

open Printf
module L = Line_oriented

(** globals **)
let fish_counts = Hashtbl.create 128

(** parsing **)
let parse_timers s =
  s |> String.split_on_char ',' |> List.map int_of_string

(** solving **)
let can_generate (value,days) = days > value

let generations (value,days) =
  if can_generate (value,days) then
    let shift = 6 - value in
    let days' = days + shift in
    let num_gens = (days' / 7) - 1 in
    let shift' = days' mod 7 in
    let rec days_of_gens res counter =
      if counter >= 0 then
        days_of_gens ((shift' + (counter * 7))::res) (counter - 1)
      else
        List.map (fun day -> (8,day)) res in
    days_of_gens [] num_gens
  else
    []

let rec count_fish_cached fish =
  let count_fish fish =
    let gens = generations fish in
    let subs = List.filter can_generate gens in
    let subcounts = List.map count_fish_cached subs in
    let adder x y = x + y in
    let count = List.fold_left adder (List.length gens) subcounts in
    Hashtbl.replace fish_counts fish count;
    count in
  let count = match Hashtbl.find_opt fish_counts fish with
    | (Some count) -> count
    | None -> count_fish fish in
  count

let solver days fishes =
  fishes
  |> List.map (fun value -> (value,days))
  |> List.map count_fish_cached
  |> List.fold_left (fun x y -> x + y) (List.length fishes)

(** main **)
let () =
  let fishes = "../resources/input6.txt" |> L.lines_of_file |> List.hd |> parse_timers in
  let fish_count = solver 256 fishes in
  printf "day6_part2: %d\n" fish_count
