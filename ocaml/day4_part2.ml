(** once opam is installed, run with coretop day4_part2.ml **)
(** https://adventofcode.com/2021/day/4 **)
#require "line_oriented"

open Printf
module L = Line_oriented

(** types **)
type input = (int * int) list

type cell = { pos: int; value: int; mark: bool; }

type board = { cells: cell list; winner: int option; }

let parse_input line =
  let cmds = String.split_on_char ',' line in
  let as_tuple i v = (i,int_of_string v) in
  List.mapi as_tuple cmds

(** parsing primitives **)
let rec partition6 res = function
  | (_::v::w::x::y::z::tl) -> partition6 ([v;w;x;y;z]::res) tl
  | _                      -> List.rev res

let parse_board lines =
  let splitter = String.split_on_char ' ' in
  let valid_cell v = not (String.equal "" v) in
  let make_cell i v = { pos = i; value = (int_of_string v); mark = false; } in
  let cells = lines
              |> List.concat_map splitter
              |> List.filter valid_cell
              |> List.mapi make_cell in
  { cells = cells; winner = None }

(** solving logic **)
let winning_row cells pos =
  let row = List.filter (fun x -> (x.pos mod 5) == (pos mod 5)) cells in
  match List.find_opt (fun x -> not x.mark) row with
  | (Some _) -> false
  | None     -> true

let winning_col cells pos =
  let row = List.filter (fun x -> (x.pos / 5) == (pos / 5)) cells in
  match List.find_opt (fun x -> not x.mark) row with
  | (Some _) -> false
  | None     -> true

let new_winner cells pos =
  (winning_row cells pos) || (winning_col cells pos)

let mark_cell value cell =
  if (value == cell.value)
  then {cell with mark = true}
  else cell

let mark_board board (pos,value) =
  let cells = List.map (mark_cell value) board.cells in
  let markcell = List.find_opt (fun x -> x.value == value) board.cells in
  match markcell with
  | (Some cell) -> if (new_winner cells cell.pos)
                   then {cells = cells; winner = Some cell.value}
                   else {board with cells = cells}
  | None        -> board

let process_input boards cmd =
  let process_input_for_board board =
    match board.winner  with
    | (Some x) -> board
    | None     -> mark_board board cmd in
  List.map process_input_for_board boards

let board_pos {winner; _} =
  match winner with
    | (Some f) -> f
    | None     -> 0

let input_pos input {winner; _} =
  let win_value = match winner with
    | (Some f) -> f
    | None     -> 0 in
  let (i,_) = List.find (fun (_,v) -> v == win_value) input in
  i

let board_unmarked {cells; _} =
  cells
  |> List.filter (fun x -> not x.mark)
  |> List.map (fun x -> x.value)
  |> List.fold_left (fun x y -> x + y) 0

let score board =
  (board_pos board) * (board_unmarked board)

let is_winner {winner; _} =
  match winner with
  | (Some _) -> true
  | None     -> false

(** main **)
let () =
  let input_lines = L.lines_of_file "../resources/input4.txt" in
  let input = input_lines
              |> List.hd
              |> parse_input in
  let boards = input_lines
               |> List.tl
               |> partition6 []
               |> List.map parse_board in
  let winner = input
               |> List.fold_left process_input boards
               |> List.filter is_winner
               |> List.sort (fun x y -> (input_pos input y) - (input_pos input x))
               |> List.hd in
  printf "day4_part2: %d\n" (score winner)
