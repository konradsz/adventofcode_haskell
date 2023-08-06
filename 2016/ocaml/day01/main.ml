open Stdio

let input =
  Sys.argv.(1) |> In_channel.read_all |> String.split_on_char ','
  |> List.map String.trim

type direction = North | East | South | West

let turn ins dir =
  match String.get ins 0 with
  | 'L' -> (
      match dir with
      | North -> West
      | East -> North
      | South -> East
      | West -> South)
  | 'R' -> (
      match dir with
      | North -> East
      | East -> South
      | South -> West
      | West -> North)
  | _ -> failwith "bad input"

let get_no_of_steps ins =
  let last = String.length ins - 1 in
  String.sub ins 1 last |> int_of_string

let move (dir, x, y) ins =
  let new_dir = turn ins dir and steps = get_no_of_steps ins in
  match new_dir with
  | North -> (new_dir, x, y + steps)
  | East -> (new_dir, x + steps, y)
  | South -> (new_dir, x, y - steps)
  | West -> (new_dir, x - steps, y)

module VisitedSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let get_walk_positions (x, y) dir steps =
  let pos_list =
    match dir with
    | North -> List.init steps (fun dy -> (x, y + dy))
    | East -> List.init steps (fun dx -> (x + dx, y))
    | South -> List.init steps (fun dy -> (x, y - dy))
    | West -> List.init steps (fun dx -> (x - dx, y))
  in
  List.to_seq pos_list |> VisitedSet.of_seq

let rec find_duplicate visited (dir, x, y) input =
  let ins = List.hd input in
  let new_dir = turn ins dir and steps = get_no_of_steps ins in
  let walk_positions = get_walk_positions (x, y) new_dir steps in
  let overlap = VisitedSet.inter visited walk_positions in
  match VisitedSet.choose_opt overlap with
  | Some (x, y) -> abs x + abs y
  | None ->
      let next = move (dir, x, y) ins
      and all_visited = VisitedSet.union visited walk_positions in
      find_duplicate all_visited next (List.tl input)

let part1 =
  let _, x, y = List.fold_left move (North, 0, 0) input in
  abs x + abs y

let part2 = find_duplicate VisitedSet.empty (North, 0, 0) input
let () = printf "Part 1: %d\nPart 2: %d\n" part1 part2
