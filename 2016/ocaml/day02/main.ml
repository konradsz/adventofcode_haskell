open Stdio

module Keypad = Map.Make (struct
  type t = int * int

  let compare = compare
end)

(* let keypad =
   let open Keypad in
   empty
   |> add (0, 0) "1"
   |> add (1, 0) "2"
   |> add (2, 0) "3"
   |> add (0, 1) "4"
   |> add (1, 1) "5"
   |> add (2, 1) "6"
   |> add (0, 2) "7"
   |> add (1, 2) "8"
   |> add (2, 2) "9" *)

let keypad =
  let open Keypad in
  empty
  |> add (2, 0) "1"
  |> add (1, 1) "2"
  |> add (2, 1) "3"
  |> add (3, 1) "4"
  |> add (0, 2) "5"
  |> add (1, 2) "6"
  |> add (2, 2) "7"
  |> add (3, 2) "8"
  |> add (4, 2) "9"
  |> add (1, 3) "A"
  |> add (2, 3) "B"
  |> add (3, 3) "C"
  |> add (2, 4) "D"

let input = Sys.argv.(1) |> In_channel.read_lines

let move_by (dx, dy) (x, y) =
  let new_pos = (x + dx, y + dy) in
  if Keypad.mem new_pos keypad then new_pos else (x, y)

let move_down = move_by (0, 1)
let move_up = move_by (0, -1)
let move_right = move_by (1, 0)
let move_left = move_by (-1, 0)

let move_ins pos dir =
  match dir with
  | 'D' -> move_down pos
  | 'U' -> move_up pos
  | 'R' -> move_right pos
  | 'L' -> move_left pos
  | _ -> failwith "bad input"

let process_instruction pos ins = String.fold_left move_ins pos ins

let pp (str, pos) ins =
  let new_pos = process_instruction pos ins in
  (str ^ Keypad.find new_pos keypad, new_pos)

let code = List.fold_left pp ("", (1, 1)) input |> fst
let () = code |> print_string
