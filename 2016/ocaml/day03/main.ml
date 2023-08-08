open Stdio

let parse line = Str.split (Str.regexp " +") line |> List.map int_of_string
let input = Sys.argv.(1) |> In_channel.read_lines |> List.map parse
let is_valid a b c = a + b > c && a + c > b && b + c > a
let int_of_bool = function true -> 1 | false -> 0

let count_valid_in_rows lst =
  List.filter
    (fun line ->
      match line with
      | [ a; b; c ] -> is_valid a b c
      | _ -> failwith "bad input")
    lst
  |> List.length

let rec count_valid_in_columns lst =
  match lst with
  | [ a1; a2; a3 ] :: [ b1; b2; b3 ] :: [ c1; c2; c3 ] :: t ->
      (is_valid a1 b1 c1 |> int_of_bool)
      + (is_valid a2 b2 c2 |> int_of_bool)
      + (is_valid a3 b3 c3 |> int_of_bool)
      + count_valid_in_columns t
  | [] -> 0
  | _ -> failwith "bad input"

let part1 = count_valid_in_rows input
let part2 = count_valid_in_columns input
let _ = printf "Part 1: %d\nPart 2: %d\n" part1 part2
