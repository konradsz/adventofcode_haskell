open Stdio

let input = Sys.argv.(1) |> In_channel.read_lines

let occurrences =
  Array.init
    (List.hd input |> String.length)
    (fun _ -> Array.init 26 (fun _ -> 0))

let () =
  let count_in_word word =
    String.iteri
      (fun idx c ->
        let letter_idx = int_of_char c - int_of_char 'a' in
        let old_value = occurrences.(idx).(letter_idx) in
        occurrences.(idx).(letter_idx) <- old_value + 1)
      word
  in
  List.iter count_in_word input

let index_of_max_element lst =
  let rec tmp lst current_max current_idx max_idx =
    match lst with
    | [] -> max_idx
    | h :: t ->
        if h > current_max then tmp t h (current_idx + 1) current_idx
        else tmp t current_max (current_idx + 1) max_idx
  in
  tmp lst 0 0 0

let index_of_element pred start lst =
  let rec tmp lst current_max current_idx max_idx =
    match lst with
    | [] -> max_idx
    | h :: t ->
        if pred h current_max then tmp t h (current_idx + 1) current_idx
        else tmp t current_max (current_idx + 1) max_idx
  in
  tmp lst start 0 0

let index_of_most_common_el =
  index_of_element (fun new_value old_value -> new_value > old_value) min_int

let index_of_least_common_el =
  index_of_element
    (fun new_value old_value -> new_value <> 0 && new_value < old_value)
    max_int

let solve f =
  Array.fold_left
    (fun code a ->
      code
      ^ (Array.to_list a |> f
        |> ( + ) (int_of_char 'a')
        |> char_of_int |> String.make 1))
    "" occurrences

let part1 = solve index_of_most_common_el
let part2 = solve index_of_least_common_el
let _ = printf "Part 1: %s\nPart 2: %s\n" part1 part2
