open Stdio

let input = Sys.argv.(1) |> In_channel.read_lines
let parse line = Str.split (Str.regexp "[][]") line

let has_abba s =
  let lst = List.init (String.length s) (String.get s) in
  let rec abba l =
    match l with
    | a :: b :: c :: d :: t ->
        if a <> b && a == d && b == c then true else abba (b :: c :: d :: t)
    | _ -> false
  in
  abba lst

let partition line =
  let lst = parse line in
  let a, b =
    List.mapi (fun idx el -> (idx mod 2 == 0, el)) lst |> List.partition fst
  in
  (List.map snd a, List.map snd b)

let count_tls_supported lst =
  List.filter
    (fun line ->
      let outside, inside = partition line in
      List.exists has_abba outside && Bool.not (List.exists has_abba inside))
    lst
  |> List.length

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let contains_bab inside bab =
  try
    ignore (List.find (fun l -> contains l bab) inside);
    true
  with Not_found -> false

let has_aba inside s =
  let lst = List.init (String.length s) (String.get s) in
  let rec aba l =
    match l with
    | a :: b :: c :: t ->
        let open Core.Char in
        if
          a <> b && a = c
          && contains_bab inside (to_string b ^ to_string a ^ to_string b)
        then true
        else aba (b :: c :: t)
    | _ -> false
  in
  aba lst

let count_ssl_supported lst =
  List.filter
    (fun line ->
      let outside, inside = partition line in
      List.exists (has_aba inside) outside)
    lst
  |> List.length

let part1 = count_tls_supported input
let part2 = count_ssl_supported input
let _ = printf "Part 1: %d\nPart 2: %d\n" part1 part2
