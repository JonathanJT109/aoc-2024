open Core

let string_to_int list = List.map list ~f:(fun item -> Int.of_string item)

let part_a filename =
  let lines =
    let x = Aoc.read_lines filename in
    List.map x ~f:(fun line -> String.split line ~on:' ' |> string_to_int)
  in
  let unsafe = ref 0 in
  List.iter lines ~f:(fun line ->
    let safe = ref true in
    let order = ref 1 in
    let i = ref 1 in
    while !safe && !i < List.length line do
      let diff =
        Option.value (List.nth line !i) ~default:0
        - Option.value (List.nth line (!i - 1)) ~default:0
      in
      if !i = 1 then if diff < 0 then order := -1;
      let diff = diff * !order in
      Printf.printf "%d " diff;
      safe := if diff >= 1 && diff <= 3 then true else false;
      i := !i + 1
    done;
    unsafe := !unsafe + if !safe then 0 else 1;
    if !safe then Printf.printf "Safe\n" else Printf.printf "Unsafe\n");
  Printf.printf "Safe: %d\n" (List.length lines - !unsafe)
;;

let remove_nth list n = List.filteri list ~f:(fun i _ -> i <> n)

let rec check line n =
  let safe = ref true in
  let order = ref 1 in
  let i = ref 1 in
  (* List.iter line ~f:(fun item -> Printf.printf "%d " item); *)
  let new_line = if n <> -1 then remove_nth line n else line in
  (* print_string " -> "; *)
  (* List.iter new_line ~f:(fun item -> Printf.printf "%d " item); *)
  (* print_endline ""; *)
  while !safe && !i < List.length new_line do
    let diff =
      Option.value (List.nth new_line !i) ~default:0
      - Option.value (List.nth new_line (!i - 1)) ~default:0
    in
    if !i = 1 then if diff < 0 then order := -1;
    let diff = diff * !order in
    safe := if diff >= 1 && diff <= 3 then true else false;
    i := !i + 1
  done;
  if !safe then true else if n < List.length line then check line (n + 1) else false
;;

let part_b filename =
  let lines =
    let x = Aoc.read_lines filename in
    List.map x ~f:(fun line -> String.split line ~on:' ' |> string_to_int)
  in
  let safe = ref 0 in
  let solution = List.map lines ~f:(fun line -> check line (-1)) in
  List.iter solution ~f:(fun s ->
    if s
    then (
      Printf.printf "Safe\n";
      safe := !safe + 1)
    else Printf.printf "Unsafe\n");
  Printf.printf "Safe: %d\n" !safe
;;

let _ =
  part_a "./input/day2.txt";
  part_b "./input/day2.txt"
;;
