open Core
open Str

let parser line =
  let pattern = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec aux line sum =
    match line with
    | "" -> sum
    | _ ->
      (try
         (* let _ = search_forward pattern line 0 in *)
         let f = search_forward pattern line 0 in
         let a = matched_group 1 line in
         let b = matched_group 2 line in
         let skip = f + String.length a + String.length b + 5 in
         let a_int = int_of_string a in
         let b_int = int_of_string b in
         aux (String.drop_prefix line skip) (sum + (a_int * b_int))
       with
       | _ -> sum)
  in
  aux line 0
;;

let parser2 line =
  let pattern = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec aux line sum lock =
    match line with
    | "" -> sum
    | _ ->
      (try
         (* let _ = search_forward pattern line 0 in *)
         let f = search_forward pattern line 0 in
         let lock = ref lock in
         let i = ref 0 in
         while !i < f do
           let new_line = String.drop_prefix line !i in
           if String.is_prefix new_line ~prefix:"do()"
           then (
             i := !i + 3;
             lock := false)
           else if String.is_prefix new_line ~prefix:"don't()"
           then (
             i := !i + 5;
             lock := true);
           i := !i + 1
         done;
         let a = matched_group 1 line in
         let b = matched_group 2 line in
         let skip = f + String.length a + String.length b + 5 in
         let a_int = int_of_string a in
         let b_int = int_of_string b in
         let sum = if !lock then sum else sum + (a_int * b_int) in
         aux (String.drop_prefix line skip) sum !lock
       with
       | _ -> sum)
  in
  aux line 0 false
;;

let part_a filename =
  let line = List.hd_exn (Aoc.read_lines filename) in
  Printf.printf "Sum %d\n" (parser line)
;;

let part_b filename =
  let line = List.hd_exn (Aoc.read_lines filename) in
  Printf.printf "Sum %d\n" (parser2 line)
;;

let _ =
  part_a "./input/day3.txt";
  part_b "./input/day3.txt"
;;
