open Core

let string_to_int list = List.map list ~f:(fun item -> Int.of_string item)

let part_a filename =
  let lines = Aoc.read_lines filename in
  let s = List.map ~f:(fun line -> String.split line ~on:' ') lines in
  let f =
    List.map ~f:(fun pair -> Option.value (List.hd pair) ~default:"") s |> string_to_int
  in
  let l =
    List.map ~f:(fun pair -> Option.value (List.last pair) ~default:"") s |> string_to_int
  in
  let firsts = List.sort ~compare f in
  let lasts = List.sort ~compare l in
  let diff =
    let l = List.map2 firsts lasts ~f:(fun a b -> Int.abs (b - a)) in
    match l with
    | Ok result -> result
    | _ -> raise (Invalid_argument "The list have unequal lengths")
  in
  let sum = List.sum (module Int) diff ~f:(fun a -> a) in
  Printf.printf "Sum: %d\n" sum
;;

let part_b filename =
  let lines = Aoc.read_lines filename in
  let hs = Hashtbl.create ~growth_allowed:true ~size:100 (module Int) in
  let s = List.map ~f:(fun line -> String.split line ~on:' ') lines in
  let f =
    List.map ~f:(fun pair -> Option.value (List.hd pair) ~default:"") s |> string_to_int
  in
  let l =
    List.map ~f:(fun pair -> Option.value (List.last pair) ~default:"") s |> string_to_int
  in
  List.iter l ~f:(fun item ->
    let n = Hashtbl.find hs item in
    match n with
    | Some x ->
      Hashtbl.remove hs item;
      Hashtbl.add hs ~key:item ~data:(x + 1) |> ignore
    | _ -> Hashtbl.add hs ~key:item ~data:1 |> ignore);
  let mult =
    List.map f ~f:(fun item -> Option.value (Hashtbl.find hs item) ~default:0 * item)
  in
  let sum = List.sum (module Int) mult ~f:(fun a -> a) in
  Printf.printf "Sum: %d\n" sum
;;

let _ =
  part_a "./input/test.txt";
  part_b "./input/day1.txt"
;;
