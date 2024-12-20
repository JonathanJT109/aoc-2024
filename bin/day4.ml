type direction =
  | Up
  | Down
  | Left
  | Right
  | UpRight
  | UpLeft
  | DownRight
  | DownLeft
  | None

let next = function
  | Up -> -1, 0
  | Down -> 1, 0
  | Left -> 0, -1
  | Right -> 0, 1
  | UpRight -> -1, 1
  | UpLeft -> -1, -1
  | DownRight -> 1, 1
  | DownLeft -> 1, -1
  | None -> 0, 0
;;

let limit lines i j =
  if j + 1 < String.length (List.nth lines i) then i, j + 1 else i + 1, 0
;;

let helper lines =
  let rec aux lines i j dir prev (sum : int) : int =
    if i = List.length lines && j = 0 && prev = ' '
    then sum
    else if i >= 0
            && i < List.length lines
            && j >= 0
            && j < String.length (List.nth lines i)
    then (
      let curr = (List.nth lines i).[j] in
      match curr, prev, dir with
      | 'X', ' ', None ->
        let directions =
          [ Up; Down; Left; Right; UpLeft; UpRight; DownLeft; DownRight ]
        in
        let outcomes =
          List.map
            (fun dir ->
              let a, b = next dir in
              aux lines (i + a) (j + b) dir 'X' sum)
            directions
        in
        Printf.printf "(%d, %d): " i j;
        List.iter (fun item -> Printf.printf "%d " item) outcomes;
        print_endline "";
        let add = List.fold_left (fun acc x -> acc + x) 0 outcomes in
        let sum = add + sum in
        Printf.printf "-> Finished | Sum: %d\n\n" sum;
        let a, b = limit lines i j in
        aux lines a b None ' ' sum
      | 'M', 'X', dir when dir <> None ->
        let a, b = next dir in
        aux lines (i + a) (j + b) dir 'M' sum
      | 'A', 'M', dir when dir <> None ->
        let a, b = next dir in
        aux lines (i + a) (j + b) dir 'A' sum
      | 'S', 'A', dir when dir <> None -> 1
      | _, _, dir when dir <> None -> 0
      | _ ->
        let a, b = limit lines i j in
        aux lines a b None ' ' sum)
    else 0
  in
  aux lines 0 0 None ' ' 0
;;

let part_a filename =
  let lines = Aoc.read_lines filename in
  Printf.printf "Occurrences: %d\n" (helper lines)
;;

let helper2 lines =
  let rec aux lines i j dir prev (sum : int) : int =
    if i = List.length lines && j = 0 && prev = ' '
    then sum
    else if i >= 0
            && i < List.length lines
            && j >= 0
            && j < String.length (List.nth lines i)
    then (
      let curr = (List.nth lines i).[j] in
      match curr, prev, dir with
      | 'A', ' ', None ->
        let directions = [ UpLeft; UpRight; DownLeft; DownRight ] in
        let outcomes =
          List.map
            (fun dir ->
              let a, b = next dir in
              aux lines (i + a) (j + b) dir 'A' sum)
            directions
        in
        Printf.printf "(%d, %d): " i j;
        List.iter (fun item -> Printf.printf "%d " item) outcomes;
        print_endline "";
        let add =
          ( List.nth outcomes 0 + List.nth outcomes 3
          , List.nth outcomes 1 + List.nth outcomes 2 )
        in
        Printf.printf "ADD: %d %d\n" (fst add) (snd add);
        let sum = if add = (0, 0) then sum + 1 else sum in
        Printf.printf "-> Finished | Sum: %d\n\n" sum;
        let a, b = limit lines i j in
        aux lines a b None ' ' sum
      | 'S', 'A', dir when dir <> None -> -1
      | 'M', 'A', dir when dir <> None -> 1
      | _, _, dir when dir <> None -> 4
      | _ ->
        let a, b = limit lines i j in
        aux lines a b None ' ' sum)
    else 4
  in
  aux lines 0 0 None ' ' 0
;;

let part_b filename =
  let lines = Aoc.read_lines filename in
  Printf.printf "Occurrences: %d\n" (helper2 lines)
;;

let _ =
  part_a "./input/day4.txt";
  part_b "./input/day4.txt"
;;
