let input = String.trim {|
#include "inputs/3"
|} in

let t = {|987654321111111
811111111111119
234234234234278
818181911112111|} in

let chars = [| '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; '0' |] in

let p1_2 str = Array.find_opt (fun c -> String.contains str c) chars in

let p1_1 str =
  Array.find_map
    (fun c ->
      match String.index_opt (String.sub str 0 (String.length str - 1)) c with
      | None -> None
      | Some idx ->
          let after = String.sub str (idx + 1) (String.length str - idx - 1) in
          Some (after, String.get str idx))
    chars
in

let p1_l str =
  let long, c1 = Option.get (p1_1 str) in
  let c2 = Option.get (p1_2 long) in
  let d = String.make 1 c1 ^ String.make 1 c2 in
  int_of_string d
in

let p1 str =
  String.split_on_char '\n' str |> List.map p1_l |> List.fold_left ( + ) 0
in

Printf.printf "%d\n" (p1 input);

let chars = [| '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; '0' |] in

let rec p2 str depth acc =
  if depth = 0 then acc
  else
    let search_len = String.length str - depth + 1 in
    if search_len < 0 then acc
    else
      match
        Array.find_map
          (fun c ->
            match String.index_opt (String.sub str 0 search_len) c with
            | None -> None
            | Some idx ->
                let after =
                  String.sub str (idx + 1) (String.length str - idx - 1)
                in
                Some (c, after))
          chars
      with
      | None -> acc
      | Some (found_char, after) -> p2 after (depth - 1) (found_char :: acc)
in

let x =
  String.split_on_char '\n' input
  |> List.map (fun l -> p2 l 12 [])
  |> List.map List.rev
  |> List.map (fun l ->
      List.fold_left (fun acc c -> acc ^ String.make 1 c) "" l)
  |> List.map int_of_string |> List.fold_left ( + ) 0
in

Printf.printf "%d\n" x
