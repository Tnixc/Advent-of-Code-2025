let input = String.trim {|
#include "inputs/3"
|} in

let t =
  {|987654321111111
811111111111119
234234234234278
818181911112111|}
in

let chars = [| '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; '0' |] in

let p1_2 str = Array.find_opt (fun c -> String.contains str c) chars in

let p1_1 str =
  Array.find_map
    (fun c ->
      match String.index_opt str c with
      | None -> None
      | Some idx ->
          let after =
            if idx + 1 = String.length str then (String.sub str 0 idx, false)
            else (String.sub str (idx + 1) (String.length str - idx - 1), true)
          in
          Some (after, String.get str idx))
    chars
in

let p1_l str =
  let (long, reverse), c1 = Option.get (p1_1 str) in
  let c2 = Option.get (p1_2 long) in
  let d =
    String.make 1 (if reverse then c1 else c2)
    ^ String.make 1 (if reverse then c2 else c1)
  in
  int_of_string d
in

let p1 str =
  String.split_on_char '\n' str |> List.map p1_l |> List.fold_left ( + ) 0
in

Printf.printf "%d\n" (p1 input)

(* String.split_on_char '\n' input *)
(* |> List.map  *)
