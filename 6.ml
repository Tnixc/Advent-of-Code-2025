let input = String.trim {|
#include "inputs/6"
|} in

let t =
  {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
    *   +   *   +  |}
in

let split_last lst =
  match List.rev lst with
  | [] -> assert false
  | last :: rest -> (List.rev rest, last)
in

let nums, ops =
  String.split_on_char '\n' input
  |> List.map (fun l ->
      String.split_on_char ' ' l
      |> List.filter (fun s -> s <> "")
      |> Array.of_list)
  |> split_last
in
let nums = List.map (fun l -> Array.map int_of_string l) nums in
let i = ref 0 in
let sum =
  Array.map
    (fun op ->
      let operands = List.map (fun l -> l.(!i)) nums in
      incr i;
      match op with
      | "*" -> List.fold_left (fun acc x -> acc * x) 1 operands
      | "+" -> List.fold_left (fun acc x -> acc + x) 0 operands
      | _ -> assert false)
    ops
  |> Array.fold_left ( + ) 0
in

Printf.printf "%d\n" sum
