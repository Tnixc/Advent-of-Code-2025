let input = String.trim {|
#include "inputs/6"
|} in

let t =
  {|123 |328| 51|64 
 45 |64 |387|23 
  6 |98 |215|314
*   |+  |*  |+  |}
in
(* Printf.printf "Input:\n%s\n" t; *)
let split_last lst =
  match List.rev lst with
  | [] -> assert false
  | last :: rest -> (List.rev rest, last)
in

let nums, ops =
  String.split_on_char '\n' input
  |> List.map (fun l -> String.split_on_char '|' l |> Array.of_list)
  |> split_last
in
let ops = Array.map String.trim ops in

(* let i = ref 0 in *)
(* let sum = *)
(*   let nums = List.map (fun l -> Array.map int_of_string l) nums in *)
(*   Array.map *)
(*     (fun op -> *)
(*       let operands = List.map (fun l -> l.(!i)) nums in *)
(*       incr i; *)
(*       match op with *)
(*       | "*" -> List.fold_left (fun acc x -> acc * x) 1 operands *)
(*       | "+" -> List.fold_left (fun acc x -> acc + x) 0 operands *)
(*       | _ -> assert false) *)
(*     ops *)
(*   |> Array.fold_left ( + ) 0 *)
(* in *)

(* Printf.printf "%d\n" sum; *)
let sum =
  let i = ref 0 in
  Array.map
    (fun op ->
      let operands_h =
        List.map
          (fun l ->
            l.(!i) |> String.to_seq |> List.of_seq |> List.rev
            |> List.map (fun c -> String.make 1 c))
          nums
      in
      incr i;

      List.iter
        (fun x -> Printf.printf "[%s]\n" (String.concat "" x))
        operands_h;

      let highest_len =
        List.fold_left (fun acc l -> max acc (List.length l)) 0 operands_h
      in

      let base = List.init highest_len (fun i -> i) in

      let operands =
        List.map
          (fun j ->
            List.fold_left
              (fun acc y ->
                acc ^ (List.nth_opt y j |> Option.value ~default:""))
              "" operands_h)
          base
        |> List.map (fun l -> String.trim l)
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string
      in

      List.iter (fun x -> Printf.printf "%d " x) operands;
      Printf.printf "\n";

      match op with
      | "*" -> List.fold_left (fun acc x -> acc * x) 1 operands
      | "+" -> List.fold_left (fun acc x -> acc + x) 0 operands
      | "*" -> 0
      | "+" -> 0
      | _ -> assert false)
    ops
  |> Array.fold_left ( + ) 0
in

Printf.printf "%d\n" sum
