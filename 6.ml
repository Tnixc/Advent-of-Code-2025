let input = String.trim {|
#include "inputs/6"
|} in

let t =
  {|123 |328| 51|64 
 45 |64 |387|23 
  6 |98 |215|314
*   |+  |*  |+  |}
in

let split_last lst =
  match List.rev lst with
  | [] -> assert false
  | last :: rest -> (List.rev rest, last)
in

let nums, ops =
  String.split_on_char '\n' input
  |> List.map (fun l -> String.split_on_char '|' l |> Array.of_list)
  |> split_last
  |> fun (a, b) -> (a, Array.map String.trim b)
in

let p1 =
  let i = ref 0 in
  let nums =
    List.map
      (fun l -> Array.map (fun s -> int_of_string (String.trim s)) l)
      nums
  in
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

let p2 =
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

      let highest_len =
        List.fold_left (fun acc l -> max acc (List.length l)) 0 operands_h
      in

      let operands =
        List.map
          (fun idx ->
            List.fold_left
              (fun acc digits ->
                acc ^ (List.nth_opt digits idx |> Option.value ~default:""))
              "" operands_h)
          (List.init highest_len (fun i -> i))
        |> List.map (fun l -> String.trim l)
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string
      in

      match op with
      | "*" -> List.fold_left (fun acc x -> acc * x) 1 operands
      | "+" -> List.fold_left (fun acc x -> acc + x) 0 operands
      | _ -> assert false)
    ops
  |> Array.fold_left ( + ) 0
in

Printf.printf "%d, %d\n" p1 p2
