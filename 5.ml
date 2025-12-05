let input = String.trim {|
#include "inputs/5"
|} in

let t = {|3-5
10-14
16-20
12-18
x
1
5
8
11
17
32|} in

let ranges, ids =
  match String.split_on_char 'x' input |> List.map String.trim with
  | [ r; i ] ->
      ( String.split_on_char '\n' r
        |> List.map (fun l ->
            match String.split_on_char '-' l with
            | [ a; b ] -> (int_of_string a, int_of_string b)
            | _ -> assert false),
        String.split_on_char '\n' i |> List.map int_of_string )
  | _ -> assert false
in

let p1 ranges ids =
  List.map
    (fun id ->
      let fresh = List.exists (fun (a, b) -> a <= id && id <= b) ranges in
      if fresh then 1 else 0)
    ids
  |> List.fold_left ( + ) 0
in

let p2 ranges =
  let merged = ref [] in
  let rec insert (lo, hi) = function
    | [] -> [ (lo, hi) ]
    | (a, b) :: rest ->
        if hi < a - 1 then (lo, hi) :: (a, b) :: rest
        else if lo > b + 1 then (a, b) :: insert (lo, hi) rest
        else insert (min lo a, max hi b) rest
  in
  let total = ref 0 in
  List.iter (fun r -> merged := insert r !merged) ranges;
  List.iter (fun (a, b) -> total := !total + abs (a - b) + 1) !merged;
  !total
in

let p1 = p1 ranges ids in
let p2 = p2 ranges in

Printf.printf "%d\n" p1;
Printf.printf "%d\n" p2
