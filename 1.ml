let input = String.trim {|
#include "inputs/1"
|} in

let emod a b =
  let r = a mod b in
  if r < 0 then r + abs b else r
in

let times = ref 0 in
let dial =
  String.split_on_char '\n' input
  |> List.map (fun line ->
      let line = String.trim line in
      (line.[0], String.sub line 1 (String.length line - 1) |> int_of_string))
  |> List.fold_left
       (fun acc (char, count) ->
         let new_acc =
           match char with
           | 'L' ->
               times :=
                 !times
                 +
                 if acc = 0 then count / 100
                 else if count >= acc then ((count - acc) / 100) + 1
                 else 0;
               emod (acc - count) 100
           | 'R' ->
               times := !times + ((acc + count) / 100);
               emod (acc + count) 100
           | _ -> assert false
         in
         new_acc)
       50
in

Printf.printf "Final position: %d\n" dial;
Printf.printf "Final times: %d\n" !times
