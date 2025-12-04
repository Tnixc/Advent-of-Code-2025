let input = String.trim {|
#include "inputs/4"
|} in

let t =
  {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}
in

let parse_input str =
  let rows =
    String.split_on_char '\n' str
    |> List.map (fun l ->
        Array.of_list (List.of_seq (String.to_seq ("#" ^ l ^ "#"))))
  in
  let width = Array.length (List.hd rows) in
  let border = Array.init width (fun _ -> '#') in
  Array.of_list ((border :: rows) @ [ border ])
in

let grid = parse_input input in
let w = Array.length grid - 2 in
let h = Array.length grid.(0) - 2 in

let count = ref 0 in
for y = 1 to h do
  for x = 1 to w do
    if grid.(y).(x) = '@' then
      (* Printf.printf "%c" grid.(y).(x) *)
      let neighbors =
        [
          (x - 1, y - 1);
          (x, y - 1);
          (x + 1, y - 1);
          (x - 1, y);
          (x + 1, y);
          (x - 1, y + 1);
          (x, y + 1);
          (x + 1, y + 1);
        ]
      in
      if
        List.length
          (List.filter
             (fun l -> l)
             (List.map (fun (i, j) -> grid.(j).(i) = '@') neighbors))
        < 4
      then count := !count + 1
  done
done;

Printf.printf "%d\n" !count
