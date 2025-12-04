let input = String.trim {|
#include "inputs/4"
|} in

let solve str =
  let pad_line l = "#" ^ l ^ "#" in
  let lines = String.split_on_char '\n' str |> List.map pad_line in
  let border = String.make (String.length (List.hd lines)) '#' in
  let grid_init =
    (border :: lines) @ [ border ]
    |> List.map (fun s -> Array.init (String.length s) (fun i -> s.[i]))
    |> Array.of_list
  in

  let p1 = ref 0 in

  let count_neighbors grid y x =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.filter (fun (dx, dy) ->
        Array.mem grid.(y + dy).(x + dx) [| '@'; 'X' |])
    |> List.length
  in

  let step grid =
    let new_grid =
      grid
      |> Array.mapi (fun y ->
          Array.mapi (fun x cell ->
              if cell = '@' then
                if count_neighbors grid y x < 4 then 'X' else '@'
              else cell))
    in

    let changed = ref false in
    let count = ref 0 in

    new_grid
    |> Array.iteri (fun y ->
        Array.iteri (fun x cell ->
            if cell = 'X' && grid.(y).(x) = '@' then (
              changed := true;
              incr count)));

    ( new_grid
      |> Array.map (Array.map (fun cell -> if cell = 'X' then '.' else cell)),
      !changed,
      !count )
  in

  let rec loop grid total_count depth =
    if depth = 1 then p1 := total_count;
    let new_grid, changed, count = step grid in
    if changed then loop new_grid (total_count + count) (depth + 1)
    else (new_grid, total_count)
  in

  let final_grid, count = loop grid_init 0 0 in
  (!p1, count)
in

let p1, p2 = solve input in
Printf.printf "%d, %d\n" p1 p2
