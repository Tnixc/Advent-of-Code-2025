let input = String.trim {|
#include "inputs/4"
|} in

let solve str =
  let grid_init =
    String.split_on_char '\n' str
    |> List.map (fun s -> Array.init (String.length s) (fun i -> s.[i]))
    |> Array.of_list
  in
  let height = Array.length grid_init in
  let width = Array.length grid_init.(0) in

  let p1 = ref 0 in

  let count_neighbors grid y x =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.filter (fun (dx, dy) ->
        let ny = y + dy in
        let nx = x + dx in
        ny >= 0 && ny < height && nx >= 0 && nx < width
        &&
        let c = grid.(ny).(nx) in
        c = '@' || c = 'X')
    |> List.length
  in

  let grid_a = grid_init in
  let grid_b = Array.map Array.copy grid_init in

  let step src dst =
    let count = ref 0 in

    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let cell = src.(y).(x) in
        if cell = '@' then
          if count_neighbors src y x < 4 then (
            dst.(y).(x) <- '.';
            incr count)
          else dst.(y).(x) <- '@'
        else dst.(y).(x) <- cell
      done
    done;

    !count
  in

  let rec loop src dst total_count depth =
    if depth = 1 then p1 := total_count;
    let count = step src dst in
    if count = 0 then total_count
    else loop dst src (total_count + count) (depth + 1)
  in

  let count = loop grid_a grid_b 0 0 in
  (!p1, count)
in

let p1, p2 = solve input in
Printf.printf "%d, %d\n" p1 p2
