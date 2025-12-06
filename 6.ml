let input = String.trim {|
#include "inputs/6"
|} in

let split_last lst =
  match List.rev lst with
  | [] -> assert false
  | last :: rest -> (List.rev rest, last)
in

let res =
  input |> String.split_on_char '\n' |> split_last
  |> fun (operands, operators) ->
  let max_len =
    List.fold_left
      (fun acc l -> max acc (String.length l))
      (String.length operators) operands
  in
  let indexes =
    [ max_len ]
    @ (String.to_seq operators |> List.of_seq
      |> List.fold_left
           (fun (acc, i) c ->
             if c = '*' || c = '+' then (i :: acc, i + 1) else (acc, i + 1))
           ([], 0)
      |> fst)
    |> List.rev
  in

  let prev = ref 0 in
  let op_len = String.length operators in
  let groups =
    indexes |> List.tl
    |> List.map (fun i ->
        let opands =
          List.map (fun l -> String.sub l !prev (i - !prev)) operands
        in
        let r =
          String.trim (String.sub operators !prev (min i op_len - !prev))
        in
        prev := i;
        (opands, r))
  in
  let p1, p2 =
    groups
    |> List.mapi (fun idx (opands, r) ->
        let highest_len =
          List.fold_left (fun acc l -> max acc (String.length l)) 0 opands
        in

        let p2 =
          List.map
            (fun idx ->
              List.fold_left
                (fun acc digits ->
                  acc ^ (List.nth_opt digits idx |> Option.value ~default:""))
                ""
                (opands
                |> List.map (fun x ->
                    x |> String.to_seq |> List.of_seq
                    |> List.map (fun c -> String.make 1 c))))
            (List.init highest_len (fun i -> i))
          |> List.map String.trim
          |> List.filter (fun s -> s <> "")
          |> List.map int_of_string
        in

        let p1 = opands |> List.map String.trim |> List.map int_of_string in
        match r with
        | "*" -> (List.fold_left ( * ) 1 p1, List.fold_left ( * ) 1 p2)
        | "+" -> (List.fold_left ( + ) 0 p1, List.fold_left ( + ) 0 p2)
        | _ -> assert false)
    |> List.fold_left
         (fun (acc1, acc2) (x1, x2) -> (acc1 + x1, acc2 + x2))
         (0, 0)
  in

  Printf.printf "%d, %d\n" p1 p2
in
()
