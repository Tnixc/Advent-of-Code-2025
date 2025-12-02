let input = String.trim {|
#include "inputs/2"
|} in

let sum = ref 0 in

let t =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
in

let check1 str len =
  len mod 2 = 0
  && String.sub str 0 (len / 2) = String.sub str (len / 2) (len / 2)
in

(* returns true if invalid *)
let rec check2 str len span span_len =
  if len = 0 then true
  else if len < span_len then false
  else if String.sub str 0 span_len <> span then false
  else
    check2
      (String.sub str span_len (len - span_len))
      (len - span_len) span span_len
in

let s =
  String.split_on_char ',' input
  |> List.map (fun x ->
      match String.split_on_char '-' x with
      | [ a; b ] -> (int_of_string a, int_of_string b)
      | _ -> assert false)
  |> List.map (fun (a, b) ->
      let count = ref 0 in
      for i = a to b do
        let str = string_of_int i in
        let len = String.length str in
        let rec loop k =
          if k > len / 2 then ()
          else if check2 str len (String.sub str 0 k) k then (
            count := !count + i;
            ())
          else loop (k + 1)
        in
        loop 1
      done;

      !count)
  |> List.fold_left (fun acc x -> acc + x) 0
in

Printf.printf "S: %d\n" s
