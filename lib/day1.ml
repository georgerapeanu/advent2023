open Base
open Stdio

let get_digits_from_line_part1 line =
  let filtered_line =
    String.fold line ~init:"" ~f:(fun acc c ->
        if Char.to_int c >= Char.to_int '0' && Char.to_int c <= Char.to_int '9'
        then acc ^ Char.escaped c
        else acc )
  in
  Array.map (String.to_array filtered_line) ~f:(fun c ->
      Char.to_int c - Char.to_int '0' )

let get_digits_from_line_part2 line =
  let idx = ref 0 in
  let token_map =
    Hashtbl.of_alist_exn
      (module String)
      [ ("zero", 0)
      ; ("one", 1)
      ; ("two", 2)
      ; ("three", 3)
      ; ("four", 4)
      ; ("five", 5)
      ; ("six", 6)
      ; ("seven", 7)
      ; ("eight", 8)
      ; ("nine", 9)
      ; ("0", 0)
      ; ("1", 1)
      ; ("2", 2)
      ; ("3", 3)
      ; ("4", 4)
      ; ("5", 5)
      ; ("6", 6)
      ; ("7", 7)
      ; ("8", 8)
      ; ("9", 9) ]
  in
  let digits = ref [] in
  while !idx < String.length line do
    let result_ref = ref None in
    Hashtbl.iteri token_map ~f:(fun ~key ~data:_ ->
        if String.is_prefix ~prefix:key (String.drop_prefix line !idx) then
          result_ref := Some key ) ;
    match !result_ref with
    | None -> idx := !idx + 1
    | Some key ->
        idx := !idx + 1 ;
        digits := Hashtbl.find_exn token_map key :: !digits
  done ;
  Array.of_list (List.rev !digits)

let get_number_from_line get_digits_from_line line =
  let line_digits = get_digits_from_line line in
  let first_digit = line_digits.(0) in
  let last_digit = line_digits.(Array.length line_digits - 1) in
  (first_digit * 10) + last_digit

let part1 () =
  let line_numbers =
    List.map
      (In_channel.input_lines In_channel.stdin)
      ~f:(get_number_from_line get_digits_from_line_part1)
  in
  let sum : int = List.fold line_numbers ~init:0 ~f:Int.( + ) in
  printf "%d" sum

let part2 () =
  let line_numbers =
    List.map
      (In_channel.input_lines In_channel.stdin)
      ~f:(get_number_from_line get_digits_from_line_part2)
  in
  let sum : int = List.fold line_numbers ~init:0 ~f:Int.( + ) in
  printf "%d" sum
