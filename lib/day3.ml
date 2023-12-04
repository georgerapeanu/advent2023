open Base
open Stdio

let input_to_matrix list_of_lines = 
  Array.of_list @@ List.map list_of_lines ~f: (fun line -> Array.of_list @@ String.to_list line)

let is_digit c =
  Char.to_int '0' <= Char.to_int c && Char.to_int c <= Char.to_int '9' 

let is_operator c = 
  not (Char.equal c '.') && not (is_digit c)
    
let is_star c = 
  Char.equal c '*'

let get_number_at_pos (matrix: char array array) i j =
  if j > 0 && is_digit matrix.(i).(j - 1) then
    None
  else
    let k = ref j in
    while !k < (Array.length matrix.(i)) && is_digit matrix.(i).(!k) do
      k := !k + 1
    done;

    let number_string = (Array.sub matrix.(i) ~pos:j ~len:(!k - j) |> String.of_array) in
    if String.length number_string = 0 then 
      None
    else
      let answer = ref None in
      for l = j to !k - 1 do
        for dx = -1 to 1 do 
          for dy = -1 to 1 do 
            let x = i + dx in
            let y = l + dy in 
            if (not (x < 0 || x >= (Array.length matrix) || y < 0 || (y >= Array.length matrix.(x)))) && is_operator matrix.(x).(y) then 
              answer := Option.some @@ Int.of_string number_string
          done;
        done;
      done;
      !answer

module IntIntTuple = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  (* Lot's of stuff. *)
end

let get_number_start matrix i j =
  if not (is_digit matrix.(i).(j))  then
    (i, j)
  else 
    let k = ref j in 
    while !k > 0 && is_digit matrix.(i).(!k - 1) do 
      k := !k - 1
      done;
    (i, !k)

let get_number_starts_adjacent_to_cell matrix i j = 
  let answer = ref [] in
  for dx = -1 to 1 do 
    for dy = -1 to 1 do 
      let x = i + dx in 
      let y = j + dy in 
      answer := (get_number_start matrix x y) :: !answer
    done 
  done;
  !answer


let get_numbers_adjacent_to_star matrix i j = 
  if is_star matrix.(i).(j) then 
    let starts = get_number_starts_adjacent_to_cell matrix i j in 
    let unique_starts_set = Set.of_list (module IntIntTuple) starts in 
    let unique_starts_list = Set.to_list unique_starts_set in 
    let unique_numbers_list_opt = List.map ~f: (fun x -> let (f, s) = x in get_number_at_pos matrix f s) unique_starts_list in 
    let unique_numbers_list = List.filter ~f: Option.is_some unique_numbers_list_opt |> List.map ~f: (fun x -> Option.value_exn x) in
    unique_numbers_list
  else 
    []

let part1 () =
  let matrix = input_to_matrix (In_channel.input_lines In_channel.stdin) in
  let list_of_num_opt = ref [] in
  for i = 0 to (Array.length matrix) - 1 do
    for j = 0 to (Array.length matrix.(i)) - 1 do
      let number_at_pos = (get_number_at_pos matrix i j) in
      list_of_num_opt := number_at_pos :: !list_of_num_opt;
    done
  done;
  let list_of_num: int list = List.filter !list_of_num_opt ~f: Option.is_some |> List.map ~f: (fun x -> Option.value_exn x) |> List.rev in
  printf "%d" (List.fold list_of_num ~init:0 ~f: Int.(+))

let part2 () =
  let matrix = input_to_matrix (In_channel.input_lines In_channel.stdin) in
  let list_of_gear_stuff = ref [] in
  for i = 0 to (Array.length matrix) - 1 do
    for j = 0 to (Array.length matrix.(i)) - 1 do
      list_of_gear_stuff := (get_numbers_adjacent_to_star matrix i j) :: !list_of_gear_stuff
    done
  done;
  
  let sum = List.filter !list_of_gear_stuff ~f: (fun x -> List.length x = 2) 
      |> List.map ~f: (fun x -> (List.nth_exn x 0) * (List.nth_exn x 1))
      |> List.fold ~init: 0 ~f: Int.(+)
  in

  printf "%d" sum
