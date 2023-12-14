open Core
open Stdio

module Matrix = struct
  type t = {rows: int; cols: int; items: char array array}

  let get t i j = Array.get (Array.get t.items i) j

  let set t i j v = Array.set (Array.get t.items i) j v

  let flip t i j = set t i j (if Char.( = ) (get t i j) '.' then '#' else '.')

  let get_row_as_string t i =
    Array.get t.items i |> Array.to_list |> String.of_list

  let get_col_as_string t i =
    Array.fold t.items ~init:"" ~f:(fun acc row ->
        acc ^ (Array.get row i |> Char.to_string) )

  let create s =
    let rows = List.length s in
    let cols = String.length (List.hd_exn s) in
    let items =
      List.map s ~f:(fun row -> String.to_list row |> Array.of_list)
      |> Array.of_list
    in
    {rows; cols; items}

  let check_reflection_horizontal t i =
    let rec check_internal i j =
      if i < 0 || j >= t.rows then true
      else if String.equal (get_row_as_string t i) (get_row_as_string t j) then
        check_internal (i - 1) (j + 1)
      else false
    in
    check_internal (i - 1) i

  let check_reflection_vertical t i =
    let rec check_internal i j =
      if i < 0 || j >= t.cols then true
      else if String.equal (get_col_as_string t i) (get_col_as_string t j) then
        check_internal (i - 1) (j + 1)
      else false
    in
    check_internal (i - 1) i

  let get_reflection_number t =
    let answer = ref None in
    for i = 1 to t.cols - 1 do
      if check_reflection_vertical t i then answer := Option.some i
    done ;
    for i = 1 to t.rows - 1 do
      if check_reflection_horizontal t i then answer := Option.some (i * 100)
    done ;
    !answer

  let get_reflection_number_except t ex =
    let answer = ref None in
    for i = 1 to t.cols - 1 do
      if check_reflection_vertical t i then
        if i <> ex then answer := Option.some i
    done ;
    for i = 1 to t.rows - 1 do
      if check_reflection_horizontal t i then
        if i * 100 <> ex then answer := Option.some (i * 100)
    done ;
    !answer

  let find_reflection_with_smudge t =
    let answer = ref None in
    let initial = get_reflection_number t |> Option.value_exn in
    for i = 0 to t.rows - 1 do
      if Option.is_none !answer then
        for j = 0 to t.cols - 1 do
          if Option.is_none !answer then (
            flip t i j ;
            answer := get_reflection_number_except t initial ;
            flip t i j )
        done
    done ;
    !answer
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:String.strip
  |> List.fold ~init:[] ~f:(fun acc row ->
         if String.is_empty row then [] :: acc
         else
           match acc with
           | [] -> [[row]]
           | head :: rest -> (head @ [row]) :: rest )
  |> List.rev |> List.map ~f:Matrix.create
  |> List.map ~f:Matrix.get_reflection_number
  |> List.map ~f:(fun value -> Option.value_exn value)
  |> List.fold ~init:0 ~f:Int.( + )
  |> printf "%d"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:String.strip
  |> List.fold ~init:[] ~f:(fun acc row ->
         if String.is_empty row then [] :: acc
         else
           match acc with
           | [] -> [[row]]
           | head :: rest -> (head @ [row]) :: rest )
  |> List.rev |> List.map ~f:Matrix.create
  |> List.map ~f:Matrix.find_reflection_with_smudge
  |> List.map ~f:(fun value -> Option.value_exn value)
  |> List.fold ~init:0 ~f:Int.( + )
  |> printf "%d"
