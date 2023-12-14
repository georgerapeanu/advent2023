open Core
open Stdio

module Matrix = struct
  type t = {rows: int; cols: int; items: char array array}
  [@@deriving compare, equal, sexp]

  let get t i j = Array.get (Array.get t.items i) j

  let set t i j v = Array.set (Array.get t.items i) j v

  let flip t i j = set t i j (if Char.( = ) (get t i j) '.' then '#' else '.')

  let is_inside t i j = i >= 0 && i < t.rows && j >= 0 && j < t.cols

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

  let apply_tilt t (dx, dy) =
    let rec get_directional_range s t =
      if s = t then [s]
      else if s < t then s :: get_directional_range (s + 1) t
      else s :: get_directional_range (s - 1) t
    in
    let row_iter =
      if dx < 0 then get_directional_range 0 (t.rows - 1)
      else get_directional_range (t.rows - 1) 0
    in
    let col_iter =
      if dy < 0 then get_directional_range 0 (t.cols - 1)
      else get_directional_range (t.cols - 1) 0
    in
    let something_changed = ref false in
    List.iter row_iter ~f:(fun row ->
        List.iter col_iter ~f:(fun col ->
            let next_row = row + dx in
            let next_col = col + dy in
            if
              is_inside t next_row next_col
              && Char.equal (get t next_row next_col) '.'
              && Char.equal (get t row col) 'O'
            then (
              something_changed := true ;
              set t next_row next_col 'O' ;
              set t row col '.' ) ) ) ;
    !something_changed

  let get_load t =
    Array.foldi t.items ~init:0 ~f:(fun i acc row ->
        Array.fold row ~init:0 ~f:(fun acc elem ->
            if Char.equal elem 'O' then acc + 1 else acc )
        |> Int.( * ) (t.rows - i)
        |> Int.( + ) acc )

  let copy t =
    { rows= t.rows
    ; cols= t.cols
    ; items= Array.map t.items ~f:(fun row -> Array.copy row) }
end

module Matrix_with_comparator = struct
  include Matrix
  include Comparator.Make (Matrix)
end

let part1 () =
  let matrix = In_channel.input_lines In_channel.stdin |> Matrix.create in
  while Matrix.apply_tilt matrix (-1, 0) do
    ()
  done ;
  printf "%d\n" (Matrix.get_load matrix)

let part2 () =
  let matrix =
    In_channel.input_lines In_channel.stdin |> Matrix_with_comparator.create
  in
  let states = ref (Map.of_alist_exn (module Matrix_with_comparator) []) in
  let i = ref 0 in
  let start_cycle = ref None in
  let target = 1000000000 in
  while !i <= target && Option.is_none !start_cycle do
    states := Map.add_exn !states ~key:(Matrix.copy matrix) ~data:!i ;
    i := !i + 1 ;
    while Matrix_with_comparator.apply_tilt matrix (-1, 0) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (0, -1) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (1, 0) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (0, 1) do
      ()
    done ;
    if Map.mem !states matrix then start_cycle := Map.find !states matrix
  done ;
  (*!start_cylce..(!i - 1) cycles*)
  let target =
    match !start_cycle with
    | None -> 0
    | Some start -> (target - start) % (!i - start)
  in
  for _ = 1 to target do
    while Matrix_with_comparator.apply_tilt matrix (-1, 0) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (0, -1) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (1, 0) do
      ()
    done ;
    while Matrix_with_comparator.apply_tilt matrix (0, 1) do
      ()
    done
  done ;
  printf "%d\n" (Matrix.get_load matrix)
