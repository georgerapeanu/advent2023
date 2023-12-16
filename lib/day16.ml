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

module State = struct
  type t = {row: int; col: int; dx: int; dy: int}
  [@@deriving compare, equal, hash, sexp]

  let advance t = {row= t.row + t.dx; col= t.col + t.dy; dx= t.dx; dy= t.dy}
end

module BeamSolver = struct
  let solve start_state matrix =
    let visited = Hash_set.create (module State) in
    let current_states = [start_state] in
    Hash_set.add visited start_state ;
    let rec do_bfs (current_level : State.t list) =
      if List.is_empty current_level then ()
      else
        let next_level = ref [] in
        List.map current_level ~f:(fun state ->
            let current_cell =
              Matrix_with_comparator.get matrix state.row state.col
            in
            let dirs =
              match current_cell with
              | '.' -> [(state.dx, state.dy)]
              | '\\' -> [(state.dy, state.dx)]
              | '/' -> [(-state.dy, -state.dx)]
              | '-' when state.dy = 0 -> [(0, -1); (0, 1)]
              | '|' when state.dx = 0 -> [(-1, 0); (1, 0)]
              | '-' -> [(state.dx, state.dy)]
              | '|' -> [(state.dx, state.dy)]
              | _ -> failwith "Unexpected char in input"
            in
            List.map dirs ~f:(fun (dx, dy) ->
                State.advance {row= state.row; col= state.col; dx; dy} )
            |> List.filter ~f:(fun state ->
                   state.row >= 0 && state.col >= 0 && state.row < matrix.rows
                   && state.col < matrix.cols ) )
        |> List.concat
        |> List.iter ~f:(fun state ->
               if not (Hash_set.mem visited state) then (
                 next_level := state :: !next_level ;
                 Hash_set.add visited state ) ) ;
        do_bfs !next_level
    in
    do_bfs current_states ;
    Hash_set.to_list visited
    |> List.map ~f:(fun state -> (state.row, state.col))
    |> List.dedup_and_sort ~compare:(fun (dx, dy) (d2x, d2y) ->
           if dx <> d2x then Int.compare dx d2x else Int.compare dy d2y )
    |> List.length
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix_with_comparator.create
  |> BeamSolver.solve {row= 0; col= 0; dx= 0; dy= 1}
  |> printf "%d\n"

let part2 () =
  let matrix =
    In_channel.input_lines In_channel.stdin |> Matrix_with_comparator.create
  in
  let start_states = ref [] in
  for i = 0 to matrix.rows - 1 do
    let first_state : State.t = {row= i; col= 0; dx= 0; dy= 1} in
    let second_state : State.t =
      {row= i; col= matrix.cols - 1; dx= 0; dy= -1}
    in
    start_states := first_state :: second_state :: !start_states
  done ;
  for i = 0 to matrix.cols - 1 do
    let first_state : State.t = {row= 0; col= i; dx= 1; dy= 0} in
    let second_state : State.t =
      {row= matrix.rows - 1; col= i; dx= -1; dy= 0}
    in
    start_states := first_state :: second_state :: !start_states
  done ;
  List.map !start_states ~f:(fun state -> BeamSolver.solve state matrix)
  |> List.fold ~init:0 ~f:max |> printf "%d\n"
