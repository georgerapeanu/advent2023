open Core

module State = struct
  module T = struct
    type t = {row: int; col: int} [@@deriving compare, equal, hash, sexp]

    let create row col = {row; col}
  end

  include T
  include Comparable.Make (T)
end

module Matrix = struct
  type t = {rows: int; cols: int; items: char array array}
  [@@deriving compare, equal, sexp]

  let get t i j = Array.get (Array.get t.items i) j

  let set t i j v = Array.set (Array.get t.items i) j v

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

  let copy t =
    { rows= t.rows
    ; cols= t.cols
    ; items= Array.map t.items ~f:(fun row -> Array.copy row) }

  let get_number_of_undetermined_distance_inwards t distance_matrix x y =
    ( if
        is_inside t (x - 1) y
        && Char.(t.items.(x - 1).(y) = 'v')
        && Option.is_none distance_matrix.(x - 1).(y)
      then 1
      else 0 )
    +
    if
      is_inside t x (y - 1)
      && Char.(t.items.(x).(y - 1) = '>')
      && Option.is_none distance_matrix.(x).(y - 1)
    then 1
    else 0

  let get_hash t x1 y1 x2 y2 =
    let x1, y1, x2, y2 =
      if x1 > x2 || (x1 = x2 && y1 > y2) then (x2, y2, x1, y1)
      else (x1, y1, x2, y2)
    in
    if Char.(t.items.(x1).(y1) <> t.items.(x2).(y2)) then
      (x1 * 1009) + (y1 * 1007) + (x2 * 2003) + (y2 * 1097) + 23
    else 0

  let get_exit_distance t =
    let row, col = (0, 1) in
    let visited_queue = Queue.create () in
    let distance_matrix =
      Array.init t.rows ~f:(fun _ -> Array.init t.cols ~f:(fun _ -> None))
    in
    distance_matrix.(row).(col) <- Some 0 ;
    Queue.enqueue visited_queue (row, col) ;
    let rec process_current_cell () =
      match Queue.dequeue visited_queue with
      | None -> ()
      | Some (row, col) ->
          let directions =
            match t.items.(row).(col) with
            | '>' -> [(0, 1)]
            | 'v' -> [(1, 0)]
            | _ -> [(-1, 0); (0, 1); (1, 0); (0, -1)]
          in
          List.iter directions ~f:(fun (dx, dy) ->
              let next_row = row + dx in
              let next_col = col + dy in
              if
                is_inside t next_row next_col
                && Char.( <> ) (get t next_row next_col) '#'
              then
                match t.items.(row).(col) with
                | '.' ->
                    if Option.is_none distance_matrix.(next_row).(next_col) then (
                      distance_matrix.(next_row).(next_col) <-
                        Option.map
                          distance_matrix.(row).(col)
                          ~f:(fun x -> x + 1) ;
                      Queue.enqueue_front visited_queue (next_row, next_col) )
                | 'v' | '>' ->
                    let candidate_dist =
                      Option.value ~default:0 distance_matrix.(row).(col) + 1
                    in
                    let current_dist =
                      Option.value ~default:0
                        distance_matrix.(next_row).(next_col)
                    in
                    distance_matrix.(next_row).(next_col) <-
                      Some (max candidate_dist current_dist) ;
                    if
                      get_number_of_undetermined_distance_inwards t
                        distance_matrix next_row next_col
                      = 0
                    then Queue.enqueue visited_queue (next_row, next_col)
                | _ -> printf "WTF" ) ;
          process_current_cell ()
    in
    process_current_cell () ;
    Option.value_exn distance_matrix.(t.rows - 1).(t.cols - 2)

  let get_exit_distance_part2 t =
    let row, col = (0, 1) in
    let visited_queue = Queue.create () in
    let initial_state = State.create row col in
    let answer = ref 0 in
    Queue.enqueue visited_queue initial_state ;
    let rec process_current_cell visited_set {State.row; col} =
      if row = t.rows - 1 && col = t.cols - 2 then
        if !answer < Set.length visited_set then (
          answer := Set.length visited_set ;
          printf "Best answer now %d\n" !answer ;
          Stdio__Out_channel.flush Stdio.stdout ) ;
      if
        Set.length visited_set + ((t.rows - row) * (t.cols - col) * 28)
        >= !answer
      then
        let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
        List.iter directions ~f:(fun (dx, dy) ->
            let next_row = row + dx in
            let next_col = col + dy in
            if
              is_inside t next_row next_col
              && Char.( <> ) (get t next_row next_col) '#'
            then
              let next_state = State.create next_row next_col in
              if not (Set.mem visited_set next_state) then
                process_current_cell (Set.add visited_set next_state) next_state )
    in
    process_current_cell State.Set.empty initial_state ;
    !answer
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix.create |> Matrix.get_exit_distance |> printf "%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix.create |> Matrix.get_exit_distance_part2 |> printf "%d\n"
