open! Core
open Stdio

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

  let get_starting_position t =
    Array.foldi t.items ~init:None ~f:(fun index_row answer row ->
        match answer with
        | Some _ -> answer
        | None ->
            Array.foldi row ~init:None ~f:(fun index_col answer_row item ->
                match answer_row with
                | Some _ -> answer_row
                | None when Char.( = ) item 'S' -> Some (index_row, index_col)
                | None -> None ) )

  let get_distance_matrix t =
    let row, col = get_starting_position t |> Option.value_exn in
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
          List.iter
            [(-1, 0); (0, 1); (1, 0); (0, -1)]
            ~f:(fun (dx, dy) ->
              let next_row = row + dx in
              let next_col = col + dy in
              if
                is_inside t next_row next_col
                && Char.( <> ) (get t next_row next_col) '#'
                && Option.is_none distance_matrix.(next_row).(next_col)
              then (
                distance_matrix.(next_row).(next_col) <-
                  Option.map distance_matrix.(row).(col) ~f:(fun x -> x + 1) ;
                Queue.enqueue visited_queue (next_row, next_col) ) ) ;
          process_current_cell ()
    in
    process_current_cell () ; distance_matrix

  let extend_matrix t times =
    let start_x, start_y = get_starting_position t |> Option.value_exn in
    t.items.(start_x).(start_y) <- '.' ;
    let rows = t.rows * ((times * 2) + 1) in
    let cols = t.cols * ((times * 2) + 1) in
    let items =
      Array.init rows ~f:(fun index_row ->
          Array.init cols ~f:(fun index_col ->
              t.items.(index_row % t.rows).(index_col % t.cols) ) )
    in
    items.((times * t.rows) + start_x).((times * t.cols) + start_y) <- 'S' ;
    {rows; cols; items}

  (* the input is handcrafted, and has the following properties

     matrix_height = matrix_widht = 131 = 2 * 65 + 1 query_distance = 65 +
     202300 * 131

     By printing the answers for 65 + x * 131 target distances, we are able to
     search it on OEIS, which comes up with the following formula: a(x) = 3799 +
     x * 15158 + 15090 * x ^ 2 => a(202300) = 617565692567199 *)
  let print_answers_array t =
    let magic_times_factor = 20 in
    let max_distance = (magic_times_factor * t.rows) - t.rows in
    let extended_t = extend_matrix t magic_times_factor in
    let distance_matrix = get_distance_matrix extended_t in
    let distances_array = Array.init max_distance ~f:(fun _ -> 0) in
    Array.iter distance_matrix
      ~f:
        (Array.iter ~f:(function
          | Some d when d < max_distance ->
              distances_array.(d) <- distances_array.(d) + 1
          | Some _ | None -> () ) ) ;
    let _, _, answers_array =
      Array.fold distances_array ~init:(Int63.zero, Int63.zero, [||])
        ~f:(fun (even, odd, prefix_sum_array) current_element ->
          let open Int63.O in
          ( odd
          , even + Int63.of_int current_element
          , Array.append prefix_sum_array [|even + Int63.of_int current_element|]
          ) )
    in
    List.init 20 ~f:(fun i ->
        let target_dist = (i * 131) + 65 in
        if target_dist < Array.length answers_array then
          Some answers_array.(target_dist)
        else None )
    |> List.iter ~f:(fun x -> print_s [%sexp (x : Int63.t option)])
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

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix_with_comparator.create |> Matrix_with_comparator.get_distance_matrix
  |> Array.fold ~init:0 ~f:(fun acc ->
         Array.fold ~init:acc ~f:(fun acc -> function
           | Some d when d % 2 = 0 && d <= 64 -> acc + 1 | Some _ | None -> acc ) )
  |> printf "%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix_with_comparator.create |> Matrix_with_comparator.print_answers_array
