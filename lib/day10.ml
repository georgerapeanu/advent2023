open Core
open Stdio

module Map = struct
  type pipe = VERTICAL | HORIZONTAL | NEL | NWL | SEL | SWL | GROUND | START

  type t = {map: pipe array array; color: int array array}

  let is_inside_map t (row, col) =
    not
      ( row < 0
      || row >= Array.length t.map
      || col < 0
      || col >= Array.length (Array.get t.map 0) )

  let get_pipe_neighbors t (row, col) =
    ( match Array.get (Array.get t.map row) col with
    | VERTICAL -> [(row - 1, col); (row + 1, col)]
    | HORIZONTAL -> [(row, col - 1); (row, col + 1)]
    | NEL -> [(row - 1, col); (row, col + 1)]
    | NWL -> [(row - 1, col); (row, col - 1)]
    | SEL -> [(row + 1, col); (row, col + 1)]
    | SWL -> [(row + 1, col); (row, col - 1)]
    | GROUND -> []
    | START -> [] )
    |> List.filter ~f:(is_inside_map t)

  let get_connected_neighbors t (row, col) =
    get_pipe_neighbors t (row, col)
    |> List.filter ~f:(fun (n_row, n_col) ->
           List.mem
             (get_pipe_neighbors t (n_row, n_col))
             (row, col)
             ~equal:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2) )

  let rec color_cell_connection t (row, col) color =
    let curr_color = Array.get (Array.get t.color row) col in
    if curr_color <> color then (
      Array.set (Array.get t.color row) col color ;
      get_connected_neighbors t (row, col)
      |> List.iter ~f:(fun neighbor -> color_cell_connection t neighbor color) )

  let create input =
    let map =
      Array.of_list input
      |> Array.map ~f:(fun line ->
             String.to_list line |> Array.of_list
             |> Array.map ~f:(fun elem ->
                    match elem with
                    | '|' -> VERTICAL
                    | '-' -> HORIZONTAL
                    | 'L' -> NEL
                    | 'J' -> NWL
                    | 'F' -> SEL
                    | '7' -> SWL
                    | '.' -> GROUND
                    | 'S' -> START
                    | _ -> failwith "unexpected char in input" ) )
    in
    let color = Array.map map ~f:(fun row -> Array.map row ~f:(fun _ -> 0)) in
    let this = {map; color} in
    let next_color = ref 1 in
    Array.iteri this.color ~f:(fun row array_row ->
        Array.iteri array_row ~f:(fun col color ->
            if color = 0 then color_cell_connection this (row, col) !next_color ;
            next_color := !next_color + 1 ) ) ;
    this

  let get_start t =
    Array.foldi ~init:None t.map ~f:(fun i acc row ->
        match acc with
        | None -> (
            let row_acc =
              Array.foldi ~init:None row ~f:(fun j acc elem ->
                  match elem with START -> Option.some j | _ -> acc )
            in
            match row_acc with None -> acc | Some j -> Option.some (i, j) )
        | _ -> acc )

  let resolve_start t =
    let start_row, start_col = get_start t |> Option.value_exn in
    let neighbors =
      [ (start_row - 1, start_col)
      ; (start_row, start_col + 1)
      ; (start_row + 1, start_col)
      ; (start_row, start_col - 1) ]
      |> List.map ~f:(fun cell ->
             if is_inside_map t cell then Option.some cell else None )
      |> List.map ~f:(fun cell ->
             if Option.is_none cell then None
             else if
               List.mem
                 (get_pipe_neighbors t (Option.value_exn cell))
                 (start_row, start_col)
                 ~equal:(fun (r1, c1) (r2, c2) -> r1 = r2 && c1 = c2)
             then cell
             else None )
    in
    let loop_color, _ =
      List.filter_opt neighbors
      |> List.map ~f:(fun (row, col) -> Array.get (Array.get t.color row) col)
      |> List.fold ~init:(0, 0) ~f:(fun (color, fr) cell_color ->
             if color = cell_color then (color, fr + 1)
             else if fr = 0 then (cell_color, 1)
             else (color, fr - 1) )
    in
    let neighbors =
      List.map neighbors ~f:(fun cell ->
          match cell with
          | None -> None
          | Some (row, col) ->
              if Array.get (Array.get t.color row) col = loop_color then
                Option.some (row, col)
              else None )
    in
    let start_pipe =
      match neighbors with
      | [Some _; None; Some _; None] -> VERTICAL
      | [None; Some _; None; Some _] -> HORIZONTAL
      | [Some _; Some _; None; None] -> NEL
      | [Some _; None; None; Some _] -> NWL
      | [None; Some _; Some _; None] -> SEL
      | [None; None; Some _; Some _] -> SWL
      | _ -> failwith "Start pipe could not be determined"
    in
    Array.set (Array.get t.map start_row) start_col start_pipe ;
    Array.set (Array.get t.color start_row) start_col loop_color ;
    t

  let get_farthest_distance_in_same_loop t (row, col) =
    let visited =
      Hash_set.create
        ( module struct
          type t = int * int [@@deriving compare, hash, equal, sexp]
        end )
    in
    let rec do_bfs current_level =
      if List.is_empty current_level then -1
      else (
        List.iter current_level ~f:(fun cell -> Hash_set.add visited cell) ;
        let next_level =
          List.fold current_level ~init:[] ~f:(fun acc cell ->
              let current_cell_neighbors =
                get_connected_neighbors t cell
                |> List.filter ~f:(fun cell -> not (Hash_set.mem visited cell))
              in
              acc @ current_cell_neighbors )
        in
        1 + do_bfs next_level )
    in
    do_bfs [(row, col)]

  let prepare_for_part2 t start_color =
    let map =
      Array.map t.map ~f:(fun row ->
          Array.append row [|GROUND|] |> Array.append [|GROUND|] )
    in
    let ground_row = Array.map (Array.get map 0) ~f:(fun _ -> GROUND) in
    let map = Array.append map [|ground_row|] |> Array.append [|ground_row|] in
    let color =
      Array.map t.color ~f:(fun row ->
          let inner_color =
            Array.map row ~f:(fun elem -> if elem = start_color then 1 else 0)
          in
          Array.append inner_color [|0|] |> Array.append [|0|] )
    in
    let zero_row = Array.map (Array.get color 0) ~f:(fun _ -> 0) in
    let color = Array.append color [|zero_row|] |> Array.append [|zero_row|] in
    {map; color}

  let part2_get_count_inside t =
    let visited =
      Hash_set.create
        ( module struct
          type t = int * int [@@deriving compare, hash, equal, sexp]
        end )
    in
    (* cell (row, col) -> (row * 2, col * 2) *)
    let can_exist_on (row, col) =
      if
        row < 0 || col < 0
        || row > (Array.length t.map - 1) * 2
        || col > (Array.length (Array.get t.map 0) - 1) * 2
      then false
      else if row % 2 = 0 then
        if col % 2 = 0 then
          Array.get (Array.get t.color (row / 2)) (col / 2) = 0
        else
          Array.get (Array.get t.color (row / 2)) (col / 2) = 0
          || Array.get (Array.get t.color (row / 2)) ((col / 2) + 1) = 0
          ||
          match
            ( Array.get (Array.get t.map (row / 2)) (col / 2)
            , Array.get (Array.get t.map (row / 2)) ((col / 2) + 1) )
          with
          | HORIZONTAL, HORIZONTAL -> false
          | HORIZONTAL, NWL -> false
          | HORIZONTAL, SWL -> false
          | NEL, HORIZONTAL -> false
          | NEL, NWL -> false
          | NEL, SWL -> false
          | SEL, HORIZONTAL -> false
          | SEL, NWL -> false
          | SEL, SWL -> false
          | _ -> true
      else if col % 2 = 0 then
        Array.get (Array.get t.color (row / 2)) (col / 2) = 0
        || Array.get (Array.get t.color ((row / 2) + 1)) (col / 2) = 0
        ||
        match
          ( Array.get (Array.get t.map (row / 2)) (col / 2)
          , Array.get (Array.get t.map ((row / 2) + 1)) (col / 2) )
        with
        | VERTICAL, VERTICAL -> false
        | VERTICAL, NWL -> false
        | VERTICAL, NEL -> false
        | SWL, VERTICAL -> false
        | SWL, NWL -> false
        | SWL, NEL -> false
        | SEL, VERTICAL -> false
        | SEL, NWL -> false
        | SEL, NEL -> false
        | _ -> true
      else true
    in
    let rec fill (row, col) =
      if Hash_set.mem visited (row, col) then 0
      else (
        Hash_set.add visited (row, col) ;
        let next_count =
          [(row - 1, col); (row, col + 1); (row + 1, col); (row, col - 1)]
          |> List.filter ~f:can_exist_on
          |> List.fold ~init:0 ~f:(fun acc cell -> acc + fill cell)
        in
        if row % 2 = 0 && col % 2 = 0 then (
          Array.set (Array.get t.color (row / 2)) (col / 2) 2 ;
          1 + next_count )
        else next_count )
    in
    let _ = fill (0, 0) in
    Array.fold t.color ~init:0 ~f:(fun acc row ->
        acc
        + Array.fold row ~init:0 ~f:(fun acc elem ->
              if elem = 0 then acc + 1 else acc ) )

  let get_color t (row, col) = Array.get (Array.get t.color row) col

  let print t =
    printf "Pipes %d x %d\n" (Array.length t.map)
      (Array.length (Array.get t.map 0)) ;
    Array.iter t.map ~f:(fun row ->
        Array.iter row ~f:(fun elem ->
            let curr =
              match elem with
              | VERTICAL -> '|'
              | HORIZONTAL -> '-'
              | NEL -> 'L'
              | NWL -> 'J'
              | SEL -> 'F'
              | SWL -> '7'
              | GROUND -> '.'
              | START -> 'S'
            in
            printf "%c" curr ) ;
        printf "\n" ) ;
    printf "Colors\n" ;
    Array.iter t.color ~f:(fun row ->
        Array.iter row ~f:(fun elem -> printf "%d " elem) ;
        printf "\n" )
end

let part1 () =
  let map = In_channel.input_lines In_channel.stdin |> Map.create in
  let start = Map.get_start map |> Option.value_exn in
  let map = Map.resolve_start map in
  printf "%d\n" (Map.get_farthest_distance_in_same_loop map start)

let part2 () =
  let map = In_channel.input_lines In_channel.stdin |> Map.create in
  let start = Map.get_start map |> Option.value_exn in
  let map = Map.resolve_start map in
  let start_color = Map.get_color map start in
  let map = Map.prepare_for_part2 map start_color in
  printf "%d\n" (Map.part2_get_count_inside map)
