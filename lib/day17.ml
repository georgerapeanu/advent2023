open Core
open Stdio

module Matrix = struct
  type t = {rows: int; cols: int; items: int array array}
  [@@deriving compare, equal, sexp]

  let get t i j = Array.get (Array.get t.items i) j

  let set t i j v = Array.set (Array.get t.items i) j v

  let is_inside t i j = i >= 0 && i < t.rows && j >= 0 && j < t.cols

  let create s =
    let rows = List.length s in
    let cols = String.length (List.hd_exn s) in
    let items =
      List.map s ~f:(fun row ->
          String.to_list row
          |> List.map ~f:(fun x -> Char.to_int x - Char.to_int '0')
          |> Array.of_list )
      |> Array.of_list
    in
    {rows; cols; items}

  let copy t =
    { rows= t.rows
    ; cols= t.cols
    ; items= Array.map t.items ~f:(fun row -> Array.copy row) }
end

module State = struct
  type t = {row: int; col: int; dx: int; dy: int; dist: int}
  [@@deriving equal, hash, sexp]

  let compare t1 t2 =
    if t1.dist <> t2.dist then Int.compare t1.dist t2.dist
    else if t1.row <> t2.row then Int.compare t1.row t2.row
    else if t1.col <> t2.col then Int.compare t1.col t2.col
    else if t1.dx <> t2.dx then Int.compare t1.dx t2.dx
    else Int.compare t1.dy t2.dy
end

module State_with_comparator = struct
  include State
  include Comparator.Make (State)
end

module State_without_distance = struct
  type t = {row: int; col: int; dx: int; dy: int}
  [@@deriving compare, hash, sexp, equal]

  let of_state (t : State_with_comparator.t) =
    {row= t.row; col= t.col; dx= t.dx; dy= t.dy}
end

let get_minimum_heat_loss (matrix : Matrix.t) =
  let start_states : State.t list =
    [ {row= 0; col= 0; dx= 0; dy= 1; dist= 0}
    ; {row= 0; col= 0; dx= 1; dy= 0; dist= 0} ]
  in
  let state_set = ref (Set.empty (module State_with_comparator)) in
  let distances = Hashtbl.create (module State_without_distance) in
  List.iter start_states ~f:(fun state ->
      state_set := Set.add !state_set state ;
      Hashtbl.set distances
        ~key:(State_without_distance.of_state state)
        ~data:state.dist ) ;
  let answer = ref None in
  while Option.is_none !answer do
    let current_state = Set.min_elt_exn !state_set in
    state_set := Set.remove !state_set current_state ;
    if
      current_state.row = matrix.rows - 1 && current_state.col = matrix.cols - 1
    then answer := Option.some current_state.dist
    else
      let best_dist =
        Hashtbl.find distances (State_without_distance.of_state current_state)
      in
      match best_dist with
      | Some d when d < current_state.dist -> ()
      | _ ->
          let next_dirs =
            [ (-current_state.dy, current_state.dx)
            ; (current_state.dy, -current_state.dx) ]
          in
          let dist = ref current_state.dist in
          for i = 1 to 3 do
            let last_row = current_state.row + ((i - 1) * current_state.dx) in
            let last_col = current_state.col + ((i - 1) * current_state.dy) in
            if Matrix.is_inside matrix last_row last_col then
              dist := !dist + Matrix.get matrix last_row last_col ;
            List.map next_dirs ~f:(fun (dx, dy) : State_with_comparator.t ->
                { row= last_row + current_state.dx
                ; col= last_col + current_state.dy
                ; dx
                ; dy
                ; dist= !dist } )
            |> List.filter ~f:(fun state ->
                   Matrix.is_inside matrix state.row state.col )
            |> List.filter ~f:(fun state ->
                   match
                     Hashtbl.find distances
                       (State_without_distance.of_state state)
                   with
                   | None -> true
                   | Some dist -> state.dist < dist )
            |> List.iter ~f:(fun state ->
                   Hashtbl.set distances
                     ~key:(State_without_distance.of_state state)
                     ~data:state.dist ;
                   state_set := Set.add !state_set state )
          done
  done ;
  Option.value_exn !answer - Matrix.get matrix 0 0
  + Matrix.get matrix (matrix.rows - 1) (matrix.cols - 1)

let get_minimum_heat_loss_ultra_crucible (matrix : Matrix.t) =
  let start_states : State.t list =
    [ {row= 0; col= 0; dx= 0; dy= 1; dist= 0}
    ; {row= 0; col= 0; dx= 1; dy= 0; dist= 0} ]
  in
  let state_set = ref (Set.empty (module State_with_comparator)) in
  let distances = Hashtbl.create (module State_without_distance) in
  List.iter start_states ~f:(fun state ->
      state_set := Set.add !state_set state ;
      Hashtbl.set distances
        ~key:(State_without_distance.of_state state)
        ~data:state.dist ) ;
  let answer = ref None in
  while Option.is_none !answer do
    let current_state = Set.min_elt_exn !state_set in
    state_set := Set.remove !state_set current_state ;
    if
      current_state.row = matrix.rows - 1 && current_state.col = matrix.cols - 1
    then answer := Option.some current_state.dist
    else
      let best_dist =
        Hashtbl.find distances (State_without_distance.of_state current_state)
      in
      match best_dist with
      | Some d when d < current_state.dist -> ()
      | _ ->
          let next_dirs =
            [ (-current_state.dy, current_state.dx)
            ; (current_state.dy, -current_state.dx) ]
          in
          let dist = ref current_state.dist in
          for i = 1 to 10 do
            let last_row = current_state.row + ((i - 1) * current_state.dx) in
            let last_col = current_state.col + ((i - 1) * current_state.dy) in
            if Matrix.is_inside matrix last_row last_col then
              dist := !dist + Matrix.get matrix last_row last_col ;
            if i >= 4 then
              List.map next_dirs ~f:(fun (dx, dy) : State_with_comparator.t ->
                  { row= last_row + current_state.dx
                  ; col= last_col + current_state.dy
                  ; dx
                  ; dy
                  ; dist= !dist } )
              |> List.filter ~f:(fun state ->
                     Matrix.is_inside matrix state.row state.col )
              |> List.filter ~f:(fun state ->
                     match
                       Hashtbl.find distances
                         (State_without_distance.of_state state)
                     with
                     | None -> true
                     | Some dist -> state.dist < dist )
              |> List.iter ~f:(fun state ->
                     Hashtbl.set distances
                       ~key:(State_without_distance.of_state state)
                       ~data:state.dist ;
                     state_set := Set.add !state_set state )
          done
  done ;
  Option.value_exn !answer - Matrix.get matrix 0 0
  + Matrix.get matrix (matrix.rows - 1) (matrix.cols - 1)

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix.create |> get_minimum_heat_loss |> printf "%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> Matrix.create |> get_minimum_heat_loss_ultra_crucible |> printf "%d\n"
