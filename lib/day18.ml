open Core
open Stdio

module State = struct
  type t = {row: int; col: int} [@@deriving equal, hash, sexp, compare]
end

module State_with_comparator = struct
  include State
  include Comparator.Make (State)
end

let part1 () =
  let cells_on_row = Hashtbl.create (module Int) in
  let state_to_type = Hashtbl.create (module State_with_comparator) in
  let last_cell : State_with_comparator.t = {row= 0; col= 0} in
  let last_cell = ref last_cell in
  In_channel.input_lines In_channel.stdin
  |> List.iter ~f:(fun line ->
         let (dx, dy), count, _ =
           match String.split line ~on:' ' with
           | ["U"; x; s] -> ((-1, 0), Int.of_string x, s)
           | ["R"; x; s] -> ((0, 1), Int.of_string x, s)
           | ["D"; x; s] -> ((1, 0), Int.of_string x, s)
           | ["L"; x; s] -> ((0, -1), Int.of_string x, s)
           | _ -> failwith "unexpected line"
         in
         for _ = 1 to count do
           if dx <> 0 then
             Hashtbl.update state_to_type !last_cell ~f:(fun x ->
                 let prev_value =
                   Option.value_or_thunk x ~default:(fun () -> 0)
                 in
                 if dx = -1 then Int.( lor ) prev_value 1
                 else Int.( lor ) prev_value 2 ) ;
           last_cell := {row= !last_cell.row + dx; col= !last_cell.col + dy} ;
           if dx <> 0 then
             Hashtbl.update state_to_type !last_cell ~f:(fun x ->
                 let prev_value =
                   Option.value_or_thunk x ~default:(fun () -> 0)
                 in
                 if dx = -1 then Int.( lor ) prev_value 2
                 else Int.( lor ) prev_value 1 ) ;
           Hashtbl.find cells_on_row !last_cell.row
           |> Option.value_or_thunk ~default:(fun () -> [])
           |> (fun cells -> !last_cell.col :: cells)
           |> fun x -> Hashtbl.set cells_on_row ~key:!last_cell.row ~data:x
         done ) ;
  Hashtbl.fold cells_on_row ~init:0 ~f:(fun ~key ~data acc ->
      List.dedup_and_sort data ~compare:Int.compare
      |> List.fold ~init:(0, 0) ~f:(fun (acc_sum, acc_type) col ->
             let current_type =
               Hashtbl.find state_to_type {row= key; col}
               |> Option.value_or_thunk ~default:(fun () -> 0)
             in
             let next_acc_type = Int.( lxor ) acc_type current_type in
             if acc_type > 0 && next_acc_type = 0 then
               (acc_sum + col, next_acc_type)
             else if acc_type = 0 && next_acc_type > 0 then
               (acc_sum - col + 1, next_acc_type)
             else (acc_sum, next_acc_type) )
      |> fst |> Int.( + ) acc )
  |> printf "%d\n"

let part2 () =
  let last_cell = (0, 0) in
  let last_cell = ref last_cell in
  let boundry_cells = ref 0 in
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:(fun line ->
         let color = String.split line ~on:' ' |> List.last_exn in
         let count =
           String.drop_prefix color 2
           |> (fun color -> String.drop_suffix color 2)
           |> (fun color -> "0x" ^ color)
           |> Int.of_string
         in
         let dx, dy =
           match String.get color (String.length color - 2) with
           | '0' -> (-1, 0)
           | '1' -> (0, 1)
           | '2' -> (1, 0)
           | '3' -> (0, -1)
           | _ -> failwith "weird input"
         in
         let next_cell =
           (fst !last_cell + (dx * count), snd !last_cell + (dy * count))
         in
         let current_cell = !last_cell in
         last_cell := next_cell ;
         boundry_cells := !boundry_cells + count ;
         if dy = 1 then -count * fst current_cell
         else if dy = -1 then count * fst current_cell
         else 0 )
  |> List.fold ~init:0 ~f:Int.( + )
  |> (fun internal_area -> abs internal_area + 1 + (!boundry_cells / 2))
  |> printf "%d\n"
