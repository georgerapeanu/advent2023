open Core
open Stdio

let part1 () =
  let parsed_lines =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line -> String.split line ~on:':' |> List.last_exn)
    |> List.map ~f:(fun line -> String.split line ~on:' ')
    |> List.map ~f:(fun line -> List.map line ~f:String.strip)
    |> List.map ~f:(fun line ->
           List.filter line ~f:(fun item -> not (String.is_empty item)) )
    |> List.map ~f:(fun line -> List.map line ~f:Int.of_string)
  in
  let times = List.hd_exn parsed_lines in
  let distances = List.last_exn parsed_lines in
  let rec get_time_distance_pairs fst snd =
    match (List.hd fst, List.hd snd) with
    | Some f, Some s ->
        (f, s) :: get_time_distance_pairs (List.tl_exn fst) (List.tl_exn snd)
    | _ -> []
  in
  let time_distance_list = get_time_distance_pairs times distances in
  let answer =
    List.fold time_distance_list ~init:1 ~f:(fun acc (t, d) ->
        let current_count = ref 0 in
        for i = 0 to t do
          if (t - i) * i > d then current_count := !current_count + 1
        done ;
        acc * !current_count )
  in
  printf "%d\n" answer

let part2 () =
  let parsed_lines =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line -> String.split line ~on:':' |> List.last_exn)
    |> List.map ~f:(fun line -> String.split line ~on:' ')
    |> List.map ~f:(fun line -> List.map line ~f:String.strip)
    |> List.map ~f:(fun line ->
           List.filter line ~f:(fun item -> not (String.is_empty item)) )
    |> List.map ~f:String.concat |> List.map ~f:Int.of_string
  in
  let time = List.hd_exn parsed_lines in
  let distance = List.last_exn parsed_lines in
  (*(t-a)a >= d -> -a^2+ta-d >= 0 -> t2-4d*)
  let delta = (Float.of_int time ** 2.0) -. (Float.of_int distance *. 4.0) in
  let fst_root = (-.Float.of_int time +. Float.sqrt delta) /. -2.0 in
  let snd_root = (-.Float.of_int time -. Float.sqrt delta) /. -2.0 in
  let fst_valid = int_of_float fst_root + 1 in
  let lst_valid = int_of_float snd_root in
  printf "%d\n" (lst_valid - fst_valid + 1)
