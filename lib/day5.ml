open Core
open Stdio

type int_int_pair = int * int [@@deriving compare]

module SingleMapping = struct
  type t = {start_from: int; start_to: int; length: int}

  let get_mapping t x =
    if t.start_from <= x && x < t.start_from + t.length then
      Option.some (x - t.start_from + t.start_to)
    else None

  let get_ranges_which_would_be_mapped t (s, e) =
    if e < t.start_from || t.start_from + t.length - 1 < s then None
    else Option.some (max t.start_from s, min (t.start_from + t.length - 1) e)

  let from_str s =
    let split_data =
      String.strip s |> String.split ~on:' ' |> List.map ~f:Int.of_string
    in
    { start_from= List.nth_exn split_data 1
    ; start_to= List.nth_exn split_data 0
    ; length= List.nth_exn split_data 2 }
end

module Mapping = struct
  type t = SingleMapping.t list

  let get_mapping t x =
    Option.value_or_thunk
      ~default:(fun () -> x)
      (List.fold t ~init:None ~f:(fun acc current_mapping ->
           match acc with
           | Some _ -> acc
           | None -> SingleMapping.get_mapping current_mapping x ) )

  let get_range_mapping t (s, e) =
    let mapped_ranges =
      List.map t ~f:(fun x ->
          SingleMapping.get_ranges_which_would_be_mapped x (s, e) )
      |> List.filter ~f:Option.is_some
      |> List.map ~f:(fun x -> Option.value_exn x)
      |> List.sort ~compare:compare_int_int_pair
    in
    let rec get_internal_unmapped_ranges = function
      | [] -> []
      | (_, first_end) :: (second_start, _) :: rest ->
          if first_end + 1 <= second_start - 1 then
            (first_end + 1, second_start - 1)
            :: get_internal_unmapped_ranges rest
          else get_internal_unmapped_ranges rest
      | _ -> []
    in
    let unmapped_internal_ranges = get_internal_unmapped_ranges mapped_ranges in
    let unmapped_ranges =
      if List.length mapped_ranges = 0 then [(s, e)]
      else
        let fst_s, _ = List.hd_exn mapped_ranges in
        let _, lst_e = List.last_exn mapped_ranges in
        let result = ref unmapped_internal_ranges in
        if s < fst_s then result := (s, fst_s - 1) :: !result ;
        if lst_e < e then result := (lst_e + 1, e) :: !result ;
        !result
    in
    let all_domain_ranges = List.concat [mapped_ranges; unmapped_ranges] in
    List.map all_domain_ranges ~f:(fun (s, e) ->
        (get_mapping t s, get_mapping t e) )

  let from_str_list s = List.map s ~f:SingleMapping.from_str
end

module Almanac = struct
  type t = {seeds: int list; maps: (string * string * Mapping.t) list}

  let from_str_list s =
    let s =
      List.map s ~f:String.strip
      |> List.filter ~f:(fun x -> not (String.is_empty x))
    in
    let current_from_state = ref "" in
    let current_to_state = ref "" in
    let running_list = ref [] in
    let result_list = ref [] in
    let seeds =
      List.hd_exn s
      |> (fun x -> String.drop_prefix x 7)
      |> String.split ~on:' ' |> List.map ~f:Int.of_string
    in
    let s = List.tl_exn s in
    List.iter s ~f:(fun current ->
        if String.contains current ':' then (
          let header = String.split current ~on:' ' |> List.hd_exn in
          if not (String.is_empty !current_from_state) then
            result_list :=
              ( !current_from_state
              , !current_to_state
              , Mapping.from_str_list !running_list )
              :: !result_list ;
          current_from_state := String.split header ~on:'-' |> List.hd_exn ;
          current_to_state := String.split header ~on:'-' |> List.last_exn ;
          running_list := [] )
        else running_list := current :: !running_list ) ;
    result_list :=
      ( !current_from_state
      , !current_to_state
      , Mapping.from_str_list !running_list )
      :: !result_list ;
    {seeds; maps= List.rev !result_list}

  let get_mapping t x =
    List.fold t.maps ~init:x ~f:(fun acc (_, _, mapping) ->
        Mapping.get_mapping mapping acc )

  let get_range_mapping t (s, e) =
    List.fold t.maps
      ~init:[(s, e)]
      ~f:(fun acc (_, _, mapping) ->
        List.concat_map acc ~f:(fun x -> Mapping.get_range_mapping mapping x) )
end

let part1 () =
  let almanac =
    Almanac.from_str_list (In_channel.input_lines In_channel.stdin)
  in
  let min_seed =
    List.fold almanac.seeds ~init:None ~f:(fun acc x ->
        let current_mapping = Almanac.get_mapping almanac x in
        if Option.is_none acc || current_mapping < Option.value_exn acc then
          Option.some current_mapping
        else acc )
  in
  printf "%d\n" (Option.value_exn min_seed)

let part2 () =
  let almanac =
    Almanac.from_str_list (In_channel.input_lines In_channel.stdin)
  in
  let rec get_pairs_list = function
    | [] -> []
    | x :: y :: rest -> (x, x + y - 1) :: get_pairs_list rest
    | _ -> []
  in
  let ranges = get_pairs_list almanac.seeds in
  let ranges = List.sort ~compare:compare_int_int_pair ranges in
  let ranges =
    List.fold ranges ~init:[] ~f:(fun acc (current_start, current_end) ->
        let last_range_opt = List.hd acc in
        match last_range_opt with
        | None -> [(current_start, current_end)]
        | Some (_, last_range_end) ->
            if last_range_end < current_start then
              (current_start, current_end) :: acc
            else if last_range_end < current_end then
              (last_range_end + 1, current_end) :: acc
            else acc )
  in
  let mapped_ranges =
    List.concat_map ranges ~f:(fun range ->
        Almanac.get_range_mapping almanac range )
  in
  let min_seed =
    List.fold mapped_ranges ~init:None ~f:(fun acc (s, _) ->
        if Option.is_none acc || Option.value_exn acc > s then Option.some s
        else acc )
  in
  printf "%d\n" (Option.value_exn min_seed)
