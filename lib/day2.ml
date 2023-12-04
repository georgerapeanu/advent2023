open Core
open Base

module Color = struct
  module T = struct
    type t = 
      | Red
      | Green
      | Blue
    [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  (* Lot's of stuff. *)
end

type extraction = {
  color: Color.t;
  amount: int;
}

type extraction_round = extraction list

type game = {
  id: int;
  extraction_rounds: extraction_round list
}

let build_extraction_from_string s = 
  let description_list = String.split ~on:' ' (String.strip s) in

  if (List.length description_list) <> 2 then
    failwith ("Extraction match failed for " ^ s);

  let amount = Int.of_string (Option.value_exn (List.hd description_list)) in

  let e = match (String.lowercase (List.nth_exn description_list 1)) with
    | "red" -> (Option.some Color.Red);
    | "green" -> (Option.some Color.Green);
    | "blue" -> (Option.some Color.Blue);
    | _ -> None 
  in

  if Option.is_none e then
    failwith ("Invalid color " ^ (List.nth_exn description_list 1));

  {amount;color = (Option.value_exn e)}

let build_extraction_round_from_string s = 
  let description_list = String.split ~on:',' (String.strip s) in
  List.map description_list ~f: build_extraction_from_string

let build_game_from_string s = 
  let game = match String.split ~on:':' (String.strip s) with
    | [] -> None
    | game_part::description_part ->
        if (String.is_prefix ~prefix:"Game " game_part) then
          let game_id = (Int.of_string (String.drop_prefix game_part 5)) in
          let rounds = (List.map ~f: build_extraction_round_from_string (String.split ~on:';' (String.concat description_part))) in
          Option.some {id = game_id; extraction_rounds = rounds}
        else
          None
  in
  
  if Option.is_none game then
    failwith ("Invalid game " ^ s);
  
  Option.value_exn game

let part1 () =
  let games =
    List.map
      (In_channel.input_lines In_channel.stdin)
      ~f:build_game_from_string
  in
  let possible_games = List.filter
    games 
    ~f: (fun game -> 
      let round_summaries = List.map 
        ~f: (fun round -> 
          List.fold
            ~init: (Hashtbl.create(module Color))
            ~f: (fun acc ex -> 
              let current = Hashtbl.find_or_add acc ex.color ~default:(fun () -> 0) in
              Hashtbl.set acc ~key:ex.color ~data:(current + ex.amount);
              acc
            )
            round
        )
        game.extraction_rounds in
        List.for_all 
        ~f: (fun summary -> 
          (Option.value ~default:0 (Hashtbl.find summary Color.Red)) <= 12 && 
          (Option.value ~default:0 (Hashtbl.find summary Color.Green)) <= 13 && 
          (Option.value ~default:0 (Hashtbl.find summary Color.Blue)) <= 14
        )
        round_summaries
    )
  in
  printf "%d" (List.fold ~init:0 ~f:(fun acc game -> acc + game.id) possible_games)

let part2 () =
  let games =
    List.map
      (In_channel.input_lines In_channel.stdin)
      ~f:build_game_from_string
  in
  let power_of_minimal_sets_of_cubes: Int63.t list = List.map
    games 
    ~f: (fun game -> 
      let round_summaries = List.map 
        ~f: (fun round -> 
          List.fold
            ~init: (Hashtbl.create(module Color))
            ~f: (fun acc ex -> 
              let current = Hashtbl.find_or_add acc ex.color ~default:(fun () -> 0) in
              Hashtbl.set acc ~key:ex.color ~data:(current + ex.amount);
              acc
            )
            round
        )
        game.extraction_rounds 
      in

      let minimal_set_of_cubes = List.fold
        ~init: (Hashtbl.create(module Color))
        ~f: (fun acc summary -> 
          Hashtbl.iteri 
            summary
            ~f: (fun ~key:color ~data:amount -> 
              let current = Hashtbl.find_or_add acc color ~default: (fun () -> 0) in
              let current = max amount current in
              Hashtbl.set acc ~key: color ~data: current;
            );
          acc
        )
        round_summaries 
      in
      let red_count = (Int63.of_int (Option.value ~default:0 (Hashtbl.find minimal_set_of_cubes Color.Red))) in
      let green_count = (Int63.of_int (Option.value ~default:0 (Hashtbl.find minimal_set_of_cubes Color.Green))) in
      let blue_count = (Int63.of_int (Option.value ~default:0 (Hashtbl.find minimal_set_of_cubes Color.Blue))) in
      Int63.( * ) (Int63.( * ) red_count green_count) blue_count
    )
  in
  printf "%s" (Int63.to_string (List.fold ~init:(Int63.of_int 0) ~f:Int63.(+) power_of_minimal_sets_of_cubes))
