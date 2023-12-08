open Core
open Stdio

module Transition = struct
  type t = {from: string; to_left: string; to_right: string}

  let from_str s =
    let from, to_ =
      String.split s ~on:'=' |> List.map ~f:String.strip
      |> fun x ->
      (List.hd_exn x |> String.strip, List.last_exn x |> String.strip)
    in
    let to_left, to_right =
      String.drop_prefix to_ 1
      |> (fun x -> String.drop_suffix x 1)
      |> String.split ~on:',' |> List.map ~f:String.strip
      |> fun x ->
      (List.hd_exn x |> String.strip, List.last_exn x |> String.strip)
    in
    Out_channel.flush Out_channel.stdout ;
    {from; to_left; to_right}
end

module Network = struct
  type t =
    {transition_table: (string, string * string) Hashtbl.t; sequence: string}

  module State = struct
    type t = {node: string; index: int} [@@deriving hash, equal, compare, sexp]
  end

  let create (transitions : Transition.t list) (sequence : string) =
    let transition_table : (string, string * string) Hashtbl.t =
      List.fold transitions
        ~init:(Hashtbl.create (module String))
        ~f:(fun acc current ->
          let () =
            Hashtbl.set acc ~key:current.from
              ~data:(current.to_left, current.to_right)
          in
          acc )
    in
    {transition_table; sequence}

  let get_next_state t (state : State.t) : State.t =
    let next_node =
      let to_left, to_right = Hashtbl.find_exn t.transition_table state.node in
      if Char.equal (String.get t.sequence state.index) 'L' then to_left
      else to_right
    in
    {node= next_node; index= (state.index + 1) % String.length t.sequence}

  let rec get_steps_to_zzz t (state : State.t) =
    if String.equal state.node "ZZZ" then 0
    else 1 + get_steps_to_zzz t (get_next_state t state)

  let rec get_cycle_details_internal t (state : State.t)
      (visited : (State.t, int) Hashtbl.t) =
    let last_visited_opt = Hashtbl.find visited state in
    match last_visited_opt with
    | None ->
        Hashtbl.set visited ~key:state ~data:(Hashtbl.length visited) ;
        get_cycle_details_internal t (get_next_state t state) visited
    | Some last_visited -> (Hashtbl.length visited - last_visited, last_visited)

  let get_cycle_details t (state : State.t) =
    get_cycle_details_internal t state (Hashtbl.create (module State))

  let get_crt_for_state t state =
    let cycle_length, tail_length = get_cycle_details t state in
    let rec check_state (state : State.t) remaining =
      let current_value =
        if String.is_suffix state.node ~suffix:"Z" then 1 else 0
      in
      if remaining = 0 then current_value
      else current_value + check_state (get_next_state t state) (remaining - 1)
    in
    if check_state state (cycle_length + tail_length) <> 1 then
      failwith "CRT doesnt work, state has multiple reminders"
    else
      let rec get_first_z_distance t (state : State.t) =
        if String.is_suffix state.node ~suffix:"Z" then 0
        else 1 + get_first_z_distance t (get_next_state t state)
      in
      let first_z_distance = get_first_z_distance t state in
      if first_z_distance < tail_length then
        failwith "CRT doesnt work, only z is before cycle"
      else (tail_length, cycle_length, first_z_distance - tail_length)
end

let part1 () =
  let lines =
    In_channel.input_lines In_channel.stdin
    |> List.filter ~f:(fun line ->
           String.strip line |> fun x -> not (String.is_empty x) )
  in
  let sequence = List.hd_exn lines in
  let body = List.tl_exn lines in
  let transitions = List.map body ~f:Transition.from_str in
  let network = Network.create transitions sequence in
  printf "%d\n" (Network.get_steps_to_zzz network {node= "AAA"; index= 0})

let part2 () =
  let lines =
    In_channel.input_lines In_channel.stdin
    |> List.filter ~f:(fun line ->
           String.strip line |> fun x -> not (String.is_empty x) )
  in
  let sequence = List.hd_exn lines in
  let body = List.tl_exn lines in
  let transitions = List.map body ~f:Transition.from_str in
  let network = Network.create transitions sequence in
  let result =
    List.map transitions ~f:(fun x -> x.from)
    |> List.filter ~f:(String.is_suffix ~suffix:"A")
    |> List.map ~f:(fun x ->
           Network.get_crt_for_state network {node= x; index= 0} )
    (* seems like cycles sync up, nice *)
    |> List.map ~f:(fun (_, cl, _) -> Int63.of_int cl)
    |> List.fold ~init:(Int63.of_int 0) ~f:(fun acc x ->
           if Int63.equal (Int63.of_int 0) acc then x
           else
             let rec gcd x y =
               if Int63.( = ) y (Int63.of_int 0) then x
               else gcd y (Int63.( % ) x y)
             in
             let lcm x y = Int63.( / ) (Int63.( * ) x y) (gcd x y) in
             lcm acc x )
  in
  printf "%s\n" (Int63.to_string result)
