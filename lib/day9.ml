open Core
open Stdio

module Sequence = struct
  type t = {items: int array; coefs: int array}

  let create items =
    let rec get_coefs curr_coefs =
      let curr_coefs_length = Array.length curr_coefs in
      let is_ok =
        Array.for_alli items ~f:(fun pos _ ->
            if pos < curr_coefs_length - 1 then true
            else
              Array.foldi ~init:0 curr_coefs ~f:(fun i acc coef ->
                  acc
                  + (coef * Array.get items (pos - curr_coefs_length + 1 + i)) )
              |> ( = ) 0 )
      in
      if is_ok then curr_coefs
      else
        let next_coefs =
          Array.mapi curr_coefs ~f:(fun pos value ->
              if pos = 0 then -value else Array.get curr_coefs (pos - 1) - value )
        in
        let next_coefs =
          Array.append next_coefs
            [|Array.get curr_coefs (curr_coefs_length - 1)|]
        in
        get_coefs next_coefs
    in
    let answer = {items; coefs= get_coefs [|1|]} in
    answer

  let predict_one t =
    let items_length = Array.length t.items in
    let coefs_length = Array.length t.coefs in
    Array.foldi t.coefs ~init:0 ~f:(fun i acc coef ->
        if i < coefs_length - 1 then
          acc + (coef * Array.get t.items (items_length - coefs_length + i + 1))
        else acc / -coef )

  let predict_one_backwards t =
    predict_one {items= Array.rev t.items; coefs= Array.rev t.coefs}
end

let part1 () =
  let answer =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun x ->
           String.split x ~on:' ' |> List.map ~f:Int.of_string )
    |> List.map ~f:Array.of_list
    |> List.map ~f:Sequence.create
    |> List.map ~f:Sequence.predict_one
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" answer

let part2 () =
  let answer =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun x ->
           String.split x ~on:' ' |> List.map ~f:Int.of_string )
    |> List.map ~f:Array.of_list
    |> List.map ~f:Sequence.create
    |> List.map ~f:Sequence.predict_one_backwards
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" answer
