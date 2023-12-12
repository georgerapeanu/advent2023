open Core
open Stdio

module Springs = struct
  type t = {springs: string; summary: int array}

  let create s =
    match String.split s ~on:' ' with
    | [springs; summary] ->
        let summary =
          String.split summary ~on:','
          |> List.map ~f:Int.of_string |> Array.of_list
        in
        {springs; summary}
    | _ -> failwith "Input error"

  let can_be_group t start length =
    if start + length - 1 >= String.length t.springs then false
    else if start > 0 && Char.equal (String.get t.springs (start - 1)) '#' then
      false
    else if
      start + length < String.length t.springs
      && Char.equal (String.get t.springs (start + length)) '#'
    then false
    else
      let result = ref true in
      for i = start to start + length - 1 do
        if Char.equal (String.get t.springs i) '.' then result := false
      done ;
      !result

  let get_arrangements t =
    let dp =
      Array.create ~len:(String.length t.springs + 1) 0
      |> Array.map ~f:(fun _ ->
             Array.create ~len:(Array.length t.summary + 1) 0 )
    in
    Array.set (Array.get dp 0) 0 1 ;
    for i = 0 to String.length t.springs do
      for j = 0 to Array.length t.summary do
        let current_value = Array.get (Array.get dp i) j in
        ( if current_value > 0 then
            if
              i + 1 <= String.length t.springs
              && not (Char.equal (String.get t.springs i) '#')
            then
              let previous_value = Array.get (Array.get dp (i + 1)) j in
              Array.set (Array.get dp (i + 1)) j (previous_value + current_value)
        ) ;
        if j < Array.length t.summary then
          let summary_length = Array.get t.summary j in
          if can_be_group t i summary_length then
            let target_next =
              if i + summary_length >= String.length t.springs then
                String.length t.springs
              else i + summary_length + 1
            in
            let previous_value = Array.get (Array.get dp target_next) (j + 1) in
            Array.set (Array.get dp target_next) (j + 1)
              (previous_value + current_value)
      done
    done ;
    Array.get (Array.get dp (String.length t.springs)) (Array.length t.summary)

  let unfold_5_times t =
    let new_springs =
      String.concat ~sep:"?"
        [t.springs; t.springs; t.springs; t.springs; t.springs]
    in
    let new_summaries =
      Array.concat [t.summary; t.summary; t.summary; t.summary; t.summary]
    in
    {springs= new_springs; summary= new_summaries}
end

let part1 () =
  let sum =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:Springs.create
    |> List.map ~f:Springs.get_arrangements
    |> List.fold ~init:0 ~f:Int.( + )
  in
  printf "%d\n" sum

let part2 () =
  let sum =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:Springs.create
    |> List.map ~f:Springs.unfold_5_times
    |> List.map ~f:Springs.get_arrangements
    |> List.fold ~init:0 ~f:Int.( + )
  in
  printf "%d\n" sum
