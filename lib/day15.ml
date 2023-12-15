open Core
open Stdio

let rec hash s curr_value =
  if String.( = ) s "" then curr_value
  else
    hash (String.drop_prefix s 1)
      ( String.get s 0 |> Char.to_int |> Int.( + ) curr_value |> Int.( * ) 17
      |> fun x -> x % 256 )

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.filter ~f:(fun x -> not (String.is_empty x))
  |> String.concat |> String.split ~on:','
  |> List.map ~f:(fun x -> hash x 0)
  |> List.fold ~init:0 ~f:Int.( + )
  |> printf "\n%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.filter ~f:(fun x -> not (String.is_empty x))
  |> String.concat |> String.split ~on:','
  |> List.fold ~init:(Array.create ~len:256 []) ~f:(fun acc step ->
         let last_char = String.get step (String.length step - 1) in
         ( if Char.equal last_char '-' then
             let label = String.drop_suffix step 1 in
             let box = hash label 0 in
             Array.set acc box
               ( Array.get acc box
               |> List.filter ~f:(fun (l, _) -> not (String.equal l label)) )
           else
             let label = String.drop_suffix step 2 in
             let box = hash label 0 in
             let power = Char.to_int last_char - Char.to_int '0' in
             let rec add l =
               match l with
               | [] -> [(label, power)]
               | (l, _) :: rest when String.equal l label ->
                   (label, power) :: rest
               | hd :: rest -> hd :: add rest
             in
             Array.set acc box (Array.get acc box |> add) ) ;
         acc )
  |> Array.mapi ~f:(fun box_i box ->
         List.foldi box ~init:0 ~f:(fun lens_i acc (_, lens_pow) ->
             acc + (lens_pow * (box_i + 1) * (lens_i + 1)) ) )
  |> Array.fold ~init:0 ~f:Int.( + )
  |> printf "%d\n"
