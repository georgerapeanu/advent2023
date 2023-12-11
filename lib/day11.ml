open Core
open Stdio

let part1 () =
  let matrix = In_channel.input_lines In_channel.stdin |> Array.of_list in
  let row_map = ref [||] in
  let current_index = ref 0 in
  Array.iter matrix ~f:(fun row ->
      row_map :=
        Array.append !row_map
          ( if String.for_all row ~f:(Char.equal '.') then (
              current_index := !current_index + 2 ;
              [|!current_index - 2|] )
            else (
              current_index := !current_index + 1 ;
              [|!current_index - 1|] ) ) ) ;
  let col_map = ref [||] in
  let current_index = ref 0 in
  Array.get matrix 0
  |> String.iteri ~f:(fun col _ ->
         let is_columne_empty =
           Array.for_all matrix ~f:(fun row ->
               String.get row col |> Char.equal '.' )
         in
         col_map :=
           Array.append !col_map
             ( if is_columne_empty then (
                 current_index := !current_index + 2 ;
                 [|!current_index - 2|] )
               else (
                 current_index := !current_index + 1 ;
                 [|!current_index - 1|] ) ) ) ;
  let row_map = !row_map in
  let col_map = !col_map in
  let galaxies =
    Array.foldi matrix ~init:[||] ~f:(fun i acc row ->
        String.foldi row ~init:[||] ~f:(fun j acc elem ->
            if Char.equal elem '#' then Array.append acc [|(i, j)|] else acc )
        |> Array.append acc )
  in
  let sum = ref 0 in
  for i = 0 to Array.length galaxies - 1 do
    let x, y = Array.get galaxies i in
    let x = Array.get row_map x in
    let y = Array.get col_map y in
    for j = 0 to i - 1 do
      let xx, yy = Array.get galaxies j in
      let xx = Array.get row_map xx in
      let yy = Array.get col_map yy in
      sum := !sum + abs (x - xx) + abs (y - yy)
    done
  done ;
  printf "%d\n" !sum

let part2 () =
  let matrix = In_channel.input_lines In_channel.stdin |> Array.of_list in
  let row_map = ref [||] in
  let current_index = ref 0 in
  Array.iter matrix ~f:(fun row ->
      row_map :=
        Array.append !row_map
          ( if String.for_all row ~f:(Char.equal '.') then (
              current_index := !current_index + 1000000 ;
              [|!current_index - 1000000|] )
            else (
              current_index := !current_index + 1 ;
              [|!current_index - 1|] ) ) ) ;
  let col_map = ref [||] in
  let current_index = ref 0 in
  Array.get matrix 0
  |> String.iteri ~f:(fun col _ ->
         let is_columne_empty =
           Array.for_all matrix ~f:(fun row ->
               String.get row col |> Char.equal '.' )
         in
         col_map :=
           Array.append !col_map
             ( if is_columne_empty then (
                 current_index := !current_index + 1000000 ;
                 [|!current_index - 1000000|] )
               else (
                 current_index := !current_index + 1 ;
                 [|!current_index - 1|] ) ) ) ;
  let row_map = !row_map in
  let col_map = !col_map in
  let galaxies =
    Array.foldi matrix ~init:[||] ~f:(fun i acc row ->
        String.foldi row ~init:[||] ~f:(fun j acc elem ->
            if Char.equal elem '#' then Array.append acc [|(i, j)|] else acc )
        |> Array.append acc )
  in
  let sum = ref 0 in
  for i = 0 to Array.length galaxies - 1 do
    let x, y = Array.get galaxies i in
    let x = Array.get row_map x in
    let y = Array.get col_map y in
    for j = 0 to i - 1 do
      let xx, yy = Array.get galaxies j in
      let xx = Array.get row_map xx in
      let yy = Array.get col_map yy in
      sum := !sum + abs (x - xx) + abs (y - yy)
    done
  done ;
  printf "%d\n" !sum
