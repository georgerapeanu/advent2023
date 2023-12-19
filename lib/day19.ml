open Core
open Stdio

module Item = struct
  type t = (string, int) Hashtbl.t [@@deriving sexp_of]

  let create s =
    let props =
      String.drop_prefix s 1
      |> (fun s -> String.drop_suffix s 1)
      |> String.split ~on:','
    in
    List.fold props
      ~init:(Hashtbl.create (module String))
      ~f:(fun acc prop_string ->
        let eq_index = String.index prop_string '=' |> Option.value_exn in
        let name = String.prefix prop_string eq_index in
        let value =
          String.drop_prefix prop_string (eq_index + 1) |> Int.of_string
        in
        Hashtbl.set acc ~key:name ~data:value ;
        acc )

  let get_prop_exn t prop = Hashtbl.find_exn t prop

  let get_rating t =
    Hashtbl.fold t ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
end

module Item_range = struct
  type t = Item.t * Item.t [@@deriving sexp_of]

  let get_combinations t =
    let a, b = t in
    List.fold ["x"; "m"; "a"; "s"] ~init:1 ~f:(fun acc prop ->
        let b_prop = Item.get_prop_exn b prop in
        let a_prop = Item.get_prop_exn a prop in
        if a_prop > b_prop then 0 else acc * (b_prop - a_prop + 1) )
end

module Rule = struct
  type t =
    { accept: Item.t -> bool
    ; goto: string
    ; split_item_range: Item_range.t -> Item_range.t * Item_range.t }

  let create s =
    let args = String.split s ~on:':' in
    let goto = List.last_exn args |> String.strip in
    let condition = List.hd_exn args in
    let operator_idx =
      String.index condition '<'
      |> Option.value_or_thunk ~default:(fun () ->
             String.index condition '>' |> Option.value_exn )
    in
    let prop = String.prefix condition operator_idx in
    let value =
      String.drop_prefix condition (operator_idx + 1) |> Int.of_string
    in
    { goto
    ; accept=
        (fun x ->
          let obj_value = Item.get_prop_exn x prop in
          if Char.( = ) (String.get condition operator_idx) '<' then
            obj_value < value
          else obj_value > value )
    ; split_item_range=
        (fun (a, b) ->
          let a_copy = Hashtbl.copy a in
          let b_copy = Hashtbl.copy b in
          let result =
            if Char.( = ) (String.get condition operator_idx) '<' then (
              let split_1 = Hashtbl.copy b_copy in
              let split_2 = Hashtbl.copy a_copy in
              Hashtbl.set split_1 ~key:prop ~data:(value - 1) ;
              Hashtbl.set split_2 ~key:prop ~data:value ;
              ((a_copy, split_1), (split_2, b_copy)) )
            else
              let split_1 = Hashtbl.copy b_copy in
              let split_2 = Hashtbl.copy a_copy in
              Hashtbl.set split_1 ~key:prop ~data:value ;
              Hashtbl.set split_2 ~key:prop ~data:(value + 1) ;
              ((split_2, b_copy), (a_copy, split_1))
          in
          result ) }
end

module Pipeline = struct
  type t = {name: string; rules: Rule.t list; default: string}

  let create s =
    let name_length = String.index s '{' |> Option.value_exn in
    let name = String.prefix s name_length |> String.strip in
    let rule_strings =
      String.drop_prefix s (name_length + 1)
      |> (fun s -> String.drop_suffix s 1)
      |> String.split ~on:','
    in
    let default = List.last_exn rule_strings |> String.strip in
    let rule_strings = List.take rule_strings (List.length rule_strings - 1) in
    let rules = List.map rule_strings ~f:Rule.create in
    {name; rules; default}

  let run t item =
    List.fold t.rules ~init:None ~f:(fun acc rule ->
        if Option.is_none acc then
          if rule.accept item then Option.some rule.goto else acc
        else acc )
    |> Option.value_or_thunk ~default:(fun () -> t.default)

  let get_splits t item_range =
    List.fold t.rules ~init:([], item_range)
      ~f:(fun (splits, current_range) rule ->
        let accept_range, reject_range = rule.split_item_range current_range in
        ((accept_range, rule.goto) :: splits, reject_range) )
    |> (fun (splits, current_range) -> (current_range, t.default) :: splits)
    |> List.filter ~f:(fun (item_range, _goto) ->
           Item_range.get_combinations item_range > 0 )
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:String.strip
  |> (fun x ->
       let idx =
         List.findi x ~f:(fun _ x -> String.( = ) "" x)
         |> Option.value_exn |> fst
       in
       let pipelines, items = List.split_n x idx in
       let pipelines =
         List.map pipelines ~f:Pipeline.create
         |> List.map ~f:(fun pipeline -> (pipeline.name, pipeline))
         |> Hashtbl.of_alist_exn (module String)
       in
       let items = List.drop items 1 |> List.map ~f:Item.create in
       List.map items ~f:(fun item ->
           let state = ref "in" in
           while
             (not (String.( = ) !state "A")) && not (String.( = ) !state "R")
           do
             state := Pipeline.run (Hashtbl.find_exn pipelines !state) item
           done ;
           if String.( = ) !state "A" then Item.get_rating item else 0 )
       |> List.fold ~init:0 ~f:Int.( + ) )
  |> printf "%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:String.strip
  |> (fun x ->
       let idx =
         List.findi x ~f:(fun _ x -> String.( = ) "" x)
         |> Option.value_exn |> fst
       in
       let pipelines, _ = List.split_n x idx in
       let pipelines =
         List.map pipelines ~f:Pipeline.create
         |> List.map ~f:(fun pipeline -> (pipeline.name, pipeline))
         |> Hashtbl.of_alist_exn (module String)
       in
       let item_range =
         ( Hashtbl.of_alist_exn
             (module String)
             [("x", 1); ("m", 1); ("a", 1); ("s", 1)]
         , Hashtbl.of_alist_exn
             (module String)
             [("x", 4000); ("m", 4000); ("a", 4000); ("s", 4000)] )
       in
       let rec solve item_range state =
         if Item_range.get_combinations item_range = 0 || String.( = ) state "R"
         then 0
         else if String.( = ) state "A" then
           Item_range.get_combinations item_range
         else
           Pipeline.get_splits (Hashtbl.find_exn pipelines state) item_range
           |> List.map ~f:(fun (range, state) -> solve range state)
           |> List.fold ~init:0 ~f:Int.( + )
       in
       solve item_range "in" )
  |> printf "%d\n"
