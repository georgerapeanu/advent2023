open Core
open Stdio

module Hand = struct
  type t = {cards: string; bid: int}

  type hand_type =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
  [@@deriving equal]

  let cards_priority = "AKQJT98765432"

  let hand_type_priority =
    [| FiveOfAKind
     ; FourOfAKind
     ; FullHouse
     ; ThreeOfAKind
     ; TwoPair
     ; OnePair
     ; HighCard |]

  let get_hand_type t =
    let fr =
      String.fold t.cards
        ~init:(Hashtbl.create (module Char))
        ~f:(fun acc card ->
          let current_fr =
            Hashtbl.find_or_add acc card ~default:(fun () -> 0)
          in
          Hashtbl.set acc ~key:card ~data:(current_fr + 1) ;
          acc )
    in
    let fr_list =
      Hashtbl.to_alist fr
      |> List.sort ~compare:(fun (_, fr1) (_, fr2) -> Int.compare fr1 fr2)
      |> List.rev
    in
    match fr_list with
    | [_] -> FiveOfAKind
    | [(_, 4); (_, 1)] -> FourOfAKind
    | [(_, 3); (_, 2)] -> FullHouse
    | (_, 3) :: _ -> ThreeOfAKind
    | [(_, 2); (_, 2); (_, 1)] -> TwoPair
    | (_, 2) :: _ -> OnePair
    | _ -> HighCard

  let compare t1 t2 =
    let t1_hand_type = get_hand_type t1 in
    let t2_hand_type = get_hand_type t2 in
    let t1_hand_index, _ =
      Array.findi hand_type_priority ~f:(fun _ x ->
          equal_hand_type x t1_hand_type )
      |> Option.value_exn
    in
    let t2_hand_index, _ =
      Array.findi hand_type_priority ~f:(fun _ x ->
          equal_hand_type x t2_hand_type )
      |> Option.value_exn
    in
    if t1_hand_index <> t2_hand_index then
      -Int.compare t1_hand_index t2_hand_index
    else
      let rec compare_tie t1 t2 =
        match (t1, t2) with
        | [], [] -> 0
        | h1 :: _, h2 :: _ when not (Char.equal h1 h2) ->
            -Int.compare
               (String.index_exn cards_priority h1)
               (String.index_exn cards_priority h2)
        | _ :: rest1, _ :: rest2 -> compare_tie rest1 rest2
        | _ -> failwith "Compare hands has weird shape"
      in
      compare_tie (String.to_list t1.cards) (String.to_list t2.cards)

  let of_string s =
    let split_string = String.split s ~on:' ' in
    let cards = List.hd_exn split_string in
    let bid = List.last_exn split_string |> Int.of_string in
    {cards; bid}
end

module Part2Hand = struct
  type t = {cards: string; bid: int}

  type hand_type =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
  [@@deriving equal]

  let cards_priority = "AKQT98765432J"

  let hand_type_priority =
    [| FiveOfAKind
     ; FourOfAKind
     ; FullHouse
     ; ThreeOfAKind
     ; TwoPair
     ; OnePair
     ; HighCard |]

  let get_hand_type t =
    let fr =
      String.fold t.cards
        ~init:(Hashtbl.create (module Char))
        ~f:(fun acc card ->
          let current_fr =
            Hashtbl.find_or_add acc card ~default:(fun () -> 0)
          in
          Hashtbl.set acc ~key:card ~data:(current_fr + 1) ;
          acc )
    in
    let fr_J =
      Hashtbl.find fr 'J' |> Option.value_or_thunk ~default:(fun () -> 0)
    in
    let fr_list =
      Hashtbl.to_alist fr
      |> List.sort ~compare:(fun (_, fr1) (_, fr2) -> Int.compare fr1 fr2)
      |> List.rev
    in
    let fr_list =
      List.filter fr_list ~f:(fun (c, _) -> not (Char.equal 'J' c))
    in
    let fr_list =
      match fr_list with
      | [] -> ('J', fr_J) :: fr_list
      | (h, fr) :: rest -> (h, fr + fr_J) :: rest
    in
    match fr_list with
    | [_] -> FiveOfAKind
    | [(_, 4); (_, 1)] -> FourOfAKind
    | [(_, 3); (_, 2)] -> FullHouse
    | (_, 3) :: _ -> ThreeOfAKind
    | [(_, 2); (_, 2); (_, 1)] -> TwoPair
    | (_, 2) :: _ -> OnePair
    | _ -> HighCard

  let compare t1 t2 =
    let t1_hand_type = get_hand_type t1 in
    let t2_hand_type = get_hand_type t2 in
    let t1_hand_index, _ =
      Array.findi hand_type_priority ~f:(fun _ x ->
          equal_hand_type x t1_hand_type )
      |> Option.value_exn
    in
    let t2_hand_index, _ =
      Array.findi hand_type_priority ~f:(fun _ x ->
          equal_hand_type x t2_hand_type )
      |> Option.value_exn
    in
    if t1_hand_index <> t2_hand_index then
      -Int.compare t1_hand_index t2_hand_index
    else
      let rec compare_tie t1 t2 =
        match (t1, t2) with
        | [], [] -> 0
        | h1 :: _, h2 :: _ when not (Char.equal h1 h2) ->
            -Int.compare
               (String.index_exn cards_priority h1)
               (String.index_exn cards_priority h2)
        | _ :: rest1, _ :: rest2 -> compare_tie rest1 rest2
        | _ -> failwith "Compare hands has weird shape"
      in
      compare_tie (String.to_list t1.cards) (String.to_list t2.cards)

  let of_string s =
    let split_string = String.split s ~on:' ' in
    let cards = List.hd_exn split_string in
    let bid = List.last_exn split_string |> Int.of_string in
    {cards; bid}
end

let part1 () =
  let hands =
    In_channel.input_lines In_channel.stdin
    |> Array.of_list
    |> Array.map ~f:Hand.of_string
  in
  Array.sort hands ~compare:Hand.compare ;
  let total_score =
    Array.foldi hands ~init:0 ~f:(fun i acc hand -> acc + (hand.bid * (i + 1)))
  in
  printf "%d\n" total_score

let part2 () =
  let hands =
    In_channel.input_lines In_channel.stdin
    |> Array.of_list
    |> Array.map ~f:Part2Hand.of_string
  in
  Array.sort hands ~compare:Part2Hand.compare ;
  let total_score =
    Array.foldi hands ~init:0 ~f:(fun i acc hand -> acc + (hand.bid * (i + 1)))
  in
  printf "%d\n" total_score
