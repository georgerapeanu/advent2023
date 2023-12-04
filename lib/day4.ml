open Base
open Stdio

module Card = struct
  type t = {
    id: int; 
    winning_numbers: int list;
    card_numbers: int list
  }

  let string_to_card s =
    match String.split ~on:':' s with 
      | [header;rest] -> 
        let id = (String.drop_prefix header 4 |> String.strip |> Int.of_string) in 
        (match String.split ~on:'|' rest with
          | [first_part; second_part] -> 
              let winning_numbers = String.strip first_part |> String.split ~on: ' ' |> List.filter ~f: (fun x -> not (String.is_empty x)) |> List.map ~f: Int.of_string in 
              let card_numbers = String.strip second_part |> String.split ~on: ' ' |> List.filter ~f:(fun x -> not (String.is_empty x)) |> List.map ~f: Int.of_string in 
              {id; winning_numbers; card_numbers}
          | _ -> failwith "Card does not have the 2 parts")
      | _ -> failwith "Card does not have :"
 
  let get_winning_numbers_on_card card = 
    List.fold card.card_numbers ~init: 0 ~f:(
      fun acc x -> 
        if List.find ~f: (fun y -> x = y) card.winning_numbers |> Option.is_some then 
          acc + 1
        else 
          acc
    )
 
  let get_card_score card = 
    let count = (get_winning_numbers_on_card card) in 
    if count = 0 then 
      0 
    else 
      2 ** (count - 1)
end

let part1 () =
  let cards = List.map ~f:Card.string_to_card (In_channel.input_lines In_channel.stdin) in
  printf "%d" (List.map cards ~f: Card.get_card_score |> List.fold ~init:0 ~f: Int.(+))

let part2 () = 
  let cards = List.map ~f:Card.string_to_card (In_channel.input_lines In_channel.stdin) in
  let count_of_each = Hashtbl.create (module Int) in 
  
  List.iter cards ~f: (fun card -> 
    Hashtbl.set count_of_each ~key: card.id ~data: ((Hashtbl.find_or_add count_of_each card.id ~default: (fun () -> 0)) + 1);
    let current_card_count = (Hashtbl.find_or_add count_of_each card.id ~default: (fun () -> 0)) in
    for j = 1 to (min (Card.get_winning_numbers_on_card card) ((List.length cards) - 1)) do 
      Hashtbl.set count_of_each ~key: (card.id + j) ~data: ((Hashtbl.find_or_add count_of_each (card.id + j) ~default: (fun () -> 0)) + current_card_count);
    done
  );

  let total_count = Hashtbl.fold count_of_each ~init: 0 ~f: (fun ~key:_ ~data acc -> 
    acc + data
  ) in

  printf "%d" total_count

