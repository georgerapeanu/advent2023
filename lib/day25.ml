open Core
open Stdio

module Edge = struct
  module T = struct
    type t = {u: string; v: string} [@@deriving hash, equal, compare, sexp]

    let create u v = if String.( > ) u v then {u= v; v= u} else {u; v}

    let other t node = if String.( = ) t.u node then t.v else t.u
  end

  include T
  include Comparable.Make (T)
end

module Graph = struct
  type t = {nodes: string list; graph: Edge.t list String.Map.t}

  let create edges =
    let graph = ref String.Map.empty in
    List.iter edges ~f:(fun edge ->
        let {Edge.u; v} = edge in
        graph :=
          Map.update !graph u ~f:(fun edges ->
              edge :: Option.value edges ~default:[] ) ;
        graph :=
          Map.update !graph v ~f:(fun edges ->
              edge :: Option.value edges ~default:[] ) ) ;
    let nodes =
      List.map edges ~f:(fun {Edge.u; v} -> [u; v])
      |> List.concat
      |> List.dedup_and_sort ~compare:String.compare
    in
    {graph= !graph; nodes}

  let bfs_farthest_path t u =
    let visited = ref String.Set.empty in
    let to_visit = Queue.create () in
    let father = String.Table.create () in
    let last_visited = ref u in
    Queue.enqueue to_visit u ;
    visited := Set.add !visited u ;
    let rec process_current_cell () =
      match Queue.dequeue to_visit with
      | None -> ()
      | Some node ->
          last_visited := node ;
          Map.find t.graph node |> Option.value ~default:[]
          |> List.iter ~f:(fun edge ->
                 let other = Edge.other edge node in
                 if not (Set.mem !visited other) then (
                   visited := Set.add !visited other ;
                   Queue.enqueue to_visit other ;
                   Hashtbl.set father ~key:other ~data:edge ) ) ;
          process_current_cell ()
    in
    process_current_cell () ;
    let rec get_path node =
      match Hashtbl.find father node with
      | None -> []
      | Some edge -> edge :: get_path (Edge.other edge node)
    in
    get_path !last_visited

  let get_by_used_frequency t =
    let frequency =
      List.map t.nodes ~f:(fun node -> bfs_farthest_path t node)
      |> List.fold ~init:Edge.Map.empty ~f:(fun acc path ->
             List.fold path ~init:acc ~f:(fun acc edge ->
                 Map.update acc edge ~f:(fun freq ->
                     Option.value_map freq ~default:1 ~f:(( + ) 1) ) ) )
    in
    frequency

  let find_component_size t u =
    let visited = ref String.Set.empty in
    let to_visit = Queue.create () in
    let count = ref 0 in
    Queue.enqueue to_visit u ;
    visited := Set.add !visited u ;
    let rec process_current_cell () =
      match Queue.dequeue to_visit with
      | None -> ()
      | Some node ->
          count := !count + 1 ;
          Map.find t.graph node |> Option.value ~default:[]
          |> List.iter ~f:(fun edge ->
                 let other = Edge.other edge node in
                 if not (Set.mem !visited other) then (
                   visited := Set.add !visited other ;
                   Queue.enqueue to_visit other ) ) ;
          process_current_cell ()
    in
    process_current_cell () ; !count

  let rec identify_cut t times =
    if times = 0 then
      let count = find_component_size t (List.hd_exn t.nodes) in
      count * (List.length t.nodes - count)
    else
      let edge =
        let frequency = get_by_used_frequency t in
        print_s [%sexp (frequency : int Edge.Map.t)] ;
        Map.fold frequency ~init:None ~f:(fun ~key:edge ~data:fr acc ->
            match acc with
            | None -> Some (edge, fr)
            | Some (_, old_fr) when old_fr < fr -> Some (edge, fr)
            | Some old_acc -> Some old_acc )
        |> Option.value_exn |> fst
      in
      let next_graph =
        Map.map t.graph ~f:(fun edges ->
            List.filter edges ~f:(fun graph_edge ->
                not (Edge.equal graph_edge edge) ) )
      in
      print_s [%sexp (edge : Edge.t)] ;
      identify_cut {nodes= t.nodes; graph= next_graph} (times - 1)
end

(* Doesn't work on the example. too bad *)
let part1 () =
  let edges =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun s ->
           String.split s ~on:':'
           |> (function
                | [u; v_list] ->
                    (String.strip u, String.strip v_list |> String.split ~on:' ')
                | _ -> raise (Invalid_argument "input") )
           |> function
           | u, v_list -> List.map v_list ~f:(fun v -> Edge.create u v) )
    |> List.concat
  in
  let graph = Graph.create edges in
  Graph.identify_cut graph 3 |> printf "%d\n"

let part2 () = part1 ()
