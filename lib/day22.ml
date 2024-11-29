open Core

module Brick = struct
  module T = struct
    type t =
      { index: int
      ; start_x: int
      ; start_y: int
      ; start_z: int
      ; end_x: int
      ; end_y: int
      ; end_z: int }
    [@@deriving hash, compare, equal, sexp]

    let create index input =
      let regex =
        Re2.create_exn "^(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)$"
      in
      match Re2.find_submatches regex input with
      | Ok [|_; Some sx; Some sy; Some sz; Some ex; Some ey; Some ez|] ->
          let temp_t =
            { index
            ; start_x= Int.of_string sx
            ; start_y= Int.of_string sy
            ; start_z= Int.of_string sz
            ; end_x= Int.of_string ex
            ; end_y= Int.of_string ey
            ; end_z= Int.of_string ez }
          in
          { index
          ; start_x= min temp_t.start_x temp_t.end_x
          ; start_y= min temp_t.start_y temp_t.end_y
          ; start_z= min temp_t.start_z temp_t.end_z
          ; end_x= max temp_t.start_x temp_t.end_x
          ; end_y= max temp_t.start_y temp_t.end_y
          ; end_z= max temp_t.start_z temp_t.end_z }
      | _ -> failwith "Invalid input format"

    let compare_z t other = Int.compare t.start_z other.start_z

    let list_of_xy t =
      List.cartesian_product
        (List.init (t.end_x - t.start_x + 1) ~f:(fun x -> x + t.start_x))
        (List.init (t.end_y - t.start_y + 1) ~f:(fun y -> y + t.start_y))

    let translate_z_to_settle t settle_z =
      {t with start_z= settle_z + 1; end_z= t.end_z - t.start_z + settle_z + 1}
  end

  include T
  include Comparable.Make (T)
end

module Tuple = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

module ThreeTuple = struct
  module T = struct
    type t = int * int * int [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Engine = struct
  type t = Brick.t list

  let create bricks =
    List.sort bricks ~compare:Brick.compare_z
    |> List.fold ~init:([], Tuple.Map.empty)
         ~f:(fun (settled_bricks, upmost_z) brick ->
           let settled_z =
             Brick.list_of_xy brick
             |> List.fold ~init:0 ~f:(fun max_z (x, y) ->
                    let current_z =
                      Map.find upmost_z (x, y) |> Option.value ~default:0
                    in
                    max max_z current_z )
           in
           let settled_brick = Brick.translate_z_to_settle brick settled_z in
           let new_upmost_z =
             Brick.list_of_xy brick
             |> List.fold ~init:upmost_z ~f:(fun upmost_z (x, y) ->
                    Map.set upmost_z ~key:(x, y) ~data:settled_brick.end_z )
           in
           (settled_brick :: settled_bricks, new_upmost_z) )
    |> fst

  let find_noncritical_bricks t =
    let top_support =
      List.fold t ~init:ThreeTuple.Map.empty ~f:(fun top_support brick ->
          Brick.list_of_xy brick
          |> List.fold ~init:top_support ~f:(fun top_support (x, y) ->
                 Map.set top_support ~key:(x, y, brick.end_z) ~data:brick ) )
    in
    let critical_bricks =
      List.fold t ~init:Brick.Set.empty ~f:(fun critical_bricks brick ->
          let support_set =
            Brick.list_of_xy brick
            |> List.fold ~init:Brick.Set.empty ~f:(fun support_bricks (x, y) ->
                   let support_brick_opt =
                     Map.find top_support (x, y, brick.start_z - 1)
                   in
                   match support_brick_opt with
                   | None -> support_bricks
                   | Some support_brick -> Set.add support_bricks support_brick )
          in
          if Set.length support_set <> 1 then critical_bricks
          else Set.union critical_bricks support_set )
    in
    List.length t - Set.length critical_bricks

  let count_chain_reaction t =
    let top_support =
      List.fold t ~init:ThreeTuple.Map.empty ~f:(fun top_support brick ->
          Brick.list_of_xy brick
          |> List.fold ~init:top_support ~f:(fun top_support (x, y) ->
                 Map.set top_support ~key:(x, y, brick.end_z) ~data:brick ) )
    in
    let get_support_bricks_set brick =
      Brick.list_of_xy brick
      |> List.fold ~init:Brick.Set.empty ~f:(fun support_set (x, y) ->
             let support_brick_opt =
               Map.find top_support (x, y, brick.start_z - 1)
             in
             match support_brick_opt with
             | None -> support_set
             | Some support_brick -> Set.add support_set support_brick )
    in
    (* Too stupid to figure out hashtbl functor *)
    let relying_critical_node = ref Brick.Map.empty in
    (* horrible code *)
    let rec lca brick1 brick2 =
      if Brick.(brick1 = brick2) then `Brick brick1
      else
        let brick1_critical = Map.find !relying_critical_node brick1 in
        let brick2_critical = Map.find !relying_critical_node brick2 in
        match (brick1_critical, brick2_critical) with
        | None, None -> `Ground
        | None, Some brick2_critical -> lca brick1 brick2_critical
        | Some brick1_critical, None -> lca brick1_critical brick2
        | Some brick1_critical, Some brick2_critical ->
            if brick1_critical.end_z < brick2_critical.end_z then
              lca brick1 brick2_critical
            else lca brick1_critical brick2
    in
    List.sort t ~compare:Brick.compare_z
    |> List.iter ~f:(fun brick ->
           let support_bricks = get_support_bricks_set brick in
           let critical_brick =
             Set.fold support_bricks ~init:`Undetermined ~f:(fun acc brick ->
                 match acc with
                 | `Undetermined -> `Brick brick
                 | `Ground -> `Ground
                 | `Brick lca_brick -> lca lca_brick brick )
           in
           match critical_brick with
           | `Undetermined | `Ground -> ()
           | `Brick critical_brick ->
               relying_critical_node :=
                 Map.set !relying_critical_node ~key:brick ~data:critical_brick ) ;
    let count_falling =
      List.sort t ~compare:Brick.compare_z
      |> List.rev
      |> List.fold ~init:Brick.Map.empty ~f:(fun count_falling brick ->
             let updated_for_current_brick_count_falling =
               Map.update count_falling brick ~f:(function
                 | None -> 1
                 | Some count -> count + 1 )
             in
             let current_count_falling =
               Map.find updated_for_current_brick_count_falling brick
               |> Option.value_exn
             in
             let critical_brick = Map.find !relying_critical_node brick in
             match critical_brick with
             | None -> updated_for_current_brick_count_falling
             | Some critical_brick ->
                 Map.update updated_for_current_brick_count_falling
                   critical_brick ~f:(function
                   | None -> current_count_falling
                   | Some count -> count + current_count_falling ) )
    in
    Map.fold count_falling ~init:0
      ~f:(fun ~key:_ ~data:current_count total_count ->
        current_count + total_count )
    - List.length t
end

let part1 () =
  In_channel.input_lines In_channel.stdin
  |> List.mapi ~f:Brick.create |> Engine.create
  |> Engine.find_noncritical_bricks |> printf "%d\n"

let part2 () =
  In_channel.input_lines In_channel.stdin
  |> List.mapi ~f:Brick.create |> Engine.create |> Engine.count_chain_reaction
  |> printf "%d\n"
