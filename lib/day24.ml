open Core
open Stdio

exception Parse_error of string

module Hailstone = struct
  type t =
    {x: Int63.t; y: Int63.t; z: Int63.t; vx: Int63.t; vy: Int63.t; vz: Int63.t}
  [@@deriving compare, equal, hash]

  let create input =
    let pattern =
      Re2.create_exn
        {|(\-?\d+),\s*(\-?\d+),\s*(\-?\d+)\s*@\s*(\-?\d+),\s*(\-?\d+),\s*(\-?\d+)|}
    in
    match Re2.find_submatches pattern input with
    | Ok [|_; Some x; Some y; Some z; Some vx; Some vy; Some vz|] ->
        (* Convert matched groups to Int63 and construct the record *)
        { x= Int63.of_string x
        ; y= Int63.of_string y
        ; z= Int63.of_string z
        ; vx= Int63.of_string vx
        ; vy= Int63.of_string vy
        ; vz= Int63.of_string vz }
    | _ -> raise (Parse_error ("Invalid input: " ^ input))

  let is_intersection_x_y_in_range t other start_range end_range =
    (* x = x1 + vx1 * t1; y = y1 + vy1 * t1

       (vx1, vx2) -> (-vx2, vx1) perp

       -vx2 * x + vx1 * y + c = 0 (x1, y1) -> c = vx2 * x1 - vx1 * y1

       let open Int63.O in let start_range_f = Float.of_int63 start_range in let
       end_range_f = Float.of_int63 end_range in if (t.x - other.x) * (other.vy
       - t.vy) = (t.y - other.y) * (other.vx - t.vx) then ( let
       intersection_time = Float.( / ) (Float.of_int63 (t.x - other.x))
       (Float.of_int63 (other.vx - t.vx)) in print_s [%sexp (intersection_time :
       Float.t)] ; let intersection_x = Float.(of_int63 t.x + (of_int63 t.vx *
       intersection_time)) in let intersection_y = Float.(of_int63 t.y +
       (of_int63 t.vy * intersection_time)) in if Float.( intersection_time >=
       0. && start_range_f <= intersection_x && intersection_x <= end_range_f &&
       start_range_f <= intersection_y && intersection_y <= end_range_f ) then
       true else false ) else false *)
    let open Float.O in
    let start_range_f = Float.of_int63 start_range in
    let end_range_f = Float.of_int63 end_range in
    let x1 = Float.of_int63 t.x in
    let y1 = Float.of_int63 t.y in
    let vx1 = Float.of_int63 t.vx in
    let vy1 = Float.of_int63 t.vy in
    let x2 = Float.of_int63 other.x in
    let y2 = Float.of_int63 other.y in
    let vx2 = Float.of_int63 other.vx in
    let vy2 = Float.of_int63 other.vy in
    let det a b c d = (a * d) - (b * c) in
    let crammer_bottom = det vx1 (-vx2) vy1 (-vy2) in
    let t1_top = det (x2 - x1) (-vx2) (y2 - y1) (-vy2) in
    let t2_top = det vx1 (x2 - x1) vy1 (y2 - y1) in
    if
      crammer_bottom = 0.
      || t1_top * crammer_bottom < 0.
      || t2_top * crammer_bottom < 0.
    then false
    else
      let t1 = t1_top / crammer_bottom in
      let intersection_x = x1 + (vx1 * t1) in
      let intersection_y = y1 + (vy1 * t1) in
      start_range_f <= intersection_x
      && intersection_x <= end_range_f
      && start_range_f <= intersection_y
      && intersection_y <= end_range_f
end

module Vector = struct
  type t = {x: Float.t; y: Float.t; z: Float.t}

  let create x y z = {x; y; z}

  let dot_product t v = (t.x *. v.x) +. (t.y *. v.y) +. (t.z *. v.z)

  let cross_product t v =
    let x = (t.y *. v.z) -. (t.z *. v.y) in
    let y = (t.z *. v.x) -. (t.x *. v.z) in
    let z = (t.x *. v.y) -. (t.y *. v.x) in
    {x; y; z}

  let ( + ) t v = {x= t.x +. v.x; y= t.y +. v.y; z= t.z +. v.z}

  let ( * ) t s = {x= t.x *. s; y= t.y *. s; z= t.z *. s}
end

let solve_with_3_hailstones (h1 : Hailstone.t) (h2 : Hailstone.t)
    (h3 : Hailstone.t) =
  let x12 = Float.of_int63 (Int63.( - ) h2.x h1.x) in
  let y12 = Float.of_int63 (Int63.( - ) h2.y h1.y) in
  let z12 = Float.of_int63 (Int63.( - ) h2.z h1.z) in
  let vx12 = Float.of_int63 (Int63.( - ) h2.vx h1.vx) in
  let vy12 = Float.of_int63 (Int63.( - ) h2.vy h1.vy) in
  let vz12 = Float.of_int63 (Int63.( - ) h2.vz h1.vz) in
  let x13 = Float.of_int63 (Int63.( - ) h3.x h1.x) in
  let y13 = Float.of_int63 (Int63.( - ) h3.y h1.y) in
  let z13 = Float.of_int63 (Int63.( - ) h3.z h1.z) in
  let vx13 = Float.of_int63 (Int63.( - ) h3.vx h1.vx) in
  let vy13 = Float.of_int63 (Int63.( - ) h3.vy h1.vy) in
  let vz13 = Float.of_int63 (Int63.( - ) h3.vz h1.vz) in
  let p12 = Vector.create x12 y12 z12 in
  let p13 = Vector.create x13 y13 z13 in
  let v12 = Vector.create vx12 vy12 vz12 in
  let v13 = Vector.create vx13 vy13 vz13 in
  let t1 =
    Vector.dot_product (Vector.cross_product p12 p13) v13
    /. Vector.dot_product (Vector.cross_product p13 v12) v13
  in
  let t2 =
    Vector.dot_product (Vector.cross_product p12 p13) v12
    /. Vector.dot_product (Vector.cross_product v13 p12) v12
  in
  let c1 = Vector.(p12 + (v12 * t1)) in
  let c2 = Vector.(p13 + (v13 * t2)) in
  let v =
    let scale = 1. /. (t2 -. t1) in
    Vector.((c2 + (c1 * -1.)) * scale)
  in
  let p = Vector.(c1 + (v * t1 * -1.)) in
  p.x +. p.y +. p.z +. Float.of_int63 h1.x +. Float.of_int63 h1.y
  +. Float.of_int63 h1.z

let part1 () =
  let hailstones =
    In_channel.input_lines In_channel.stdin |> List.map ~f:Hailstone.create
  in
  List.cartesian_product hailstones hailstones
  |> List.filter ~f:(fun (h1, h2) -> Hailstone.compare h1 h2 > 0)
  |> List.map ~f:(fun (h1, h2) ->
         Hailstone.is_intersection_x_y_in_range h1 h2
           (Int63.of_string "200000000000000")
           (Int63.of_string "400000000000000") )
  |> List.filter ~f:Fn.id |> List.length |> printf "%d\n"

let part2 () =
  let hailstones =
    In_channel.input_lines In_channel.stdin |> List.map ~f:Hailstone.create
  in
  match hailstones with
  | h1 :: h2 :: h3 :: _ -> solve_with_3_hailstones h1 h2 h3 |> printf "%.6f\n"
  | _ -> printf "wtf\n"
