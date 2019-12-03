module Coord = struct
  type t = { x : int; y: int }

  let manhattan_to_zero { x; y } = abs x + abs y

  let compare { x = x0; y = y0 } { x = x1; y = y1 } =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c

  let pp fmt {x; y} =
    Format.fprintf fmt "(%a, %a)" CCInt.pp x CCInt.pp y

  let origin = { x=0; y=0 }
end

module Direction = struct
  type t = Up | Down | Right | Left

  let walk origin n = function
    | Up -> { origin with Coord.y = origin.Coord.y + n }
    | Down -> { origin with Coord.y = origin.Coord.y - n }
    | Right -> { origin with Coord.x = origin.Coord.x + n }
    | Left -> { origin with Coord.x = origin.Coord.x - n }

  let from_char = function
    | 'R' -> Right
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | _ -> failwith "Invalid direction"
end

module Path = struct
  type t = (Direction.t * int) list

  let parse (input : string) : t =
    let parse_one_movement raw_movement =
        let direction = Direction.from_char (CCString.get raw_movement 0)
        and length = int_of_string (CCString.sub raw_movement 1 (CCString.length raw_movement - 1))
        in
        (direction, length)
    in
    CCList.map parse_one_movement (CCString.split_on_char ',' input)
end

module Grid = struct
  include CCMap.Make(Coord)

  let zip : 'a t -> 'b t -> ('a * 'b) t =
    let merge_fun _ = function
      | `Both (x, y) -> Some (x, y)
      | _ -> None
    in
    merge_safe ~f:merge_fun

  let add_path_segment grid current_coord cumul_distance (direction, length) =
    let new_places : (Coord.t*int) CCList.t =
      CCList.init
        length
        (fun step ->
          (Direction.walk current_coord step direction, cumul_distance + step))
    in
    (
      union (fun _ x _ -> Some x) grid (of_list new_places),
      Direction.walk current_coord length direction,
      cumul_distance + length
    )

  let from_path (init_coord : Coord.t) (path : Path.t) : int t =
    let (final_grid, _, _) =
      CCList.fold_left
        (fun (acc_grid, current_coord, cumul_distance) ->
          add_path_segment acc_grid current_coord cumul_distance)
        (empty, init_coord, 0)
        path
    in
    remove Coord.origin final_grid

  let from_stdin () : int t =
    read_line () |> Path.parse |> from_path Coord.origin
end

let _ =
  let g1 = Grid.from_stdin ()
  and g2 = Grid.from_stdin () in
  let intersections : (int*int) list = CCList.of_seq (Grid.values (Grid.zip g1 g2)) in
  Format.printf "%d\n"
    (CCList.fold_left (fun acc (d1, d2) -> min acc (d1+d2)) 99999999 intersections)
