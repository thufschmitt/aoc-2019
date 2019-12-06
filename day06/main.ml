module StringMap = CCMap.Make(CCString)

module Orbitals = struct
  (** Type of an orbitals tree, linking each object to its parent *)
  type t = string StringMap.t

  let rec fold (orbitals: t) (init: 'a) (f: 'a -> string -> 'a) (start: string) : 'a =
    match StringMap.get start orbitals with
    | None -> init
    | Some x -> f (fold orbitals init f x) x

  let pp : t CCMap.printer = StringMap.pp CCString.print CCString.print

  let from_chan chan : t =
    let parse_line line = match CCString.split_on_char ')' line with
      | [center; orbiter] -> (orbiter, center)
      | _ -> failwith (Format.sprintf "Invalid line %s" line)
    in
    CCIO.read_lines chan
      |> Gen.map parse_line
      |> Sequence.of_gen
      |> StringMap.of_seq

  let number_of_centers orbitals = fold orbitals 0 (fun acc _ -> acc+1)

  let cumul_weights (orbitals: t) : int =
    orbitals
      |> StringMap.keys
      |> Sequence.map (number_of_centers orbitals)
      |> Sequence.fold (+) 0

  let parents orbitals = fold orbitals [] (fun x y -> y::x)
end

let part1 chan =
  let orbitals = Orbitals.from_chan chan in
  Orbitals.cumul_weights orbitals |> Format.printf "%d\n"

let shortest_path parents1 parents2 =
  let rec aux = function
    | [] -> failwith "Bleh"
    | hd::tl -> match CCList.find_idx ((=) hd) parents2 with
      | None -> 1 + aux tl
      | Some (idx, _) -> idx
  in aux parents1

let part2 chan =
  let orbitals = Orbitals.from_chan chan in
  let parents1 = Orbitals.parents orbitals "YOU"
  and parents2 = Orbitals.parents orbitals "SAN"
  in
  Format.printf "%d\n" (shortest_path parents1 parents2)

let _ = part2 stdin
