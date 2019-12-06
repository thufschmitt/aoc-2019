module StringMap = CCMap.Make(CCString)

module OrbitalTree = struct
  type t = (CCString.t list * string option) StringMap.t

  let pp : t CCMap.printer = StringMap.pp CCString.print
    (fun fmt -> Format.fprintf fmt "(%a)"
      (CCPair.pp
        (CCList.pp ~start:"[" ~stop:"]" CCString.print)
        (CCOpt.pp CCString.print)))

  let add_parent parent child orbitals =
    let update_child = function
      | None -> Some ([], Some parent)
      | Some (childs, _) -> Some (childs, Some parent)
    and update_parent = function
      | None -> Some ([child], None)
      | Some (childs, granddad) -> Some (child::childs, granddad)
    in
    orbitals
      |> StringMap.update child update_child
      |> StringMap.update parent update_parent

  let from_assocs : (string*string) Gen.t -> t =
    Gen.fold
      (fun accu (parent, child) -> add_parent parent child accu)
      StringMap.empty

  let from_stdin : t =
    let parse_line line = match CCString.split_on_char ')' line with
      | [center; orbiter] -> (center, orbiter)
      | _ -> failwith (Format.sprintf "Invalid line %s" line)
    in
    let assocs = CCIO.read_lines stdin |> Gen.map parse_line in
    from_assocs assocs

  type accum_weight = { weight: int; child_count: int }
  let rec weight_of (orbitals: t) (planet: string) : accum_weight =
    let (children, _) = StringMap.get_or ~default:([], None) planet orbitals in
    let children_weights = CCList.map (weight_of orbitals) children in
    let cumul_child_count = CCList.fold_left (fun accum { child_count; _} -> accum + child_count) 0 children_weights in
    {
      weight = cumul_child_count;
      child_count = cumul_child_count + 1;
    }

  let cumul_weights (orbitals: t) : int =
    orbitals
      |> StringMap.to_seq
      |> Sequence.map (fun (elt, _) -> weight_of orbitals elt)
      |> Sequence.fold (fun acc { weight; _ } -> acc + weight) 0

  let parents (orbitals: t) (elt: string) =
    let rec aux elt = match StringMap.get elt orbitals with
      | None -> []
      | Some (_, None) -> []
      | Some (_, Some parent) -> parent :: aux parent
    in aux elt
end

let part1 () =
  let orbitals = OrbitalTree.from_stdin in
  OrbitalTree.cumul_weights orbitals |> Format.printf "%d\n"

let shortest_path parents1 parents2 =
  let rec aux = function
    | [] -> failwith "Bleh"
    | hd::tl -> match CCList.find_idx ((=) hd) parents2 with
      | None -> 1 + aux tl
      | Some (idx, _) -> idx
    (* | (_::tl1, []) -> 1 + aux tl1 parents2 *)
    (* | (hd1::_, hd2::_) when hd1 = hd2 -> 0 *)
    (* | (_, _::tl2) -> 1 + aux l1 tl2 *)
  in aux parents1

let part2 () =
  let orbitals = OrbitalTree.from_stdin in
  let parents1 = OrbitalTree.parents orbitals "YOU"
  and parents2 = OrbitalTree.parents orbitals "SAN"
  in
  Format.printf "%a\n" (CCList.pp CCString.print) parents1;
  Format.printf "%a\n" (CCList.pp CCString.print) parents2;
  Format.printf "%d\n" (shortest_path parents1 parents2)

let _ = part2 ()
