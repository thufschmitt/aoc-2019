type machine = int array

let print_int = Format.printf "%d\n"

let raw_input_machine : int list =
  let raw_input = CCIO.read_all stdin
    |> CCString.split_on_char ','
    |> CCList.filter_map CCInt.of_string
  in
  raw_input

let op_binary (op : int -> int -> int) (machine : machine) (pos : int) : unit =
  let pos_first_operand = machine.(pos + 1)
  and pos_second_operand = machine.(pos + 2)
  and pos_result = machine.(pos + 3)
  in
  machine.(pos_result) <- op machine.(pos_first_operand) machine.(pos_second_operand)

let op_add = op_binary (+)
let op_mult = op_binary ( * )

(**
 * Mutates the machine in-place and returns `Some next_position` or `None` if
 * the machine should halt
*)
let eval_step (machine : machine) (current_pos : int) : int option =
  match machine.(current_pos) with
    | 1 -> op_add machine (current_pos); Some (current_pos + 4)
    | 2 -> op_mult machine (current_pos); Some (current_pos + 4)
    | 99 -> None (* Stopping condition *)
    | _ -> failwith "Invalid opcode"

let rec eval (machine : machine) (current_pos : int) : unit =
  match eval_step machine current_pos with
    | Some next_pos -> eval machine next_pos
    | None -> ()

;;

let apply_inputs noun verb =
  let machine = CCArray.of_list raw_input_machine in
  machine.(1) <- noun;
  machine.(2) <- verb;
  match eval machine 0 with
    | exception (Invalid_argument (_)) -> ()
    | _ ->
        let res = machine.(0) in
        if res = 19690720 then
          print_int (100 * noun + verb)
        else ()
;;

for noun = 0 to 1024 do
  for verb = 0 to 1024 do
    apply_inputs noun verb
  done
done
