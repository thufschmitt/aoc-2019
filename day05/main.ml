[@@@ocaml.warning "-8"]

module Machine = struct
  module Machine = struct
    type t = int array
  end
  type t = Machine.t

  module Operand = struct
    type t =
      | ByValue of int
      | ByReference of int

    let of_int = function
      | 0 -> Format.printf "ByRef\n"; fun x -> ByReference x
      | 1 -> Format.printf "ByVal\n"; fun x -> ByValue x
      | _ -> failwith "Invalid parameter type"

    let valueOf machine = function
      | ByValue x -> x
      | ByReference ref -> machine.(ref)

    let addressOf = function
      | ByValue _ -> failwith "Can't take the address of a value"
      | ByReference ref -> ref

    let parse_operands (machine: Machine.t) (operand_types: (int -> t) list) (pos: int) : t list =
      CCList.mapi
        (fun idx constructor -> constructor (machine.(pos+idx)))
        (CCList.rev operand_types)
  end

  module Operator = struct
    type next_instruction =
      | Rel of int
      | Abs of int
      | Stop

    type t = {
      eval: Machine.t -> Operand.t list -> next_instruction;
      arity : int;
    }

    let op_binary (op: int -> int -> int) : t =
      let
        eval machine args = match args with
          | [x;y;dest] -> machine.(Operand.addressOf dest) <- op (Operand.valueOf machine x) (Operand.valueOf machine y); Rel 4
          | _ -> failwith "Invalid number of arguments"
      in
      { eval; arity = 3; }
    let op_add = op_binary (+)
    let op_mult = op_binary ( * )

    let op_stop = {
      arity = 0;
      eval = (fun _ _ -> Stop;)
    }

    let op_input = {
      arity = 1;
      eval = (fun machine [x] -> machine.(Operand.addressOf x) <- read_int (); Rel 2);
    }

    let op_output = {
      arity = 1;
      eval = (fun machine [x] -> Format.printf "%d\n" (Operand.valueOf machine x); Rel 2);
    }

    let op_jump (condition: int -> bool) = {
      arity = 2;
      eval = (fun machine [tested_value; next_address] ->
        if condition (Operand.valueOf machine tested_value) then
          Abs (Operand.valueOf machine next_address)
        else Rel 3
      );
    }

    let op_compare (comp: int -> int -> bool) = op_binary (fun x y -> if comp x y then 1 else 0)


    let of_int : int -> t = function
      | 1 -> op_add
      | 2 -> op_mult
      | 3 -> op_input
      | 4 -> op_output
      | 5 -> op_jump (fun x -> x != 0)
      | 6 -> op_jump (fun x -> x = 0)
      | 7 -> op_compare (fun x y -> x < y)
      | 8 -> op_compare (fun x y -> x = y)
      | 99 -> op_stop
      | _ -> failwith "Invalid operator"

    let parse (raw_value: int) : (t * (int -> Operand.t) list) =
      let operator = of_int (raw_value mod 100) in
      let (_, operand_types) = CCList.fold_left
        (fun (remainder, accu) _ ->
          (remainder / 10, Operand.of_int (remainder mod 10) :: accu)
        )
        (raw_value / 100, [])
        (List.init (operator.arity) (fun _ -> ()))
      in
      (operator, operand_types)

  end


  let raw_input : int list =
    CCIO.with_in "day05/input.txt" (fun ic ->
      CCIO.read_all ic
      |> CCString.trim
      |> CCString.split_on_char ','
      |> CCList.filter_map CCInt.of_string
    )

  (**
  * Mutates the machine in-place and returns `Some next_position` or `None` if
  * the machine should halt
  *)
  let eval_step (machine : t) (current_pos : int) : int option =
    Format.printf "Current position: %d\n" current_pos;
    Format.printf "Current instruction: %d\n" machine.(current_pos);
    let (op, arg_constructors) = Operator.parse machine.(current_pos) in
    let next_instruction = op.Operator.eval machine (Operand.parse_operands machine arg_constructors (current_pos+1)) in
    match next_instruction with
      | Operator.Rel x -> Some (current_pos + x)
      | Abs x -> Some x
      | Stop -> None

  let rec eval (machine : t) (current_pos : int) : unit =
    match eval_step machine current_pos with
      | Some next_pos -> eval machine next_pos
      | None -> ()

end

let _ =
  let machine = CCArray.of_list Machine.raw_input in
  Machine.eval machine 0
