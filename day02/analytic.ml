type operator = Add | Mult

let print_op = function
  | Add -> "+"
  | Mult -> "*"

type op_args = { operator: operator; arg1: int; arg2: int; dest: int }

type operation =
  | Stop
  | BinOp of op_args

type program = operation list

type data = int array

let raw_input = CCIO.read_all stdin
  |> CCString.split_on_char ','
  |> CCList.filter_map CCInt.of_string

let rec parse_input = function
  | 99::_ -> [Stop]
  | op::arg1::arg2::dest::tl ->
    let
      operator = match op with
        | 1 -> Add
        | 2 -> Mult
        | _ -> failwith "Invalid opcode"
    in
    let this_operation = BinOp { operator; arg1; arg2; dest } in
    this_operation :: (parse_input tl)
  | _ -> failwith "Invalid input"

let get_data = CCArray.of_list

let eval_op = function
  | Add -> (+)
  | Mult -> ( * )

let rec direct_eval (program: program) (data: data) =
  match program with
  | [] | Stop::_ -> ()
  | BinOp { operator; arg1; arg2; dest }::tl ->
      let arg1_val = data.(arg1)
      and arg2_val = data.(arg2)
      in
      let res = eval_op operator arg1_val arg2_val in
      if dest mod 4 = 0 then
        Format.printf "Tried to override the opcode at position %d with value %d\n" dest res;
      data.(dest) <- res;
      direct_eval tl data

(* Symbolic calculus *)
type symbolic_expr =
  | Const of int
  | Input1
  | Input2
  | SymbOp of operator * symbolic_expr * symbolic_expr

let rec pp_symbolic_expr formatter = function
  | Const x -> Format.pp_print_int formatter x
  | Input1 -> Format.pp_print_string formatter "Input1"
  | Input2 -> Format.pp_print_string formatter "Input2"
  | SymbOp (op, e1, e2) ->
      Format.fprintf formatter "(%a %s %a)" pp_symbolic_expr e1 (print_op op) pp_symbolic_expr e2

let eval_symbolic input1 input2 = 
  let rec aux = function
    | Const x -> x
    | Input1 -> input1
    | Input2 -> input2
    | SymbOp (op, e1, e2) ->
        let v1 = aux e1 and v2 = aux e2 in
        eval_op op v1 v2
  in aux

let rec normalize = function
  | Const x -> Const x
  | Input1 -> Input1
  | Input2 -> Input2
  | SymbOp (op, e1, e2) -> match (normalize e1, normalize e2) with
    | (e1', Input1) -> SymbOp (op, Input1, e1') (* Always put the unknow first to make further simplifications easier *)
    | (e1', Input2) -> SymbOp (op, Input2, e1')
    | (SymbOp (op', e11, e12), e2') when op = op' -> (SymbOp (op, e11, SymbOp (op, e12, e2')))
    | (e1', SymbOp (op', e21, e22)) when op = op' -> (SymbOp (op, e21, SymbOp (op, e22, e1')))
    | (x, y) -> SymbOp (op, x, y)

let simplify_symbolic tree =
  let rec aux = function
    | SymbOp (op, e1, e2) -> begin match (aux e1, aux e2) with
      | (Const x, Const y) -> Const (eval_op op  x y)
      | (e1', e2') -> SymbOp (op, e1', e2')
    end
    | x -> x
  in aux (normalize tree)


let rec symbolic_eval (program: program) (data: symbolic_expr array) =
  match program with
  | [] | Stop::_ -> ()
  | BinOp { operator; arg1; arg2; dest }::tl ->
      let arg1_val = data.(arg1)
      and arg2_val = data.(arg2)
      in
      let res = SymbOp (operator, arg1_val, arg2_val) in
      data.(dest) <- res;
      symbolic_eval tl data

;;

let _ =
  let data = CCArray.of_list (CCList.map (fun x -> Const (x)) raw_input) in
  data.(1) <- Input1;
  data.(2) <- Input2;
  symbolic_eval (parse_input raw_input) data;
  pp_symbolic_expr Format.std_formatter (simplify_symbolic data.(0));
  Format.print_newline ();
  for noun = 0 to 1024 do
    for verb = 0 to 1024 do
      let res = eval_symbolic noun verb data.(0) in
      if res = 19690720 then
        Format.printf "%d\n" (100 * noun + verb)
    done
  done;
