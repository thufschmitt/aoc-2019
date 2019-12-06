module Password = struct
  type t = int array

  let pass_length = 6

  let incr_digit (password: t) (digit: int) : bool =
    password.(digit) <- (password.(digit) + 1) mod 10;
    password.(digit) == 0

  let find_first_invalid (password: t) : int option =
    let found_duplicate = ref false
    and previous_digit = ref (-1)
    and previous_digit_seq_length = ref 1
    in
    let check_duplicate current_digit =
      if !previous_digit == current_digit then begin
        incr previous_digit_seq_length
      end
      else begin
        let res = !previous_digit_seq_length == 2 in
        previous_digit_seq_length := 1;
        found_duplicate := !found_duplicate || res
      end
    in
    let exception FoundDecreasing of int in
    try
      for i=0 to (pass_length - 1) do
        check_duplicate password.(i);
        if !previous_digit > password.(i) then
          raise (FoundDecreasing i);
        previous_digit := password.(i)
      done;
      check_duplicate (-1);
      if !found_duplicate then None else Some 0
    with
      FoundDecreasing i -> Some i

  let is_valid password = CCOpt.is_none (find_first_invalid password)

  let from_int input =
    let rev_pass = CCArray.make pass_length 0 in
    for i=0 to (pass_length-1) do
      rev_pass.(i) <- (input / (CCInt.pow 10 i)) mod 10
    done;
    CCArray.rev rev_pass
end

let _ =
  let min_range = 382345
  and max_range = 843167
  in
  let all_passwords_within_range =
    Gen.init
      ~limit:(max_range-min_range)
      (fun x -> x + min_range)
    |> Gen.map Password.from_int
  in
  let valid_passwords =
    Gen.filter
      Password.is_valid
      all_passwords_within_range
  in
  Format.printf "%d\n" (Gen.length valid_passwords)
