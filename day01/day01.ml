let rec fuel_for (elem_mass : int) : int =
  let needed_fuel = (elem_mass/3) - 2 in
  if needed_fuel <= 0
  then 0
  else needed_fuel + (fuel_for needed_fuel)

let fuel_all_modules (weights : int Gen.gen) : int = Gen.sum (Gen.map fuel_for weights)

let input = Gen.map int_of_string (CCIO.read_lines stdin);;

Format.printf "%d\n" (fuel_all_modules input)
