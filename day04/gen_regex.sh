function has_doubles_regex () {
  for i in {0..9}; do
    echo -n "(^|[^$i])$i$i($|[^$i])|"
  done
  echo -n "INVALID" # Definitely not match string to and the `|`
}
function has_decreasing_regex () {
  echo -n '('
  for i in {1..9}; do
    echo -n "$i(?!["
    for j in $(seq 0 $(($i-1))); do
      echo -n "$j"
    done
    echo -n "])|"
  done
  echo -n "INVALID" # Definitely not match string to and the `|`
  echo -n ')*$'
}

grep -P "^(?=$(has_decreasing_regex))(?=.*$(has_doubles_regex))"
