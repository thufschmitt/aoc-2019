import           Data.List

main = print . length . flip filter [206938..679128] $ \nb ->
  let x = show nb in
  any ((==2) . length) (group x) && (sort x == x)
