type Defines = [(String, Value)]

run :: Defines -> IO ()
run defines = do
  r <- getLine
  let p = eval defines $ parse valueParser r
  print p
  case p of
    Just (Function name args body env) -> run $ (name, Function name args body env) : defines
    _ -> run defines

main :: IO ()
main = run []