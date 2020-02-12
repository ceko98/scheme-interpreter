import ParseScheme
import Parser ( parse )
import Evaluate
import TypeInstances

run :: Defines -> IO ()
run defines = do
  r <- getLine
  let p = eval defines $ parse valueParser r
  print p
  case p of
    Just (Function name args body) -> run $ (name, Function name args body) : defines
    _ -> run defines

main :: IO ()
main = run []
