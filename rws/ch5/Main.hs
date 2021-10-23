import Prettify
import PrettyJSON
import SimpleJSON

-- main =
  -- let val = JObject [("foo", JNumber 1), ("bar", JBool False)]
      -- out = pretty 15 (renderJValue val)
  -- in putStrLn out

main =
  let val = JArray [JBool True, JBool False]
      out = docLength (renderJValue val)
  in print out
