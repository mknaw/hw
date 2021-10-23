module PrettyJSON
  (
    renderJValue
  ) where

import Prelude hiding ((<>))

import SimpleJSON(JValue(..))
import Prettify (
    Doc
  , (<>)
  , char
  , double
  , series
  , string
  , fsep
  , hcat
  , punctuate
  , text
  , compact
  , pretty
  )

renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where field (name, val)  = string name
                          <> text ": "
                          <> renderJValue val

