module Html.Internal where
import GHC.Exts.Heap (StgInfoTable(code))

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

_l :: String -> [Structure] -> Structure
_l tag = Structure . el tag . concatMap (el "li" . getStructureString)

ul_ :: [Structure] -> Structure
ul_ = _l "ul"

ol_ :: [Structure] -> Structure
ol_ = _l "ol"

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar