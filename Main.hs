-- hello.hs

import Html ( Html, html_, p_, h_, render, ol_, ul_ )

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ "My title"
    $ h_ 1 "Heading"
      <> p_ "Paragraph #1"
      <> ul_ [p_ "l1", p_ "l2"]