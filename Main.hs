-- hello.hs

import Html ( Html, html_, p_, h1_, append_, render, ol_, ul_ )

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        -- (p_ "Paragraph #2")
        (ul_ [
          p_ "l1",
          p_ "l2"
        ])
      )
    )