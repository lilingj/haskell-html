-- Markup.hs

module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural
import Data.Maybe
import Distribution.Simple.Program.HcPkg (list)

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]


parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      appendIfExist context ((Heading 1 . trim $ line) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just . UnorderedList $ list <> [trim line]) rest
        _ ->
          appendIfExist context $ parseLines (Just . UnorderedList $ [trim line]) rest

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just . OrderedList $ list <> [trim line]) rest
        _ -> 
          appendIfExist context $ parseLines (Just . OrderedList $ [trim line]) rest

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just . CodeBlock $ code <> [trim line]) rest
        _ ->
          appendIfExist context $ parseLines (Just . CodeBlock $ [line]) rest


    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            appendIfExist context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just . Paragraph . unwords $ [paragraph, line]) rest
              _ ->
                appendIfExist context $ parseLines (Just . Paragraph $ line) rest

trim :: String -> String
trim = unwords . words

appendIfExist :: Maybe a -> [a] -> [a]
appendIfExist = maybe id (:)
