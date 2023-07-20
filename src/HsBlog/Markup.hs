module HsBlog.Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe (maybeToList)

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)


parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      addIfExist context $ (Heading 1 . trim $ line) : parseLines Nothing rest

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just . UnorderedList $ list <> [trim line]) rest

        _ ->
          addIfExist context $ parseLines (Just . UnorderedList $ [trim line]) rest

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just . OrderedList $ list <> [trim line]) rest

        _ ->
          addIfExist context $ parseLines (Just . OrderedList $ [trim line]) rest

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just . CodeBlock $ code <> [line]) rest

        _ ->
          addIfExist context $ parseLines (Just . CodeBlock $ [line]) rest

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            addIfExist context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just . Paragraph . unwords $ [paragraph, line]) rest
              _ ->
                addIfExist context $ parseLines (Just . Paragraph $ line) rest

trim :: String -> String
trim = unwords . words

addIfExist :: Maybe a -> [a] -> [a]
addIfExist = maybe id (:)
