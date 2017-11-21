module Lib
    ( parse
    ) where

import Data.Char (toLower)

type SearchText = String
type SearchSpace = String
type Result = Either String String

{- Helpers -}

punctuation :: [Char]
punctuation = [',', '.', '\'', '"']

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` punctuation

isMatch :: Char -> Char -> Bool
isMatch a b = toLower a == toLower b

clean :: SearchText -> SearchText
clean = filter (not . isPunctuation)

{-
Start with user's search text and the search space i.e. the article contents.
Remove punctuation from the search text
-}
parse :: SearchText -> SearchSpace -> Result
parse searchText searchSpace =
  parseOuter "" (clean searchText) searchSpace

{-
Advance through the search space, checking for matches. Keep track of characters
that came before. Succeeds only when `parseInner` succeeds, and fails if the
search space runs out
-}
parseOuter :: String -> SearchText -> SearchSpace -> Result
parseOuter before searchText [] = Left "No text left to search"
parseOuter before [] searchSpace = Left "Nothing left to search for"
parseOuter before (searchChar:searchText) (spaceChar:searchSpace) =
  let
    advanceOneChar =
      parseOuter (before ++ [spaceChar]) (searchChar:searchText) searchSpace

  in
    -- We don't care about punctuation except to keep track of it
    if isPunctuation spaceChar
      then advanceOneChar
      -- If we match on the first letter, try an 'inner' parse that adds the `<mark>`
      else if isMatch searchChar spaceChar
        then
          case parseInner ("<mark>" ++ [spaceChar]) searchText searchSpace of
            -- Inner parse failed; store the character and continue
            Left _ ->
              advanceOneChar

            -- Winner winner
            Right (before', after) ->
              Right (before ++ before' ++ "</mark>" ++ after)

        -- If the letter wasn't a match, store it and move on
        else advanceOneChar

{-
Similar to `parseOuter` except we have succeeded if `searchText` runs out, and we
quit immediately as soon as something doesn't match
-}
parseInner :: String -> SearchText -> SearchSpace -> Either String (String, SearchSpace)
parseInner before [] searchSpace = Right (before, searchSpace)
parseInner before searchText [] = Left "No text left to search"
parseInner before (searchChar:searchText) (spaceChar:searchSpace) =
  -- We still don't care about punctuation except to keep track of it
  if isPunctuation spaceChar
    then parseInner (before ++ [spaceChar]) (searchChar:searchText) searchSpace
    -- If we get a match, store the letter and continue, advancing the search
    else if isMatch searchChar spaceChar
      then parseInner (before ++ [spaceChar]) searchText searchSpace
      -- If we didn't get a match, exit the function
      else Left "No match"
