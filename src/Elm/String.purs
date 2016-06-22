
-- | A built-in representation for efficient string manipulation. String literals
-- | are enclosed in `"double quotes"`. Strings are *not* lists of characters.
-- |
-- | *This is implemented in terms of Purescript's `String` type, so you can also
-- | use functions from `Data.String`.*

module Elm.String
    ( module Virtual
    , isEmpty, cons
    , startsWith, endsWith
    , reverse, repeat, concat
    , split, join, slice
    , map, filter, foldl, foldr
    , left, right, dropLeft, dropRight
    , pad, padLeft, padRight
    , trimLeft, trimRight
    , words, lines
    , any, all
    , indexes, indices
    , toInt, toFloat
    , toList, fromList
    , fromChar
    ) where


-- For re-export

import Data.String
    ( uncons, length, trim
    , toUpper, toLower, contains
    ) as Virtual

import Prelude (append) as Virtual


-- Internal

import Data.String (null, fromCharArray, toCharArray, singleton, length, take, drop, joinWith)
import Data.String as String
import Elm.Result (Result(..))
import Elm.Basics (Bool, Float)
import Elm.Char (isDigit)
import Data.List (List)
import Data.List as List
import Prelude ((<>), (<<<), (>>>), (-), (/), ($))
import Prelude as Prelude
import Data.Foldable (class Foldable, fold)
import Data.Unfoldable (class Unfoldable)
import Data.Array as Array


-- | Determine if a string is empty.
-- |
-- |     isEmpty "" == true
-- |     isEmpty "the world" == false
-- |
-- | Equivalent to Purescript's `null`.
isEmpty :: String -> Bool
isEmpty = null


-- | Add a character to the beginning of a string.
-- |
-- |     cons 'T' "he truth is out there" == "The truth is out there"
cons :: Char -> String -> String
cons c = (<>) (singleton c)


-- | Concatenate many strings into one.
-- |
-- |     concat ["never","the","less"] == "nevertheless"
-- |
-- | * Equivalent to Purescript's `fold` *
-- |
-- | * The signature uses `Foldable` to work with `List` or `Array`,
-- | among others. *
concat :: ∀ f. (Foldable f) => f String -> String
concat = fold


-- | Transform every character in a string
-- |
-- |     map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
map :: (Char -> Char) -> String -> String
map func string =
    fromCharArray $
        Prelude.map func (toCharArray string)


-- | Keep only the characters that satisfy the predicate.
-- |
-- |     filter isDigit "R2-D2" == "22"
filter :: (Char -> Bool) -> String -> String
filter func string =
    fromCharArray $
        Array.filter func (toCharArray string)


-- | Reverse a string.
-- |
-- |     reverse "stressed" == "desserts"
reverse :: String -> String
reverse string =
    -- It feels as though there should be a better way to do this
    fromCharArray $
        Array.reverse (toCharArray string)


-- | Reduce a string from the left.
-- |
-- |     foldl cons "" "time" == "emit"
foreign import foldl :: ∀ b. (Char -> b -> b) -> b -> String -> b


-- | Reduce a string from the right.
-- |
-- |     foldr cons "" "time" == "time"
foreign import foldr :: ∀ b. (Char -> b -> b) -> b -> String -> b


-- | Split a string using a given separator.
-- |
-- |     split "," "cat,dog,cow"        == ["cat","dog","cow"]
-- |     split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]
-- |
-- | Use `Regex.split` if you need something more flexible.
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
split :: ∀ f. (Unfoldable f) => String -> String -> f String
split sep s =
    Array.toUnfoldable $
        String.split sep s


-- | Put many strings together with a given separator.
-- |
-- |     join "a" ["H","w","ii","n"]        == "Hawaiian"
-- |     join " " ["cat","dog","cow"]       == "cat dog cow"
-- |     join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
join :: ∀ f. (Foldable f) => String -> f String -> String
join sep list = joinWith sep (Array.fromFoldable list)


-- | Repeat a string *n* times.
-- |
-- |     repeat 3 "ha" == "hahaha"
foreign import repeat :: Int -> String -> String


-- | Take a substring given a start and end index. Negative indexes
-- | are taken starting from the *end* of the list.
-- |
-- |     slice   7    9  "snakes on a plane!" == "on"
-- |     slice   0    6  "snakes on a plane!" == "snakes"
-- |     slice   0  (-7) "snakes on a plane!" == "snakes on a"
-- |     slice (-6) (-1) "snakes on a plane!" == "plane"
foreign import slice :: Int -> Int -> String -> String


-- | Take *n* characters from the left side of a string.
-- |
-- |     left 2 "Mulder" == "Mu"
-- |
-- | Equivalent to Purescript's `take`.
left :: Int -> String -> String
left = take


-- | Take *n* characters from the right side of a string.
-- |
-- |     right 2 "Scully" == "ly"
foreign import right :: Int -> String -> String


-- | Drop *n* characters from the left side of a string.
-- |
-- |     dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"
-- |
-- | Equivalent to Purescript's `drop`.
dropLeft :: Int -> String -> String
dropLeft = drop


-- | Drop *n* characters from the right side of a string.
-- |
-- |     dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"
foreign import dropRight :: Int -> String -> String


-- | Pad a string on both sides until it has a given length.
-- |
-- |     pad 5 ' ' "1"   == "  1  "
-- |     pad 5 ' ' "11"  == "  11 "
-- |     pad 5 ' ' "121" == " 121 "
pad :: Int -> Char -> String -> String
pad desiredLength padding string =
    let
        padder =
            singleton padding

        addSpaces =
            desiredLength - (length string)

        addToRight =
            addSpaces / 2

        addToLeft =
            addSpaces - addToRight

    in
        (repeat addToLeft padder) <>
        string <>
        (repeat addToRight padder)


-- | Pad a string on the left until it has a given length.
-- |
-- |     padLeft 5 '.' "1"   == "....1"
-- |     padLeft 5 '.' "11"  == "...11"
-- |     padLeft 5 '.' "121" == "..121"
padLeft :: Int -> Char -> String -> String
padLeft desiredLength padding string =
    let
        padder =
            singleton padding

        addSpaces =
            desiredLength - (length string)

    in
        (repeat addSpaces padder) <> string


-- | Pad a string on the right until it has a given length.
-- |
-- |     padRight 5 '.' "1"   == "1...."
-- |     padRight 5 '.' "11"  == "11..."
-- |     padRight 5 '.' "121" == "121.."
padRight :: Int -> Char -> String -> String
padRight desiredLength padding string =
    let
        padder =
            singleton padding

        addSpaces =
            desiredLength - (length string)

    in
        string <> (repeat addSpaces padder)


-- | Get rid of whitespace on the left of a string.
-- |
-- |     trimLeft "  hats  \n" == "hats  \n"
foreign import trimLeft :: String -> String


-- | Get rid of whitespace on the right of a string.
-- |
-- |     trimRight "  hats  \n" == "  hats"
foreign import trimRight :: String -> String


-- | Break a string into words, splitting on chunks of whitespace.
-- |
-- |     words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
words :: ∀ f. (Unfoldable f) => String -> f String
words = Array.toUnfoldable <<< _words


foreign import _words :: String -> Array String


-- | Break a string into lines, splitting on newlines.
-- |
-- |     lines "How are you?\nGood?" == ["How are you?", "Good?"]
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
lines :: ∀ f. (Unfoldable f) => String -> f String
lines = Array.toUnfoldable <<< _lines


foreign import _lines :: String -> Array String


-- | Determine whether *any* characters satisfy a predicate.
-- |
-- |     any isDigit "90210" == True
-- |     any isDigit "R2-D2" == True
-- |     any isDigit "heart" == False
foreign import any :: (Char -> Bool) -> String -> Bool


-- | Determine whether *all* characters satisfy a predicate.
-- |
-- |     all isDigit "90210" == True
-- |     all isDigit "R2-D2" == False
-- |     all isDigit "heart" == False
foreign import all :: (Char -> Bool) -> String -> Bool


-- | See if the second string starts with the first one.
-- |
-- |     startsWith "the" "theory" == True
-- |     startsWith "ory" "theory" == False
foreign import startsWith :: String -> String -> Bool


-- | See if the second string ends with the first one.
-- |
-- |     endsWith "the" "theory" == False
-- |     endsWith "ory" "theory" == True
foreign import endsWith :: String -> String -> Bool


-- | Get all of the indexes for a substring in another string.
-- |
-- |     indexes "i" "Mississippi"   == [1,4,7,10]
-- |     indexes "ss" "Mississippi"  == [2,5]
-- |     indexes "needle" "haystack" == []
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
indexes :: ∀ f. (Unfoldable f) => String -> String -> f Int
indexes little big =
    Array.toUnfoldable $
        _indexes little big


foreign import _indexes :: String -> String -> Array Int


-- | Alias for `indexes`.
indices :: ∀ f. (Unfoldable f) => String -> String -> f Int
indices = indexes


type ConversionHelper a =
    { ok :: a -> Result String a
    , err :: String -> Result String a
    , isDigit :: Char -> Boolean
    }

foreign import _toInt :: ConversionHelper Int -> String -> Result String Int
foreign import _toFloat :: ConversionHelper Float -> String -> Result String Float

conversionHelper :: ∀ a. ConversionHelper a
conversionHelper =
    { ok: Ok
    , err: Err
    , isDigit: isDigit
    }


-- | Try to convert a string into an int, failing on improperly formatted strings.
-- |
-- |     String.toInt "123" == Ok 123
-- |     String.toInt "-42" == Ok (-42)
-- |     String.toInt "3.1" == Err "could not convert string '3.1' to an Int"
-- |     String.toInt "31a" == Err "could not convert string '31a' to an Int"
-- |
-- | If you are extracting a number from some raw user input, you will typically
-- | want to use `Result.withDefault` to handle bad data:
-- |
-- |     Result.withDefault 0 (String.toInt "42") == 42
-- |     Result.withDefault 0 (String.toInt "ab") == 0
toInt :: String -> Result String Int
toInt = _toInt conversionHelper


-- | Try to convert a string into a float, failing on improperly formatted strings.
-- |
-- |     String.toFloat "123" == Ok 123.0
-- |     String.toFloat "-42" == Ok (-42.0)
-- |     String.toFloat "3.1" == Ok 3.1
-- |     String.toFloat "31a" == Err "could not convert string '31a' to a Float"
-- |
-- | If you are extracting a number from some raw user input, you will typically
-- | want to use `Result.withDefault` to handle bad data:
-- |
-- |     Result.withDefault 0.0 (String.toFloat "42.5") == 42.5
-- |     Result.withDefault 0.0 (String.toFloat "cats") == 0.0
toFloat :: String -> Result String Float
toFloat = _toFloat conversionHelper


-- | Convert a string to a list of characters.
-- |
-- |     toList "abc" == ['a','b','c']
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
toList :: ∀ f. (Unfoldable f) => String -> f Char
toList = Array.toUnfoldable <<< toCharArray


-- | Convert a list of characters into a String. Can be useful if you
-- | want to create a string primarily by consing, perhaps for decoding
-- | something.
-- |
-- |     fromList ['a','b','c'] == "abc"
-- |
-- | * Uses a polymorphic container to accommodate `List` and `Array`,
-- | among others. *
fromList :: ∀ f. (Foldable f) => f Char -> String
fromList = Array.fromFoldable >>> fromCharArray


-- | Create a string from a given character.
-- |
-- |     fromChar 'a' == "a"
-- |
-- | * Equivalent to Purescript's `singleton` *
fromChar :: Char -> String
fromChar = singleton
