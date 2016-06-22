
-- | Library for working with dates.
-- |
-- | * See notes below about the types used for `Date`, `Day` and `Month`. *

module Elm.Date
    ( module Virtual
    , Date(..), fromString
    , Time, toTime, fromTime
    , Day(..), year, month, day, dayOfWeek
    , hour, minute, second, millisecond
--  , now
    ) where


-- For re-export

import Data.Date.Component (Month(..)) as Virtual

-- Internal

import Data.JSDate
    ( JSDate, isValid, getTime
    , getFullYear, getMonth, getDate, getDay
    , getHours, getMinutes, getSeconds, getMilliseconds
    )

import Data.Date (Weekday(..), Month)
import Data.Maybe (fromJust)
import Data.Enum (toEnum)
import Data.Int (round)
import Elm.Result (Result(..))

import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Prelude ((<<<), (>>>), (<>), show, ($), (#), (+))


-- | Elm's `Date` type is implemented here in terms of Purescript's `JSDate`,
-- | which, in both cases, is a Javascript Date object.
type Date = JSDate


-- | Elm's `Day` type is equivalent to `Weekday` in Purescript.
-- |
-- | However, note that in Purescript the constructors spell out the name of the day
-- | ... i.e. `Saturday` instead of `Sat`.
type Day = Weekday


-- | An alias for units of time, representing milliseconds.
type Time = Number


-- | Attempt to read a date from a string.
fromString :: String -> Result String Date
fromString str =
    let
        result =
            fromStringImpl str

    in
        if isValid result
            then Ok result
            else Err ("unable to parse '" <> str <> "' as a date")


-- Basically, calls `new Date(d)`
foreign import fromStringImpl :: String -> Date


-- | Convert a `Date` to a time in milliseconds.
-- |
-- | A time is the number of milliseconds since
-- | [the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).
toTime :: Date -> Time
toTime = getTime


-- | Convert a time in milliseconds into a `Date`.
-- |
-- | A time is the number of milliseconds since
-- | [the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).
foreign import fromTime :: Time -> Date


-- | Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `1990`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
year :: Date -> Int
year = round <<< unsafePerformEff <<< getFullYear


-- | Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the month `June` as defined below.
-- |
-- | Note that in Purescript, the constructors for `Month` are fully spelled out,
-- | so it is 'June` instead of `Jun`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
month :: Date -> Month
month d =
    -- Should be safe, given what `getMonth` can return
    unsafePartial (fromJust (toEnum (round (unsafePerformEff (getMonth d)) + 1)))


-- | Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `23`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
day :: Date -> Int
day = round <<< unsafePerformEff <<< getDate


-- Day is an Enum, but the numbering is different, so this is best
jsDayToDay :: Partial => Int -> Day
jsDayToDay d =
    case d of
        0 -> Sunday
        1 -> Monday
        2 -> Tuesday
        3 -> Wednesday
        4 -> Thursday
        5 -> Friday
        6 -> Saturday
        _ -> crashWith ("Invalid day of week: " <> show d)


-- | Extract the day of the week for a given date. Given the date 23 June
-- | 1990 at 11:45AM this returns the day `Saturday` as defined below.
-- |
-- | * Note that in Purescript, the days of the week are fully spelled out,
-- | so it is `Thursday` instead of `Thu`. *
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
dayOfWeek :: Date -> Day
dayOfWeek =
    -- Should be safe, given what `getDay` can return
    unsafePartial $ jsDayToDay <<< round <<< unsafePerformEff <<< getDay


-- | Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `11`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
hour :: Date -> Int
hour = round <<< unsafePerformEff <<< getHours


-- | Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `45`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
minute :: Date -> Int
minute = round <<< unsafePerformEff <<< getMinutes


-- | Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
-- | this returns the integer `0`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
second :: Date -> Int
second = round <<< unsafePerformEff <<< getSeconds


-- | Extract the millisecond of a given date. Given the date 23 June 1990 at 11:45:30.123AM
-- | this returns the integer `123`.
-- |
-- | * As in the Elm implementation, this implicitly uses the current locale. *
millisecond :: Date -> Int
millisecond = round <<< unsafePerformEff <<< getMilliseconds


-- TODO: Once I move tasks here.
--
-- | Get the `Date` at the moment when this task is run.
-- |
-- | * Added in version 4.0.0 of elm-lang/core *
-- now :: Task x Date
-- now =
