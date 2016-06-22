module Test.Elm.Date (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert, equal)

import Elm.Date
import Prelude (class Eq, class Show, bind, flip, (<$>))
import Elm.Basics ((<|), (==))
import Data.Date.Component (Weekday(..))
import Data.JSDate (getTime)
import Elm.Result (toMaybe, Result(..))
import Data.Maybe (isNothing)


infixl 9 equals as ==>

equals :: ∀ a e. (Eq a, Show a) => a -> a -> Test e
equals = flip equal


tests :: ∀ e. TestSuite e
tests = suite "Date" do
    test "conversion" do
        assert "fromString good" <| (getTime <$> fromString "May 27, 2014 00:00 CDT") == Ok 1401166800000.0
        assert "fromString bad"  <| isNothing (toMaybe (fromString "bob")) 

        assert "fromTime" <| toTime (fromTime 1318258080000.0) == 1318258080000.0
        assert "toTime" <| (toTime <$> (fromString "2011-10-10T14:48:00Z")) == Ok 1318258080000.0

        let sample = fromString("Jun 23 1990 11:45:23")
        assert "year" <| (year <$> sample) == Ok 1990
        assert "month" <| (month <$> sample) == Ok June
        assert "day" <| (day <$> sample) == Ok 23
        (dayOfWeek <$> sample) ==> Ok Saturday
        (hour <$> sample) ==> Ok 11
        assert "minute" <| (minute <$> sample) == Ok 45
        assert "second" <| (second <$> sample) == Ok 23
        assert "millisecond" <| (millisecond <$> sample) == Ok 0
