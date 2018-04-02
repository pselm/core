module Test.Elm.Result (tests) where

import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (assert)

import Elm.Result as Result
import Elm.Result (Result(..))
import Elm.Basics ((<|), (==), (+), (%), (++))
import Elm.Maybe (Maybe(..))

import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)

import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.Extend(checkExtend)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Bounded (checkBounded)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.QuickCheck.Laws (A, B, C)
import Type.Proxy (Proxy(..), Proxy2(..))

import Control.Alt (class Alt, alt)
import Control.Extend (class Extend, extend)

import Prelude
    ( class Eq, eq
    , class Bounded, top, bottom
    , class Ord, compare
    , class Bind, bind, pure, discard
    , class Functor, map
    , class Apply, apply
    , class Applicative
    , class Monad
    , class Semigroup, append
    , (<<<), (<$>), ($)
    )


assertEqual :: ∀ e. String -> Result String Int -> Result String Int -> Test e
assertEqual name expected actual =
    assert name <| expected == actual


assertMaybe :: ∀ e. String -> Maybe Int -> Maybe Int -> Test e
assertMaybe name expected actual =
    assert name <| expected == actual


isEven :: Int -> Result String Int
isEven n =
    if n % 2 == 0
        then Ok n
        else Err "number is odd"


add3 :: Int -> Int -> Int -> Int
add3 a b c =
    a + b + c


add4 :: Int -> Int -> Int -> Int -> Int
add4 a b c d =
    a + b + c + d


add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5 a b c d e =
    a + b + c + d + e


tests :: ∀ e. TestSuite (random :: RANDOM, exception :: EXCEPTION, console :: CONSOLE | e)
tests = suite "Result" do
    test "map" do
        assertEqual "map Ok"  (Ok 3)        (Result.map ((+) 1) (Ok 2))
        assertEqual "map Err" (Err "error") (Result.map ((+) 1) (Err "error"))

    test "mapN" do
        assertEqual "map2 Ok"  (Ok 3)    (Result.map2 (+) (Ok 1) (Ok 2))
        assertEqual "map2 Err" (Err "x") (Result.map2 (+) (Ok 1) (Err "x"))

        assertEqual "map3 Ok"  (Ok 6)    (Result.map3 add3 (Ok 1) (Ok 2) (Ok 3))
        assertEqual "map3 Err" (Err "x") (Result.map3 add3 (Ok 1) (Ok 2) (Err "x"))

        assertEqual "map4 Ok"  (Ok 10)   (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Ok 4))
        assertEqual "map4 Err" (Err "x") (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Err "x"))

        assertEqual "map5 Ok"  (Ok 15)   (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Ok 5))
        assertEqual "map5 Err" (Err "x") (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Err "x"))

    test "andThen" do
        assertEqual "andThen Ok" (Ok 42) ((Ok 42) `Result.andThen` isEven)

        assertEqual "andThen first Err"
            (Err "could not convert string '4.2' to an Int")
            ((Err "could not convert string '4.2' to an Int") `Result.andThen` isEven)

        assertEqual "andThen second Err"
            (Err "number is odd")
            ((Ok 41) `Result.andThen` isEven)

    test "withDefault" do
        assert "Ok" <|  Result.withDefault 0 (Ok 123) == 123
        assert "Err" <| Result.withDefault 0 (Err "abc") == 0

    test "formatError" do
        assertEqual "Ok"  (Result.formatError (\e -> "Error: " ++ e) (Ok 123)) (Ok 123)
        assertEqual "Err" (Result.formatError (\e -> "Error: " ++ e) (Err "abc")) (Err "Error: abc")

    test "toMaybe" do
        assertMaybe "Ok"  (Result.toMaybe (Ok 27)) (Just 27)
        assertMaybe "Err" (Result.toMaybe (Err "bob")) Nothing

    test "fromMaybe" do
        assertEqual "Ok"  (Result.fromMaybe "bad" (Just 27)) (Ok 27)
        assertEqual "Err" (Result.fromMaybe "bad" (Nothing)) (Err "bad")

    test "laws\n" $
        liftEff do
            checkFunctor prx2Result
            checkApply prx2Result
            checkApplicative prx2Result
            checkAlt prx2Result
            checkBind prx2Result
            checkMonad prx2Result
            checkExtend prx2Result
            checkEq prxResult
            checkOrd prxResult
            checkBounded prxResult
            checkSemigroup prxResult


prxResult :: Proxy (Testable A B)
prxResult = Proxy

prx2Result :: Proxy2 (Testable C)
prx2Result = Proxy2


-- A newtype so that we can define an Arbitrary instance while
-- keeping Quickcheck in devDependencies rather than dependencies.
newtype Testable a b = Testable (Result a b)


instance arbitraryTestable :: (Arbitrary a, Arbitrary b) => Arbitrary (Testable a b) where
    arbitrary = do
        b <- arbitrary
        r <-
            if b
                then Err <$> arbitrary
                else Ok <$> arbitrary

        pure $ Testable r


instance coarbitraryTestable :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Testable a b) where
    coarbitrary (Testable (Err a)) = coarbitrary a
    coarbitrary (Testable (Ok b)) = coarbitrary b


-- Then, of course, we have to define all the instances
-- we're actually testing. Note that this is pretty generic ...
-- we could copy this elsewhere.
instance eqTestable :: (Eq a, Eq b) => Eq (Testable a b) where
    eq (Testable a) (Testable b) = eq a b

instance boundedTestable :: (Bounded a, Bounded b) => Bounded (Testable a b) where
    top = Testable top
    bottom = Testable bottom

instance ordTestable :: (Ord a, Ord b) => Ord (Testable a b) where
    compare (Testable a) (Testable b) = compare a b

instance functorTestable :: Functor (Testable a) where
    map func (Testable a) = Testable $ map func a

instance applyTestable :: Apply (Testable a) where
    apply (Testable a) (Testable b) = Testable $ apply a b

instance applicativeTestable :: Applicative (Testable a) where
    pure = Testable <<< pure

instance altTestable :: Alt (Testable a) where
    alt (Testable a) (Testable b) = Testable $ alt a b

instance bindTestable :: Bind (Testable a) where
    bind (Testable r) func =
        Testable $
            bind r \a ->
                case func a of
                    Testable b -> b

instance monadTestable :: Monad (Testable a)

instance extendTestable :: Extend (Testable a) where
    extend func (Testable a) = Testable $ extend (func <<< Testable) a

instance semigroupTestable :: (Semigroup b) => Semigroup (Testable a b) where
    append (Testable a) (Testable b) = Testable $ append a b
