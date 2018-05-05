
-- | > Library for [bitwise operations](http://en.wikipedia.org/wiki/Bitwise_operation)
-- |
-- | Implemented using Purescript's [`Data.Int.Bits`](https://pursuit.purescript.org/packages/purescript-integers) module.

module Elm.Bitwise
    ( module Virtual
    , and, or, xor
    , shiftLeft, shiftRight, shiftRightLogical
    , shiftLeftBy, shiftRightBy, shiftRightZfBy
    ) where


-- For re-export

import Data.Int.Bits (complement) as Virtual

-- Internal

import Data.Int.Bits ((.&.), (.|.), (.^.), shl, shr, zshr)
import Prelude (flip)


-- | > Bitwise AND
-- |
-- | Equivalent to Purescript's `(.&.)`
and :: Int -> Int -> Int
and = (.&.)


-- | > Bitwise OR
-- |
-- | Equivalent to Purescript's `(.|.)`
or :: Int -> Int -> Int
or = (.|.)


-- | > Bitwise XOR
-- |
-- | Equivalent to Purescript's `(.^.)`
xor :: Int -> Int -> Int
xor = (.^.)


-- | > Shift bits to the left by a given offset, filling new bits with zeros.
-- | > This can be used to multiply numbers by powers of two.
-- | >
-- | >     8 `shiftLeft` 1 == 16
-- | >     8 `shiftLeft` 2 == 32
-- |
-- | Equivalent to Purescript's `shl`
-- |
-- | This function was removed in Elm 0.18, in favour of `shiftLeftBy`,
-- | which has its arguments flipped.
shiftLeft :: Int -> Int -> Int
shiftLeft = shl


-- | > Shift bits to the left by a given offset, filling new bits with zeros.
-- | > This can be used to multiply numbers by powers of two.
-- | >
-- | >     shiftLeftBy 1 5 == 10
-- | >     shiftLeftBy 5 1 == 32
-- |
-- | Equivalent to Purescript's `shl`, but with the arguemnts flipped.
-- |
-- | This function was introduced in Elm 0.18.
shiftLeftBy :: Int -> Int -> Int
shiftLeftBy = flip shl


-- | > Shift bits to the right by a given offset, filling new bits with
-- | > whatever is the topmost bit. This can be used to divide numbers by powers of two.
-- | >
-- | >      32 `shiftRight` 1 == 16
-- | >      32 `shiftRight` 2 == 8
-- | >     -32 `shiftRight` 1 == -16
-- | >
-- | > This is called an
-- | > [arithmetic right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift),
-- | > often written (>>), and sometimes called a sign-propagating
-- | > right shift because it fills empty spots with copies of the highest bit.
-- |
-- | Equivalent to Purescript's `shr`
-- |
-- | This function was removed in Elm 0.18, in favour of `shiftRightBy`,
-- | which has its arguments flipped.
shiftRight :: Int -> Int -> Int
shiftRight = shr


-- | > Shift bits to the right by a given offset, filling new bits with
-- | > whatever is the topmost bit. This can be used to divide numbers by powers of two.
-- | >
-- | >     shiftRightBy 1  32 == 16
-- | >     shiftRightBy 2  32 == 8
-- | >     shiftRightBy 1 -32 == -16
-- | >
-- | > This is called an
-- | > [arithmetic right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift),
-- | > often written (>>), and sometimes called a sign-propagating
-- | > right shift because it fills empty spots with copies of the highest bit.
-- |
-- | Equivalent to Purescript's `shr`, but with the arguments flipped.
-- |
-- | This function was introduced in Elm 0.18.
shiftRightBy :: Int -> Int -> Int
shiftRightBy = flip shr


-- | > Shift bits to the right by a given offset, filling new bits with
-- | > zeros.
-- | >
-- | >      32 `shiftRightLogical` 1 == 16
-- | >      32 `shiftRightLogical` 2 == 8
-- | >     -32 `shiftRightLogical` 1 == 2147483632
-- | >
-- | > This is called an
-- | > [logical right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift),
-- | > often written (>>>), and sometimes called a zero-fill right shift because
-- | > it fills empty spots with zeros.
-- |
-- | Equivalent to Purescript's `zshr`.
-- |
-- | This function was removed in Elm 0.18, in favour of `shiftRightZfBy`,
-- | which has its arguments flipped.
shiftRightLogical :: Int -> Int -> Int
shiftRightLogical = zshr


-- | >  Shift bits to the right by a given offset, filling new bits with
-- | >  zeros.
-- | >
-- | >      shiftRightZfBy 1  32 == 16
-- | >      shiftRightZfBy 2  32 == 8
-- | >      shiftRightZfBy 1 -32 == 2147483632
-- | >
-- | >  This is called an
-- | >  [logical right shift](http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift),
-- | >  often written (>>>), and sometimes called a zero-fill right shift because
-- | >  it fills empty spots with zeros.
-- |
-- | Equivalent to Purescript's `zshr`, but with its arguments flipped.
-- |
-- | This function was added in Elm 0.18.
shiftRightZfBy :: Int -> Int -> Int
shiftRightZfBy = flip zshr
