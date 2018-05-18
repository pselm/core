## Module Elm.List

> A library for manipulating lists of values. Every value in a
> list must have the same type.

Implemented in terms of Purescript's `Data.List`, so you can also
use functions from `Data.List` on a `List`.

Note that Purescript uses `:` for `cons` and and `::` to indicate the type
of a thing, which is exactly the opposite of Elm.

Purescript's compiler doesn't have a literal
syntax for lists, so instead of this:

    [1, 2, 3]

... you need to do something like this:

    (1 : 2 : 3 : Nil)

There is a literal syntax for `Array`, e.g. `[1, 2, 3]`. However, the `Array` type in Purescript
is actually a Javascript array, which is typically not what you want (unless you're getting
one from elsewhere anyway). And, it's not what `Elm.Array` is.

What you can do, though, to get a list is something like this:

    Data.List.toList [1, 2, 3]

... which is a nice little trick when porting code, as all you have to add is the `Data.List.toList`.

I have also made some of the Elm APIs accept either `Elm.List`, `Array` or `Elm.Array` by using
type-classes in the function signatures.

#### `isEmpty`

``` purescript
isEmpty :: forall a. List a -> Bool
```

> Determine if a list is empty.
>
>     isEmpty Nil == True

Equivalent to Purescript's `null`.

#### `member`

``` purescript
member :: forall a. Eq a => a -> List a -> Bool
```

> Figure out whether a list contains a value.
>
>     member 9 (1 : 2 : 3 : 4 : Nil) == False
>     member 4 (1 : 2 : 3 : 4 : Nil) == True

#### `map2`

``` purescript
map2 :: forall a b result. (a -> b -> result) -> List a -> List b -> List result
```

> Combine two lists, combining them with the given function.
> If one list is longer, the extra elements are dropped.
>
>     map2 (+) (1 : 2 : 3 : Nil) (1 : 2 : 3 : 4 : Nil) == (2 : 4 : 6 : Nil)
>
>     map2 Tuple (1 : 2 : 3 : Nil) ('a' : 'b' : Nil) == (Tuple 1 'a' : Tuple 2 'b' : Nil)
>
>     pairs :: List a -> List b -> List (Tuple a b)
>     pairs lefts rights =
>         map2 Tuple lefts rights

Equivalent to Purescript's `zipWith`.

#### `map3`

``` purescript
map3 :: forall a b c result. (a -> b -> c -> result) -> List a -> List b -> List c -> List result
```

#### `map4`

``` purescript
map4 :: forall a b c d result. (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
```

#### `map5`

``` purescript
map5 :: forall a b c d e result. (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
```

#### `intersperse`

``` purescript
intersperse :: forall a. a -> List a -> List a
```

> Places the given value between all members of the given list.
>
>     intersperse "on" ("turtles" : "turtles" : "turtles" : Nil) == ("turtles" : "on" : "turtles" : "on" : "turtles" : Nil)

Similar to Purescript's `intercalate`, but `intercalate` also immediately
combines the result as a Monoid.

#### `scanl`

``` purescript
scanl :: forall a b. (a -> b -> b) -> b -> List a -> List b
```

> Reduce a list from the left, building up all of the intermediate results into a list.
>
>     scanl (+) 0 (1 : 2 : 3 : 4 : Nil) == (0 : 1 : 3 : 6 : 10 : Nil)

This is like Purescript's `scanl`, except that the function you provide in the first
parameter is flipped, and the second parameter is included in the resulting list.

#### `filterMap`

``` purescript
filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
```

> Apply a function that may succeed to all values in the list, but only keep
> the successes.
>
>     filterMap isTeen [3, 15, 12, 18, 24] == [15, 18]
>
>     isTeen :: Int -> Maybe Int
>     isTeen n =
>         if 13 <= n && n <= 19
>             then Just n
>             else Nothing

Equivalent to Purescript's `mapMaybe`.

#### `partition`

``` purescript
partition :: forall a. (a -> Bool) -> List a -> Tuple (List a) (List a)
```

> Partition a list based on a predicate. The first list contains all values
> that satisfy the predicate, and the second list contains all the value that do
> not.
>
>     partition (\x -> x < 3) (0..5) == Tuple (0 : 1 : 2 : Nil) (3 : 4 : 5 : Nil)
>     partition isEven        (0..5) == Tuple (0 : 2 : 4 : Nil) (1 : 3 : 5 : Nil)

#### `unzip`

``` purescript
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
```

> Decompose a list of tuples into a tuple of lists.
>
>     unzip (Tuple 0 True : Tuple 17 False :  Tuple 1337 True : Nil) == Tuple (0 : 17 : 1337 : Nil) (True : False : True : Nil)

#### `repeat`

``` purescript
repeat :: forall a. Int -> a -> List a
```

> Create a list with *n* copies of a value:
>
>     repeat 3 0 == (0 : 0 : 0 : Nil)
>
> Equivalent to Purescript's `replicate`.

#### `sortBy`

``` purescript
sortBy :: forall a comparable. Ord comparable => (a -> comparable) -> List a -> List a
```

> Sort values by a derived property.
>
>     alice = { name: "Alice", height: 1.62 }
>     bob   = { name: "Bob"  , height: 1.85 }
>     chuck = { name: "Chuck", height: 1.76 }
>
>     sortBy _.name   (chuck : alice : bob : Nil) == (alice : bob : chuck : Nil)
>     sortBy _.height (chuck : alice : bob : Nil) == (alice : chuck : bob : Nil)
>
>     sortBy String.length ("mouse" : "cat" : Nil) == ("cat" : "mouse" : Nil)

Note that this is not the same as Purescript's `sortBy`, which is
like Elm's `sortWith`.

#### `sortWith`

``` purescript
sortWith :: forall a. (a -> a -> Order) -> List a -> List a
```

> Sort values with a custom comparison function.
>
>     sortWith flippedComparison (1..5) == (5 : 4 : 3 : 2 : 1 : Nil)
>
>     flippedComparison a b =
>         case compare a b of
>           LT -> GT
>           EQ -> EQ
>           GT -> LT
>
> This is also the most general sort function, allowing you
> to define any other: `sort == sortWith compare`

Equivalent to Purescript's `sortBy`.

#### `range`

``` purescript
range :: Int -> Int -> List Int
```

> Create a list of numbers, every element increasing by one.
> You give the lowest and highest number that should be in the list.
>
>     range 3 6 == [3, 4, 5, 6]
>     range 3 3 == [3]
>     range 6 3 == []

Like Purescript's `range`, except that the Elm version produces an empty list
if the first parameter is greater than the second.

#### `(..)`

``` purescript
infixl 4 range as ..
```

This operator was removed in Elm 0.18.


### Re-exported from Data.Foldable:

#### `foldr`

``` purescript
foldr :: forall a b f. Foldable f => (a -> b -> b) -> b -> f a -> b
```

### Re-exported from Data.List:

#### `List`

``` purescript
data List a
  = Nil
  | Cons a (List a)
```

##### Instances
``` purescript
(Show a) => Show (List a)
(Eq a) => Eq (List a)
Eq1 List
(Ord a) => Ord (List a)
Ord1 List
Semigroup (List a)
Monoid (List a)
Functor List
FunctorWithIndex Int List
Foldable List
FoldableWithIndex Int List
Unfoldable List
Traversable List
TraversableWithIndex Int List
Apply List
Applicative List
Bind List
Monad List
Alt List
Plus List
Alternative List
MonadZero List
MonadPlus List
Extend List
```

#### `take`

``` purescript
take :: forall a. Int -> List a -> List a
```

Take the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to take.

#### `tail`

``` purescript
tail :: forall a. List a -> Maybe (List a)
```

Get all but the first element of a list, or `Nothing` if the list is empty.

Running time: `O(1)`

#### `sort`

``` purescript
sort :: forall a. Ord a => List a -> List a
```

Sort the elements of an list in increasing order.

#### `singleton`

``` purescript
singleton :: forall a. a -> List a
```

Create a list with a single element.

Running time: `O(1)`

#### `reverse`

``` purescript
reverse :: List ~> List
```

Reverse a list.

Running time: `O(n)`

#### `length`

``` purescript
length :: forall a. List a -> Int
```

Get the length of a list

Running time: `O(n)`

#### `head`

``` purescript
head :: List ~> Maybe
```

Get the first element in a list, or `Nothing` if the list is empty.

Running time: `O(1)`.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> List a -> List a
```

Filter a list, keeping the elements which satisfy a predicate function.

Running time: `O(n)`

#### `drop`

``` purescript
drop :: forall a. Int -> List a -> List a
```

Drop the specified number of elements from the front of a list.

Running time: `O(n)` where `n` is the number of elements to drop.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> List b) -> List a -> List b
```

Apply a function to each element in a list, and flatten the results
into a single, new list.

Running time: `O(n)`, where `n` is the total number of elements.

#### `concat`

``` purescript
concat :: forall a. List (List a) -> List a
```

Flatten a list of lists.

Running time: `O(n)`, where `n` is the total number of elements.

#### `any`

``` purescript
any :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
```

`any f` is the same as `or <<< map f`; map a function over the structure,
and then get the disjunction of the results.

#### `all`

``` purescript
all :: forall a b f. Foldable f => HeytingAlgebra b => (a -> b) -> f a -> b
```

`all f` is the same as `and <<< map f`; map a function over the structure,
and then get the conjunction of the results.

#### `(:)`

``` purescript
infixr 6 Cons as :
```

### Re-exported from Data.Traversable:

#### `sum`

``` purescript
sum :: forall a f. Foldable f => Semiring a => f a -> a
```

Find the sum of the numeric values in a data structure.

#### `product`

``` purescript
product :: forall a f. Foldable f => Semiring a => f a -> a
```

Find the product of the numeric values in a data structure.

#### `minimum`

``` purescript
minimum :: forall a f. Ord a => Foldable f => f a -> Maybe a
```

Find the smallest element of a structure, according to its `Ord` instance.

#### `maximum`

``` purescript
maximum :: forall a f. Ord a => Foldable f => f a -> Maybe a
```

Find the largest element of a structure, according to its `Ord` instance.

### Re-exported from Elm.Foldable:

#### `foldl`

``` purescript
foldl :: forall a b f. Foldable f => (a -> b -> b) -> b -> f a -> b
```

Reduce a container from the left.

Equivalent to Purescript's `foldl`, but the function you supply is flipped.

### Re-exported from Elm.FunctorWithIndex:

#### `indexedMap`

``` purescript
indexedMap :: forall i f a b. FunctorWithIndex i f => (i -> a -> b) -> f a -> f b
```

Map over a container with an index.

Equivalent to Purescript's `mapWithIndex`

### Re-exported from Prelude:

#### `append`

``` purescript
append :: forall a. Semigroup a => a -> a -> a
```

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

