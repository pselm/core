## Module Elm.Date

> Library for working with dates.

See notes below about the types used for `Date`, `Day` and `Month`.

#### `Date`

``` purescript
type Date = JSDate
```

Elm's `Date` type is implemented here in terms of Purescript's `JSDate`,
which, in both cases, is a Javascript Date object.

#### `fromString`

``` purescript
fromString :: String -> Result String Date
```

Attempt to read a date from a string.

#### `Time`

``` purescript
type Time = Number
```

An alias for units of time, representing milliseconds.

#### `toTime`

``` purescript
toTime :: Date -> Time
```

> Convert a `Date` to a time in milliseconds.
>
> A time is the number of milliseconds since
> [the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).

#### `fromTime`

``` purescript
fromTime :: Time -> Date
```

> Convert a time in milliseconds into a `Date`.
>
> A time is the number of milliseconds since
> [the Unix epoch](http://en.wikipedia.org/wiki/Unix_time).

#### `Day`

``` purescript
type Day = Weekday
```

Elm's `Day` type is equivalent to `Weekday` in Purescript.

However, note that in Purescript the constructors spell out the name of the day
... i.e. `Saturday` instead of `Sat`.

#### `year`

``` purescript
year :: Date -> Int
```

> Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
> this returns the integer `1990`.

As in the Elm implementation, this implicitly uses the current locale.

#### `month`

``` purescript
month :: Date -> Month
```

> Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
> this returns the month `June` as defined below.

Note that in Purescript, the constructors for `Month` are fully spelled out,
so it is 'June` instead of `Jun`.

As in the Elm implementation, this implicitly uses the current locale.

#### `day`

``` purescript
day :: Date -> Int
```

> Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
> this returns the integer `23`.

As in the Elm implementation, this implicitly uses the current locale.

#### `dayOfWeek`

``` purescript
dayOfWeek :: Date -> Day
```

> Extract the day of the week for a given date. Given the date 23 June
> 1990 at 11:45AM this returns the day `Saturday` as defined below.

Note that in Purescript, the days of the week are fully spelled out,
so it is `Thursday` instead of `Thu`.

As in the Elm implementation, this implicitly uses the current locale.

#### `hour`

``` purescript
hour :: Date -> Int
```

> Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
> this returns the integer `11`.

As in the Elm implementation, this implicitly uses the current locale.

#### `minute`

``` purescript
minute :: Date -> Int
```

>  Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
>  this returns the integer `45`.

As in the Elm implementation, this implicitly uses the current locale.

#### `second`

``` purescript
second :: Date -> Int
```

> Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
> this returns the integer `0`.

As in the Elm implementation, this implicitly uses the current locale.

#### `millisecond`

``` purescript
millisecond :: Date -> Int
```

> Extract the millisecond of a given date. Given the date 23 June 1990 at 11:45:30.123AM
> this returns the integer `123`.

As in the Elm implementation, this implicitly uses the current locale.

#### `now`

``` purescript
now :: forall x. Task x Date
```

> Get the `Date` at the moment when this task is run.

Added in Elm 0.17 / version 4.0.0 of elm-lang/core


### Re-exported from Data.Date:

#### `Month`

``` purescript
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
```

A month component for a date in the Gregorian calendar.

##### Instances
``` purescript
Eq Month
Ord Month
Generic Month
Bounded Month
Enum Month
BoundedEnum Month
Show Month
```

