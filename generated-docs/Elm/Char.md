## Module Elm.Char

> Functions for working with characters. Character literals are enclosed in
> `'a'` pair of single quotes.

Implemented using Purescript's [`Data.Char`](https://pursuit.purescript.org/packages/purescript-strings) module.

#### `KeyCode`

``` purescript
type KeyCode = Int
```

> Keyboard keys can be represented as integers. These are called *key codes*.
> You can use [`toCode`](#toCode) and [`fromCode`](#fromCode) to convert between
> key codes and characters.

#### `toCode`

``` purescript
toCode :: Char -> KeyCode
```

> Convert to key code.

#### `fromCode`

``` purescript
fromCode :: KeyCode -> Char
```

> Convert from key code.

#### `isUpper`

``` purescript
isUpper :: Char -> Bool
```

> True for upper case ASCII letters.

#### `isLower`

``` purescript
isLower :: Char -> Bool
```

> True for lower case ASCII letters.

#### `isDigit`

``` purescript
isDigit :: Char -> Bool
```

> True for ASCII digits `[0-9]`.

#### `isOctDigit`

``` purescript
isOctDigit :: Char -> Bool
```

> True for ASCII octal digits `[0-7]`.

#### `isHexDigit`

``` purescript
isHexDigit :: Char -> Bool
```

> True for ASCII hexadecimal digits `[0-9a-fA-F]`.

#### `toLocaleUpper`

``` purescript
toLocaleUpper :: Char -> Char
```

> Convert to upper case, according to any locale-specific case mappings.

#### `toLocaleLower`

``` purescript
toLocaleLower :: Char -> Char
```

> Convert to lower case, according to any locale-specific case mappings.


### Re-exported from Data.Char:

#### `toUpper`

``` purescript
toUpper :: Char -> Char
```

Converts a character to uppercase.

#### `toLower`

``` purescript
toLower :: Char -> Char
```

Converts a character to lowercase.

