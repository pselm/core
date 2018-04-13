
module Elm.Platform.Sub
  ( Sub
  , map
  , batch
  , none
  ) where


import Data.List (List(..))
import Partial (crash)


-- | A subscription is a way of telling Elm, “Hey, let me know if anything
-- | interesting happens over there!” So if you want to listen for messages on a web
-- | socket, you would tell Elm to create a subscription. If you want to get clock
-- | ticks, you would tell Elm to subscribe to that. The cool thing here is that
-- | this means *Elm* manages all the details of subscriptions instead of *you*.
-- | So if a web socket goes down, *you* do not need to manually reconnect with an
-- | exponential backoff strategy, *Elm* does this all for you behind the scenes!
-- |
-- | Every `Sub` specifies (1) which effects you need access to and (2) the type of
-- | messages that will come back into your application.
-- |
-- | **Note:** Do not worry if this seems confusing at first! As with every Elm user
-- | ever, subscriptions will make more sense as you work through [the Elm Architecture
-- | Tutorial](http://guide.elm-lang.org/architecture/index.html) and see how they fit
-- | into a real application!
data Sub msg =
    Sub


map :: ∀ a msg. Partial => (a -> msg) -> Sub a -> Sub msg
map func sub =
    crash


batch :: ∀ msg. Partial => List (Sub msg) -> Sub msg
batch subs =
    crash


none :: ∀ msg. Partial => Sub msg
none =
    batch Nil


