module Examples.Simple where


import Elm.Default

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.IO (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Elm.Platform (exportProgram, program)
import Elm.Platform.Cmd as Cmd
import Elm.Task (Task)
import Elm.Task (perform) as Task
import Elm.Time (Time)
import Elm.Time (every) as Elm.Time
import Prelude (class Show, Unit, ($), (<<<))


type Model =
    { counter :: Int
    , currentTime :: Time
    }


emptyModel :: Model
emptyModel =
    { counter : 0
    , currentTime : 0.0
    }


data Msg
    = HandleTime Time
    | NoOp


init :: Tuple Model (Cmd Msg)
init =
    Tuple emptyModel Cmd.none


update :: Msg -> Model -> Tuple Model (Cmd Msg)
update msg model =
    case msg of
        HandleTime time ->
             model { currentTime = time }
             /\ Task.perform (always NoOp) (log time)

        NoOp ->
            model /\ Cmd.none


subscriptions :: Model -> Sub Msg
subscriptions model =
    Elm.Time.every 1000.0 HandleTime


log :: ∀ x a. Show a => a -> Task x Unit
log = liftAff <<< logShow


main :: Eff (infinity :: INFINITY) Unit
main =
    runIOSync $ exportProgram "Examples.Simple" $
        program {init, update, subscriptions}
