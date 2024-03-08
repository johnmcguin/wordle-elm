module Main exposing (main)

import Browser
import Html exposing (Html, div, text)

type Msg = Increment | Decrement
type alias Model = Int

main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
  div [] [text "hello wordle"]
