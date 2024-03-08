module Main exposing (main)

import Browser
import Html exposing (Html, div, text)

type Msg = Increment | Decrement
type alias Model = Int

main : Program () Int Msg
main =
    Browser.element { init = init , update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : () -> (Model, Cmd Msg)
init _ = 
  (0, Cmd.none)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Increment ->
            (model + 1, Cmd.none)

        Decrement ->
            (model - 1, Cmd.none)


view : Model -> Html Msg
view model =
  div [] [text "hello wordle"]
