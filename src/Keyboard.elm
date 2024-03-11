module Keyboard exposing (Model, Msg(..), init, update)


type alias Model =
    Int


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )
