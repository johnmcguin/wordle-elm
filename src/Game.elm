module Game exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra as LE



-- \u{23ce} - enter key
-- \u{232b} - delete key
-- Model


type LetterState
    = Blank
    | Pending
    | Correct
    | Present
    | Incorrect


type alias Letter =
    ( Char, LetterState )


type alias Model =
    { keyboardLetters : List (List Char)
    , currentGuess : List Char
    , currentRow : Int
    , boardLetters : List (List Letter)
    }


init : Model
init =
    { keyboardLetters =
        [ [ 'q'
          , 'w'
          , 'e'
          , 'r'
          , 't'
          , 'y'
          , 'u'
          , 'i'
          , 'o'
          , 'p'
          ]
        , [ '0'
          , 'a'
          , 's'
          , 'd'
          , 'f'
          , 'g'
          , 'h'
          , 'j'
          , 'k'
          , 'l'
          , '0'
          ]
        , [ '⏎'
          , 'z'
          , 'x'
          , 'c'
          , 'v'
          , 'b'
          , 'n'
          , 'm'
          , '⌫'
          ]
        ]
    , currentGuess = []
    , currentRow = 0
    , boardLetters = List.repeat 6 <| List.repeat 5 ( ' ', Blank )
    }



-- Update


type Msg
    = KeyPress Char
    | SubmitGuess
    | Delete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            let
                guessWritable =
                    List.length model.currentGuess < 5

                -- get the current row we are guessing for
                row =
                    model.boardLetters
                        |> LE.getAt model.currentRow
                        |> Maybe.withDefault []

                -- find the first blank cell (that is the target to fill for the next input value
                -- find returns (Index, (Char, LetterState))
                cell =
                    row
                        |> find (\( char, _ ) -> char == ' ')

                updatedRow =
                    case cell of
                        Just c ->
                            row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( key, Pending ))

                        Nothing ->
                            row

                _ =
                    Debug.log "cell" cell
            in
            if guessWritable then
                ( { model
                    | currentGuess = model.currentGuess ++ [ key ]
                    , boardLetters =
                        model.boardLetters
                            |> List.indexedMap
                                (\idx letterRow ->
                                    if idx == model.currentRow then
                                        updatedRow

                                    else
                                        letterRow
                                )
                  }
                , Cmd.none
                )

            else
                -- shake?
                ( model, Cmd.none )

        SubmitGuess ->
            ( model, Cmd.none )

        Delete ->
            let
                currentGuess =
                    model.currentGuess
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
            ( { model | currentGuess = currentGuess }, Cmd.none )



-- Views


view : Model -> Html Msg
view model =
    div [ HA.class "app" ]
        [ div [ HA.class "board_wrapper" ]
            [ div [ HA.class "board" ]
                [ div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                , div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                , div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                , div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                , div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                , div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                ]
            ]
        , div [ HA.class "keyboard" ] (List.map renderRow model.keyboardLetters)
        ]


renderRow : List Char -> Html Msg
renderRow letter_rows =
    let
        key_rows =
            List.map renderBtn letter_rows
    in
    div [ HA.class "keyboard_row" ] key_rows


renderBtn : Char -> Html Msg
renderBtn letter =
    case letter of
        '⌫' ->
            button [ HA.class "key is_delete", onClick Delete ] [ String.fromChar letter |> text ]

        '⏎' ->
            button [ HA.class "key is_enter", onClick SubmitGuess ] [ String.fromChar letter |> text ]

        '0' ->
            div [ HA.class "key is_spacer" ] []

        char ->
            button [ HA.class (keyClass char), onClick <| KeyPress letter ] [ String.fromChar letter |> text ]


keyClass : Char -> String
keyClass letter =
    "key " ++ "is_" ++ String.fromChar letter



-- Utils


getBoardLetterRowAtIndex : Int -> List (List Letter) -> List Letter
getBoardLetterRowAtIndex index list =
    list
        |> LE.getAt index
        |> Maybe.withDefault []


find : (a -> Bool) -> List a -> Maybe ( Int, a )
find pred list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just ( 0, x )

            else
                Maybe.map (\( index, item ) -> ( index + 1, item )) <| find pred xs
