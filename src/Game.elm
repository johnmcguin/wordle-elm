module Game exposing (Model, Msg(..), getRandomWord, init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra as LE
import Random
import Words exposing (getRandom, wordLength)



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
    , solution : String
    , board : List (List Letter)
    }


init : String -> Model
init solution =
    -- many state bits will need to be dynamic in the case of a refresh
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
    , board = List.repeat 6 <| List.repeat 5 ( ' ', Blank )
    , solution = solution
    }



-- Update


type Msg
    = KeyPress Char
    | SubmitGuess
    | Delete
    | GotRandomIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            let
                guessWritable =
                    List.length model.currentGuess < 5

                -- get the current row we are guessing for
                row =
                    model.board
                        |> LE.getAt model.currentRow
                        |> Maybe.withDefault []

                -- find the first blank cell (that is the target to fill for the next input value
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
            in
            if guessWritable then
                ( { model
                    | currentGuess = model.currentGuess ++ [ key ]
                    , board =
                        model.board
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
                row =
                    model.board
                        |> LE.getAt model.currentRow
                        |> Maybe.withDefault []

                backwards_row =
                    row |> List.reverse

                cell =
                    backwards_row
                        |> find (\( char, _ ) -> char /= ' ')

                updatedRow =
                    case cell of
                        Just c ->
                            backwards_row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( ' ', Blank ))
                                |> List.reverse

                        Nothing ->
                            row

                currentGuess =
                    model.currentGuess
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
            ( { model
                | currentGuess = currentGuess
                , board =
                    model.board
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

        GotRandomIndex idx ->
            let
                _ =
                    Debug.log "idx" idx
            in
            ( { model | solution = getRandom idx |> Maybe.withDefault "" }
            , Cmd.none
            )



-- Views


view : Model -> Html Msg
view model =
    div [ HA.class "app" ]
        [ div [ HA.class "board_wrapper" ]
            [ div [ HA.class "board" ] (List.map renderBoardRow model.board) ]
        , div [ HA.class "keyboard" ] (List.map renderRow model.keyboardLetters)
        ]


renderBoardRow : List Letter -> Html Msg
renderBoardRow boardRow =
    div [ HA.class "board_row" ] (List.map renderBoardRowItems boardRow)


renderBoardRowItems : Letter -> Html Msg
renderBoardRowItems letter =
    div [ HA.class "tile" ] [ text (String.fromChar <| Tuple.first letter) ]


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


getRandomWord : Cmd Msg
getRandomWord =
    let
        generator : Random.Generator Int
        generator =
            Random.int 0 wordLength
    in
    Random.generate GotRandomIndex generator
