module Game exposing (Model, Msg(..), getRandomWord, init, update, view)

import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra as LE
import Process
import Random
import Task
import Words exposing (getRandom, wordIsValid, wordLength)



-- Model


type LetterState
    = Blank
    | Pending
    | Correct
    | Present
    | Incorrect


type alias Letter =
    ( Char, LetterState )


type alias KeyboardDictionary =
    Dict Char LetterState


type alias KeyboardRow =
    List Letter


type EndResult
    = WonIn Int
    | Lost


type alias GameResult =
    { result : EndResult
    , board : List KeyboardRow
    , keyboardLetters : List (List Char)
    , keyboardDictionary : KeyboardDictionary
    , solution : String
    , message : Maybe String
    }


type alias GameInProgress =
    { keyboardLetters : List (List Char)
    , keyboardDictionary : KeyboardDictionary
    , currentGuess : List Char
    , currentRow : Int
    , solution : String
    , shakeRow : Maybe Int
    , board : List KeyboardRow
    , message : Maybe String
    }


type Model
    = InProgress GameInProgress
    | GameEnd GameResult


init : String -> Model
init solution =
    InProgress
        { keyboardLetters = initKeyboardDictLetters
        , keyboardDictionary = initKeyboardDict
        , currentGuess = []
        , currentRow = 0
        , board = initBoard
        , solution = solution
        , shakeRow = Nothing
        , message = Nothing
        }



-- Update


type Msg
    = KeyPress Char
    | SubmitGuess
    | Delete
    | GotRandomIndex Int
    | ClearAnimation
    | ClearAlert
    | ShowEndGameMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( InProgress gameState, KeyPress key ) ->
            let
                guessWritable : Bool
                guessWritable =
                    List.length gameState.currentGuess < 5

                getRow : Int -> List Letter
                getRow idx =
                    gameState.board
                        |> LE.getAt idx
                        |> Maybe.withDefault []

                findCell : List ( Char, b ) -> Maybe ( Int, ( Char, b ) )
                findCell =
                    find (\( char, _ ) -> char == ' ')

                updatedRow : List Letter -> List ( Char, LetterState )
                updatedRow row =
                    case findCell row of
                        Just c ->
                            row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( key, Pending ))

                        Nothing ->
                            row
            in
            if guessWritable then
                ( InProgress
                    { gameState
                        | currentGuess = gameState.currentGuess ++ [ key ]
                        , board =
                            gameState.board
                                |> List.indexedMap
                                    (\idx letterRow ->
                                        if idx == gameState.currentRow then
                                            updatedRow (getRow gameState.currentRow)

                                        else
                                            letterRow
                                    )
                    }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ( InProgress gameState, SubmitGuess ) ->
            let
                isPending : Letter -> Bool
                isPending letter =
                    case letter of
                        ( _, Pending ) ->
                            True

                        _ ->
                            False

                isSubmittable : Bool
                isSubmittable =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map (List.all isPending)
                        |> Maybe.withDefault False

                guess : String
                guess =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map (List.map Tuple.first)
                        |> Maybe.map String.fromList
                        |> Maybe.withDefault ""

                gameLost : String -> Bool
                gameLost solution =
                    isSubmittable && guess /= solution && gameState.currentRow == 5 && wordIsValid guess

                progressNextRow : Int -> Bool
                progressNextRow currentRow =
                    isSubmittable && guess /= gameState.solution && currentRow < 6 && wordIsValid guess

                shouldApplyGuess : Bool
                shouldApplyGuess =
                    isSubmittable && gameState.currentRow < 6 && wordIsValid guess

                isUnsupportedWord : String -> Bool
                isUnsupportedWord guessedWord =
                    isSubmittable && guessedWord /= gameState.solution && not (wordIsValid guessedWord)

                getMessage : Bool -> Maybe String
                getMessage isUnsupported =
                    if isUnsupported then
                        Just "Not in word list"

                    else if not isSubmittable then
                        Just "Not enough letters"

                    else
                        Nothing

                board : List KeyboardRow
                board =
                    if shouldApplyGuess then
                        applyGuess gameState

                    else
                        gameState.board

                gameWon : Bool
                gameWon =
                    board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map (List.all (\( _, state ) -> state == Correct))
                        |> Maybe.withDefault False

                newDict : Bool -> KeyboardDictionary
                newDict shouldUpdate =
                    if shouldUpdate then
                        updateKeyboardDict gameState.currentGuess gameState.keyboardDictionary gameState.solution

                    else
                        gameState.keyboardDictionary
            in
            if gameWon then
                ( GameEnd
                    { solution = gameState.solution
                    , board = board
                    , result = WonIn <| gameState.currentRow
                    , message = Nothing
                    , keyboardLetters = gameState.keyboardLetters
                    , keyboardDictionary = newDict shouldApplyGuess
                    }
                , showEndGameMessage
                )

            else if gameLost gameState.solution then
                ( GameEnd
                    { solution = gameState.solution
                    , board = board
                    , result = Lost
                    , message = Nothing
                    , keyboardLetters = gameState.keyboardLetters
                    , keyboardDictionary = newDict shouldApplyGuess
                    }
                , showEndGameMessage
                )

            else
                let
                    message : Maybe String
                    message =
                        getMessage (isUnsupportedWord guess)
                in
                ( InProgress
                    { gameState
                        | currentRow =
                            if progressNextRow gameState.currentRow then
                                gameState.currentRow + 1

                            else
                                gameState.currentRow
                        , board = board
                        , currentGuess =
                            if not (isUnsupportedWord guess) then
                                []

                            else
                                gameState.currentGuess
                        , shakeRow =
                            if isUnsupportedWord guess || not isSubmittable then
                                Just gameState.currentRow

                            else
                                Nothing
                        , message = message
                        , keyboardDictionary = newDict shouldApplyGuess
                    }
                , Cmd.batch [ clearAnimation (isUnsupportedWord guess || not isSubmittable), clearAlert message ]
                )

        ( InProgress gameState, Delete ) ->
            let
                row : List Letter
                row =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.withDefault []

                backwards_row : List Letter
                backwards_row =
                    row |> List.reverse

                cell : Maybe ( Int, ( Char, LetterState ) )
                cell =
                    backwards_row
                        |> find (\( char, _ ) -> char /= ' ')

                updatedRow : List ( Char, LetterState )
                updatedRow =
                    case cell of
                        Just c ->
                            backwards_row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( ' ', Blank ))
                                |> List.reverse

                        Nothing ->
                            row

                currentGuess : List Char
                currentGuess =
                    gameState.currentGuess
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
            ( InProgress
                { gameState
                    | currentGuess = currentGuess
                    , board =
                        gameState.board
                            |> List.indexedMap
                                (\idx letterRow ->
                                    if idx == gameState.currentRow then
                                        updatedRow

                                    else
                                        letterRow
                                )
                }
            , Cmd.none
            )

        ( InProgress gameState, GotRandomIndex idx ) ->
            ( InProgress { gameState | solution = getRandom idx |> Maybe.withDefault "" }
            , Cmd.none
            )

        ( InProgress gameState, ClearAnimation ) ->
            ( InProgress { gameState | shakeRow = Nothing }
            , Cmd.none
            )

        ( InProgress gameState, ClearAlert ) ->
            ( InProgress { gameState | message = Nothing }
            , Cmd.none
            )

        ( GameEnd gameResult, ShowEndGameMessage ) ->
            ( GameEnd
                { gameResult
                    | message =
                        case gameResult.result of
                            WonIn count ->
                                successMessage count

                            Lost ->
                                Just gameResult.solution
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- Views


view : Model -> Html Msg
view model =
    case model of
        InProgress gameState ->
            let
                message : Html Msg
                message =
                    maybeRenderMessage gameState.message

                boardRows : List (Html Msg)
                boardRows =
                    renderBoardRows gameState.board gameState.shakeRow

                keyboardRows : List (Html Msg)
                keyboardRows =
                    renderKeyboardRows gameState.keyboardDictionary gameState.keyboardLetters
            in
            renderGame message boardRows keyboardRows

        GameEnd gameResult ->
            let
                message : Html Msg
                message =
                    maybeRenderMessage gameResult.message

                boardRows : List (Html Msg)
                boardRows =
                    renderBoardRows gameResult.board Nothing

                keyboardRows : List (Html Msg)
                keyboardRows =
                    renderKeyboardRows gameResult.keyboardDictionary gameResult.keyboardLetters
            in
            renderGame message boardRows keyboardRows


renderGame : Html Msg -> List (Html Msg) -> List (Html Msg) -> Html Msg
renderGame msg boardRows keyboardRows =
    div [ HA.class "app" ]
        [ div [ HA.class "board_wrapper" ]
            [ msg
            , div [ HA.class "board" ] boardRows
            ]
        , div [ HA.class "keyboard" ] keyboardRows
        ]


maybeRenderMessage : Maybe String -> Html Msg
maybeRenderMessage maybeMessage =
    case maybeMessage of
        Just messageText ->
            div [ HA.class "message" ] [ text <| String.toUpper messageText ]

        Nothing ->
            text ""


renderBoardRows : List KeyboardRow -> Maybe Int -> List (Html Msg)
renderBoardRows gameBoard maybeShakeRowIdx =
    gameBoard
        |> List.indexedMap (\idx row -> renderBoardRow idx maybeShakeRowIdx row)


renderKeyboardRows : KeyboardDictionary -> List (List Char) -> List (Html Msg)
renderKeyboardRows keyboardDictionary keyboardLetters =
    List.map (renderRow keyboardDictionary) keyboardLetters


renderBoardRow : Int -> Maybe Int -> KeyboardRow -> Html Msg
renderBoardRow idx shakeRowVal boardRow =
    div [ HA.class <| boardRowClass idx shakeRowVal ] (List.indexedMap (\index letter -> renderBoardRowItems index letter) boardRow)


renderBoardRowItems : Int -> Letter -> Html Msg
renderBoardRowItems idx letter =
    let
        charAsString : String
        charAsString =
            letter
                |> Tuple.first
                |> String.fromChar
    in
    div [ HA.class <| revealTileClass letter ]
        [ div
            [ HA.class "front"
            , HA.style "transition-delay" (withDelay (idx * 300))
            ]
            [ text charAsString ]
        , div
            [ HA.class "back"
            , HA.style "transition-delay" (withDelay (idx * 300))
            , HA.style "animation-delay" (withDelay (idx * 100))
            ]
            [ text charAsString ]
        ]


renderRow : KeyboardDictionary -> List Char -> Html Msg
renderRow keyboardDictionary letterRows =
    let
        keyRows : List (Html Msg)
        keyRows =
            List.map (renderBtn keyboardDictionary) letterRows
    in
    div [ HA.class "keyboard_row" ] keyRows


renderBtn : KeyboardDictionary -> Char -> Html Msg
renderBtn keyboardDictionary letter =
    case letter of
        '⌫' ->
            button [ HA.class "key is_delete", onClick Delete ] [ String.fromChar letter |> text ]

        '⏎' ->
            button [ HA.class "key is_enter", onClick SubmitGuess ] [ String.fromChar letter |> text ]

        '0' ->
            div [ HA.class "key is_spacer" ] []

        char ->
            let
                state =
                    Dict.get char keyboardDictionary
            in
            button [ HA.class (keyClass char state), onClick <| KeyPress letter ] [ String.fromChar letter |> text ]



-- Game related functions


letterStateAsString : LetterState -> String
letterStateAsString letterState =
    case letterState of
        Blank ->
            "Blank"

        Pending ->
            "Pending"

        Incorrect ->
            "Incorrect"

        Correct ->
            "Correct"

        Present ->
            "Present"


getRandomWord : Cmd Msg
getRandomWord =
    let
        generator : Random.Generator Int
        generator =
            Random.int 0 wordLength
    in
    Random.generate GotRandomIndex generator


applyGuess : GameInProgress -> List KeyboardRow
applyGuess game =
    game.board
        |> List.indexedMap (markCorrect game.currentRow game.solution)
        |> List.indexedMap (markOtherTiles game.currentRow game.solution)


successMessage : Int -> Maybe String
successMessage solvedIn =
    case solvedIn of
        0 ->
            Just "Genius"

        1 ->
            Just "Magnificent"

        2 ->
            Just "Impressive"

        3 ->
            Just "Splendid"

        4 ->
            Just "Great"

        5 ->
            Just "Phew"

        _ ->
            Nothing


updateKeyboardDict : List Char -> KeyboardDictionary -> String -> KeyboardDictionary
updateKeyboardDict currentGuess currentDict solution =
    LE.indexedFoldl
        (\idx ch dict ->
            Dict.update ch
                (\maybeLetterState ->
                    let
                        checkCorrect : LetterState -> LetterState
                        checkCorrect =
                            checkCorrectChar ch idx solution

                        checkOthers : LetterState -> LetterState
                        checkOthers =
                            checkOtherStatesChar ch solution

                        checkAll : LetterState -> LetterState
                        checkAll =
                            checkCorrect >> checkOthers
                    in
                    case maybeLetterState of
                        Just Blank ->
                            Just <| checkAll Blank

                        -- if at any point is has been marked correct, we want to keep that demarcation on the keyboard
                        Just Correct ->
                            Just Correct

                        Just Present ->
                            Just <| checkAll Present

                        Just Incorrect ->
                            Just <| checkAll Incorrect

                        _ ->
                            Just Blank
                )
                dict
        )
        currentDict
        currentGuess


checkCorrectChar : Char -> Int -> String -> LetterState -> LetterState
checkCorrectChar ch idx word currentLetterState =
    let
        charAsString : String
        charAsString =
            String.fromChar ch

        occurrences : List Int
        occurrences =
            String.indexes charAsString word

        isCorrect : Bool
        isCorrect =
            List.member idx occurrences
    in
    if isCorrect then
        Correct

    else
        currentLetterState


checkOtherStatesChar : Char -> String -> LetterState -> LetterState
checkOtherStatesChar ch word markedLetterState =
    let
        isPresent : Char -> String -> Bool
        isPresent char string =
            char
                |> String.fromChar
                |> (\search -> String.indexes search string)
                |> List.length
                |> (\len -> len > 0)

        currentlyCorrect : Bool
        currentlyCorrect =
            case markedLetterState of
                Correct ->
                    True

                _ ->
                    False
    in
    if currentlyCorrect then
        Correct

    else if isPresent ch word then
        Present

    else
        Incorrect


markCorrect : Int -> String -> Int -> List ( Char, LetterState ) -> List ( Char, LetterState )
markCorrect activeGameRow solution boardIdx tiles =
    if boardIdx == activeGameRow then
        tiles
            |> List.indexedMap
                (\letterIdx tile ->
                    let
                        ( char, currentLetterState ) =
                            tile

                        letterState =
                            checkCorrectChar char letterIdx solution currentLetterState
                    in
                    ( char, letterState )
                )

    else
        tiles


markOtherTiles : Int -> String -> Int -> List ( Char, LetterState ) -> List ( Char, LetterState )
markOtherTiles activeGameRow solution boardIdx tiles =
    let
        calcNewSolution : ( Char, LetterState ) -> Char -> Char
        calcNewSolution tile solutionChar =
            case tile of
                ( _, Correct ) ->
                    '_'

                _ ->
                    solutionChar

        calculatedSolution : String -> String
        calculatedSolution currentSolution =
            List.map2 calcNewSolution tiles (String.toList currentSolution) |> String.fromList |> String.trim
    in
    if boardIdx == activeGameRow then
        tiles
            |> List.map
                (\tile ->
                    let
                        ( char, currentLetterState ) =
                            tile

                        newLetterState =
                            checkOtherStatesChar char (calculatedSolution solution) currentLetterState
                    in
                    ( char, newLetterState )
                )

    else
        tiles


initKeyboardDict : KeyboardDictionary
initKeyboardDict =
    Dict.fromList
        [ ( 'a', Blank )
        , ( 'b', Blank )
        , ( 'c', Blank )
        , ( 'd', Blank )
        , ( 'e', Blank )
        , ( 'f', Blank )
        , ( 'g', Blank )
        , ( 'h', Blank )
        , ( 'i', Blank )
        , ( 'j', Blank )
        , ( 'k', Blank )
        , ( 'l', Blank )
        , ( 'm', Blank )
        , ( 'n', Blank )
        , ( 'o', Blank )
        , ( 'p', Blank )
        , ( 'q', Blank )
        , ( 'r', Blank )
        , ( 's', Blank )
        , ( 't', Blank )
        , ( 'u', Blank )
        , ( 'v', Blank )
        , ( 'w', Blank )
        , ( 'x', Blank )
        , ( 'y', Blank )
        , ( 'z', Blank )
        ]


initKeyboardDictLetters : List (List Char)
initKeyboardDictLetters =
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


initBoard : List KeyboardRow
initBoard =
    List.repeat 6 <| List.repeat 5 ( ' ', Blank )



-- Helper function for dynamic classes


revealTileClass : Letter -> String
revealTileClass letter =
    case letter of
        ( _, Correct ) ->
            "tile reveal is_correct"

        ( _, Present ) ->
            "tile reveal is_present"

        ( _, Incorrect ) ->
            "tile reveal is_incorrect"

        ( _, Pending ) ->
            "tile filled"

        _ ->
            "tile"


keyClass : Char -> Maybe LetterState -> String
keyClass letter maybeLetterState =
    let
        state : String
        state =
            case maybeLetterState of
                Just st ->
                    letterStateAsString st

                Nothing ->
                    letterStateAsString Blank
    in
    "key " ++ "is_" ++ String.fromChar letter ++ " " ++ "is_" ++ String.toLower state


boardRowClass : Int -> Maybe Int -> String
boardRowClass boardRowIdx shakeRow =
    case shakeRow of
        Just idx ->
            if boardRowIdx == idx then
                "board_row shake"

            else
                "board_row"

        Nothing ->
            "board_row"



-- Utils


withDelay : Int -> String
withDelay int =
    let
        delay =
            String.fromInt int
    in
    delay ++ "ms"


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


showEndGameMessage : Cmd Msg
showEndGameMessage =
    Process.sleep 1800 |> Task.perform (\_ -> ShowEndGameMessage)


clearAnimation : Bool -> Cmd Msg
clearAnimation shouldClear =
    if shouldClear then
        Process.sleep 500 |> Task.perform (\_ -> ClearAnimation)

    else
        Cmd.none


clearAlert : Maybe String -> Cmd Msg
clearAlert maybeMessage =
    case maybeMessage of
        Just _ ->
            Process.sleep 1000 |> Task.perform (\_ -> ClearAlert)

        _ ->
            Cmd.none
