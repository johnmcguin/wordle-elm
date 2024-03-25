module POC exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Tests Run"
        [ Test.test "Runs Tests" <|
            \_ ->
                True
                    |> Expect.equal True
        ]
