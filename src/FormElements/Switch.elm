module FormElements.Switch exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| A simple on/off switch. Use for cases where the user has one option out of a binary selection


# TEA

@docs view

-}
view : Bool -> String -> (Bool -> a) -> Html a
view isOn label onSwitch =
    div [ class "elm-switch-container" ]
        [ div
            [ classList
                [ ( "elm-switch", True )
                , ( "elm-switch--selected", isOn )
                ]
            , onClick (onSwitch <| not isOn)
            ]
            [ div
                [ classList
                    [ ( "elm-switch__switch-indicator", True )
                    , ( "elm-switch__switch-indicator--toggled", isOn )
                    ]
                ]
                []
            ]
        , div
            [ classList
                [ ( "elm-switch__label", True )
                , ( "elm-switch__label--toggled", isOn )
                ]
            ]
            [ text label
            ]
        ]
