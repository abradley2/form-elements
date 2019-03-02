module FormElements.Switch exposing (view, Props)

{-| A simple on/off switch. Use for cases where the user has one option out of a binary selection


# Definitions

@docs view, Props

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Configurable properties for rendering the view
-}
type alias Props a =
    { isOn : Bool
    , label : String
    , handleToggle : Bool -> a
    }


{-| The view for displaying the element.
-}
view : Props a -> Html a
view props =
    div [ class "elm-switch-container" ]
        [ div
            [ classList
                [ ( "elm-switch", True )
                , ( "elm-switch--selected", props.isOn )
                ]
            , onClick (props.handleToggle <| not props.isOn)
            ]
            [ div
                [ classList
                    [ ( "elm-switch__switch-indicator", True )
                    , ( "elm-switch__switch-indicator--toggled", props.isOn )
                    ]
                ]
                []
            ]
        , div
            [ classList
                [ ( "elm-switch__label", True )
                , ( "elm-switch__label--toggled", props.isOn )
                ]
            ]
            [ text props.label
            ]
        ]
