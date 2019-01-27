module FormElements.CheckBox exposing (view)

{-| A simple checkbox


# Definitions

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Renders a checkbox. The first argument is whether or not it is selected.
The second argument represents the label.
-}
view : Bool -> String -> (Bool -> a) -> Html a
view selected label handleClick =
    div []
        [ Html.label
            [ for ("radio-btn-" ++ label)
            , class "erb-container"
            ]
            [ div
                [ class "erb-circle-group"
                ]
                [ div
                    [ classList
                        [ ( "erb-square-group__outer", True )
                        , ( "erb-square-group__outer--selected", selected )
                        ]
                    ]
                    [ div
                        [ classList
                            []
                        ]
                        []
                    , div
                        [ classList
                            [ ( "erb-square-group__inner", True )
                            ]
                        ]
                        [ span [ class "erb-checkmark" ] [] ]
                    ]
                ]
            , div
                []
                [ text label ]
            ]
        , div
            [ class "erb-hidden-checkbox" ]
            [ input
                [ id ("radio-btn-" ++ label)
                , type_ "checkbox"
                , onCheck handleClick
                ]
                []
            ]
        ]
