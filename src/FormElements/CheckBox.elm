module FormElements.CheckBox exposing (view, Props)

{-| A simple checkbox


# Definitions

@docs view, Props

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Configurable properties for rendering the view
-}
type alias Props a =
    { selected : Bool
    , label : String
    , handleToggle : Bool -> a
    }


{-| The view for displaying the element.
-}
view : Props a -> Html a
view props =
    div []
        [ Html.label
            [ for ("radio-btn-" ++ props.label)
            , class "erb-container"
            ]
            [ div
                [ class "erb-circle-group"
                ]
                [ div
                    [ classList
                        [ ( "erb-square-group__outer", True )
                        , ( "erb-square-group__outer--selected", props.selected )
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
                [ text props.label ]
            ]
        , div
            [ class "erb-hidden-checkbox" ]
            [ input
                [ id ("radio-btn-" ++ props.label)
                , type_ "checkbox"
                , onCheck props.handleToggle
                ]
                []
            ]
        ]
