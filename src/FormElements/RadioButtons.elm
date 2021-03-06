module FormElements.RadioButtons exposing (view, Props)

{-| A group of radio buttons


# Definitions

@docs view, Props

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task


{-| Configurable properties for rendering the view
-}
type alias Props x y =
    { selected : x
    , options : List ( String, x )
    , onSelect : x -> y
    }


radioButtonView : (x -> y) -> Bool -> ( String, x ) -> Html y
radioButtonView onSelect selected ( label, val ) =
    div []
        [ Html.label
            [ for ("radio-btn-" ++ label)
            , class "erb-container"

            -- , onClick (OnClick props.selected)
            ]
            [ div
                [ class "erb-circle-group"
                ]
                [ div
                    [ classList
                        [ ( "erb-circle-group__outer", True )
                        , ( "erb-circle-group__outer--selected", selected )
                        ]
                    ]
                    [ div
                        [ classList
                            [ ( "erb-circle-group__inner", True )
                            , ( "erb-circle-group__inner--selected", selected )
                            ]
                        ]
                        []
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
                , onCheck (\_ -> onSelect val)
                ]
                []
            ]
        ]


{-| The view for displaying the element.
-}
view : Props x y -> Html y
view props =
    div [] <|
        List.map
            (\option ->
                let
                    ( label, val ) =
                        option

                    selected =
                        val == props.selected
                in
                radioButtonView props.onSelect selected ( label, val )
            )
            props.options
