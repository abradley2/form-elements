module FormElements.TextInput exposing (view, init, update, defaultProps, Msg(..), Model, Props)

{-| A text input widget for Elm


# TEA

@docs view, init, update, defaultProps, Msg, Model, Props

-}

import Browser.Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe as M
import Task


{-| Msg

  - `OnInput`

-}
type Msg
    = NoOp
    | OnInput String
    | OnFocus
    | OnBlur
    | OnInputKeyPress Int


{-| Model
Tracks the internal foxus state of the element
-}
type alias Model =
    { id : String
    , hasFocus : Bool
    }


type FieldType
    = Outline
    | Filled


{-| Props

  - `id` Unique id for the elements
  - `label` Label for the text input
  - `value` The value of the text input
  - `errorText` Set this to a string when there is a validation error to display to the user. Will override `helperText`
  - `helperText` Helpful text displayed under the text input

-}
type alias Props =
    { id : String
    , label : String
    , value : String
    , errorText : Maybe String
    , helperText : Maybe String
    , fieldType : FieldType
    }


{-| defaultProps
Default props for the text input. Generally the label should be set before passing
these defaults to the `view`
-}
defaultProps : Props
defaultProps =
    { id = ""
    , label = ""
    , value = ""
    , errorText = Nothing
    , helperText = Nothing
    , fieldType = Outline
    }


{-| init
Initialization for the text input model. The `id` passed in _must be unique_
-}
init : String -> ( Model, Cmd Msg )
init id =
    ( { id = id
      , hasFocus = False
      }
    , Cmd.none
    )


{-| update
The update function for the text input element
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnBlur ->
            ( { model
                | hasFocus = False
              }
            , Cmd.none
            )

        OnFocus ->
            ( { model | hasFocus = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


getTextInputId : String -> String
getTextInputId id =
    id ++ "__eti-text-input"


textInputLabel : Model -> Props -> Html Msg
textInputLabel model props =
    div
        [ classList
            [ ( "eti-text-input__label", True )
            , ( "eti-text-input__label--raised", model.hasFocus || props.value /= "" )
            , ( "eti-text-input__label--active", model.hasFocus )
            ]
        ]
        [ span [] [ text props.label ]
        ]


textInput : Model -> Props -> Html Msg
textInput model props =
    div
        [ class "eti-text-input__wrapper"
        ]
        [ textInputLabel model props
        , input
            [ id <| getTextInputId model.id
            , class "eti-text-input__input"
            , type_ "text"
            , value props.value
            , onInput OnInput
            , onBlur OnBlur
            , onFocus OnFocus
            , on "keydown" (Json.Decode.map OnInputKeyPress Html.Events.keyCode)
            ]
            []
        ]


bottomLineWrapper : Model -> Props -> Html Msg
bottomLineWrapper model props =
    div
        [ class "eti-bottom-line__container"
        ]
        [ div [ class "eti-bottom-line__relative-wrapper" ]
            [ div
                [ classList
                    [ ( "eti-bottom-line__highlighter", True )
                    , ( "eti-bottom-line__highlighter--active", model.hasFocus )
                    ]
                ]
                []
            ]
        ]


getBottomTextData props =
    case props.errorText of
        Just errorText ->
            Just ( class "eti-bottom-text eti-bottom-text--error", errorText )

        Nothing ->
            M.map
                (\helperText ->
                    ( class "eti-bottom-text eti-bottom-text--helper", helperText )
                )
                props.helperText


bottomText : Model -> Props -> Html Msg
bottomText model props =
    M.withDefault (div [] []) <|
        M.map
            (\( classAttr, innerText ) -> div [ classAttr ] [ text innerText ])
            (getBottomTextData props)


{-| view
The view function for displaying the text input element
-}
view : Model -> Props -> Html Msg
view model props =
    div
        [ classList
            [ ( "eti-container", True )
            , ( "eti-outline", props.fieldType == Outline )
            , ( "eti-outline--focused", model.hasFocus )
            ]
        ]
        [ textInput model props
        , if props.fieldType == Filled then
            bottomLineWrapper model props

          else
            div [] []
        , bottomText model props
        ]
