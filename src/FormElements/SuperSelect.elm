module FormElements.SuperSelect exposing
    ( view, init, update, subscriptions, defaultProps, Msg(..), Model, Props, Option
    , ExternalMsg(..)
    )

{-| A "select" control that uses auto-complete to assist users.


# Definitions

@docs view, init, update, subscriptions, defaultProps, Msg, Model, Props, Option

-}

import Array
import Browser.Events as BrowserEvents
import ComponentResult as CR
import FormElements.TextInput as TextInput exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Regex
import String
import Task


{-| Messages output from the `view` in the Elm runtime. Combine these with `update`
-}
type Msg a
    = NoOp
    | Clear
    | TextInputMsg TextInput.Msg
    | UnsetFocusedOption
    | SetFocusedOption Int
    | OptionSelected (Option a)
    | KeyPress (Props a) Int


{-| The model the element uses to render itself.
-}
type alias Model =
    { id : String
    , textInputData : TextInput.Model
    , focusedOption : Maybe Int
    , hasFocus : Bool
    }


{-| An "option" for the menu. Each option has a label, and an associated value of whatever type.
-}
type alias Option a =
    ( String, a )


{-| The external message type for the [Component Result](https://package.elm-lang.org/packages/z5h/component-result/latest/)
-}
type ExternalMsg a
    = ValueChanged String (Maybe a)


{-| Alias for the element's [Component Result](https://package.elm-lang.org/packages/z5h/component-result/latest/) type.
-}
type alias SuperSelectResult a =
    CR.ComponentResult Model (Msg a) (ExternalMsg a) Never


{-| Configurable properties for rendering the view
-}
type alias Props a =
    { id : String
    , options : List (Option a)
    , errorText : Maybe String
    , helperText : Maybe String
    , label : String
    , value : Maybe a
    , inputValue : String
    }


textInputId : String -> String
textInputId parentId =
    parentId ++ "__text-input"


{-| Default properties for the element. The props actually passed in should generally have `label` set to
a non-empty string
-}
defaultProps : Props a
defaultProps =
    { id = ""
    , options = []
    , errorText = Nothing
    , helperText = Nothing
    , label = ""
    , value = Nothing
    , inputValue = ""
    }


maybeToBool : Maybe a -> Bool
maybeToBool =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


isRelevantKey : Int -> Bool
isRelevantKey keyCode =
    List.filter (\k -> k == keyCode) [ 38, 40, 13, 9 ]
        |> List.head
        |> maybeToBool


{-| subscriptions
As this element requires listening to keyboard events, the subscriptions must be added
to your main function so it may do so. For each instance of this element in your application,
wire up the subscriptions along with the associated `Props` you would give to that instance's `view`
function
-}
subscriptions : Props a -> Sub (Msg a)
subscriptions superSelectProps =
    Sub.batch
        [ BrowserEvents.onKeyDown
            (D.field "keyCode" D.int
                |> D.andThen
                    (\code ->
                        if isRelevantKey code then
                            D.succeed <| KeyPress superSelectProps code

                        else
                            D.succeed NoOp
                    )
            )
        ]


{-| Creates the initial model for the element. `id` should be a _unique_ string identifier.
-}
init : String -> ( Model, Cmd (Msg a) )
init id =
    let
        ( textInputData, textInputCmd ) =
            TextInput.init <| textInputId id
    in
    ( { id = id
      , textInputData = textInputData
      , focusedOption = Nothing
      , hasFocus = False
      }
    , Cmd.map TextInputMsg textInputCmd
    )


getFocusedOption : Model -> Props a -> Maybe (Option a)
getFocusedOption model props =
    let
        opts =
            getFilteredOptions model props
    in
    case model.focusedOption of
        Just focusedOption ->
            Array.get focusedOption (Array.fromList opts)

        _ ->
            if List.length opts == 1 then
                List.head opts

            else
                Maybe.withDefault Nothing <|
                    Maybe.map (\value -> Just ( props.inputValue, value )) props.value


handleKeyPress : Model -> Props a -> Int -> SuperSelectResult a
handleKeyPress model props keyCode =
    let
        optionIndex =
            Maybe.withDefault -1 model.focusedOption
    in
    case keyCode of
        38 ->
            CR.withModel
                { model
                    | focusedOption =
                        Just <|
                            if optionIndex - 1 >= 0 then
                                optionIndex - 1

                            else
                                optionIndex
                }

        40 ->
            CR.withModel
                { model
                    | focusedOption =
                        Just <|
                            if optionIndex + 1 == List.length (getFilteredOptions model props) then
                                optionIndex

                            else
                                optionIndex + 1
                }

        13 ->
            let
                selectedOption =
                    getFocusedOption model props |> Maybe.map (\( label, value ) -> value)

                inputValue =
                    Maybe.withDefault props.inputValue <|
                        Maybe.map Tuple.first (getFocusedOption model props)
            in
            CR.withModel { model | focusedOption = Nothing }
                |> CR.withExternalMsg (ValueChanged inputValue selectedOption)

        9 ->
            let
                selectedOption =
                    Maybe.map
                        (\( label, value ) -> value)
                        (getFocusedOption model props)

                inputValue =
                    Maybe.withDefault props.inputValue <|
                        Maybe.map (\( label, value ) -> label) (getFocusedOption model props)
            in
            CR.withModel model
                |> CR.withExternalMsg (ValueChanged inputValue selectedOption)

        _ ->
            CR.withModel model


handleTextInputInternal props textInputMsg model =
    case textInputMsg of
        TextInput.OnFocus ->
            { model
                | focusedOption = Nothing
                , hasFocus = True
            }

        TextInput.OnBlur ->
            let
                inputValue =
                    if props.value == Nothing then
                        ""

                    else
                        props.inputValue
            in
            { model
                | hasFocus = False
            }

        _ ->
            model


handleTextInputMsg : Props a -> TextInput.ExternalMsg -> SuperSelectResult a -> SuperSelectResult a
handleTextInputMsg props textInputMsg result =
    case textInputMsg of
        TextInput.ValueChanged value ->
            result
                |> CR.applyExternalMsg (\ext res -> res)
                |> CR.withExternalMsg (ValueChanged value Nothing)


handleTextInputUpdate : Model -> Props a -> TextInput.Msg -> TextInput.TextInputResult -> SuperSelectResult a
handleTextInputUpdate model props textInputMsg textInputResult =
    textInputResult
        |> CR.mapModel (\textInputData -> { model | textInputData = textInputData })
        |> CR.mapModel (handleTextInputInternal props textInputMsg)
        |> CR.mapMsg TextInputMsg
        |> CR.applyExternalMsg (handleTextInputMsg props)


{-| The main function for updating the element in response to `Msg`
-}
update : Msg a -> Model -> Props a -> SuperSelectResult a
update msg model props =
    case msg of
        TextInputMsg textInputMsg ->
            TextInput.update textInputMsg model.textInputData
                |> handleTextInputUpdate model props textInputMsg

        Clear ->
            CR.withModel model
                |> CR.withExternalMsg (ValueChanged "" Nothing)

        SetFocusedOption optionIndex ->
            CR.withModel { model | focusedOption = Just optionIndex }

        UnsetFocusedOption ->
            CR.withModel { model | focusedOption = Nothing }

        OptionSelected ( label, value ) ->
            CR.withModel { model | focusedOption = Nothing }
                |> CR.withExternalMsg (ValueChanged label (Just value))

        KeyPress superSelectProps keyCode ->
            if model.hasFocus then
                handleKeyPress model superSelectProps keyCode

            else
                CR.withModel model

        NoOp ->
            CR.withModel model


textInputSettings : Model -> Props a -> TextInput.Props
textInputSettings model props =
    let
        defaults =
            TextInput.defaultProps
    in
    { defaults
        | id = textInputId props.id
        , value = props.inputValue
        , helperText = props.helperText
        , errorText = props.errorText
        , label = props.label
    }


optionIsFocused : Option a -> Maybe String -> Bool
optionIsFocused ( label, value ) =
    Maybe.map (\focused -> focused == label) >> Maybe.withDefault False


type alias StringGetter a =
    { workingWordList : List (Html (Msg a))
    , currentString : String
    }


onReduce : String -> String -> StringGetter a -> StringGetter a
onReduce matcher letter stringGetter =
    let
        nextString =
            stringGetter.currentString ++ letter

        index =
            List.head (String.indices (String.toUpper matcher) (String.toUpper nextString))

        found =
            Maybe.withDefault False <| Maybe.map (\_ -> True) index

        rightPart =
            Maybe.map
                (\i -> b [] [ text <| String.dropLeft i nextString ])
                index

        leftPart =
            Maybe.map
                (\i -> span [] [ text <| String.left i nextString ])
                index
    in
    { stringGetter
        | workingWordList =
            Maybe.withDefault stringGetter.workingWordList <|
                Maybe.map2
                    (\l r -> stringGetter.workingWordList ++ [ l, r ])
                    leftPart
                    rightPart
        , currentString =
            if found then
                ""

            else
                nextString
    }


getOptionText : String -> String -> Html (Msg a)
getOptionText optionLabel textInputValue =
    let
        result =
            List.foldl
                (onReduce textInputValue)
                { workingWordList = []
                , currentString = ""
                }
                (String.split "" optionLabel)
    in
    span
        [ class "ess-option-list__word" ]
        (result.workingWordList ++ [ span [] [ text result.currentString ] ])


option : Model -> Props a -> Int -> Option a -> Html (Msg a)
option model props index ( label, value ) =
    button
        [ classList
            [ ( "ess-option-list__button", True )
            , ( "ess-option-list__button--focused"
              , Maybe.withDefault -1 model.focusedOption == index
              )
            ]
        , onMouseEnter <| SetFocusedOption index
        , onMouseLeave <| UnsetFocusedOption
        , onMouseDown <| OptionSelected ( label, value )
        ]
        [ getOptionText label props.inputValue
        ]


getMatchedOption : Model -> Props a -> List (Option a) -> Maybe (Option a)
getMatchedOption model props =
    List.head << List.filter (\( label, value ) -> label == props.inputValue)


matches : String -> String -> Bool
matches base =
    String.toUpper >> String.contains (base |> String.toUpper)


getFilteredOptions : Model -> Props a -> List (Option a)
getFilteredOptions model props =
    List.filter (\( label, value ) -> matches props.inputValue label) props.options


showOptionList : Model -> Props a -> List (Option a) -> Bool
showOptionList model props options =
    props.value == Nothing && List.length options /= 0 && model.hasFocus


optionList : Model -> Props a -> Html (Msg a)
optionList model props =
    let
        filteredOptions =
            getFilteredOptions model props

        showOptions =
            showOptionList model props filteredOptions
    in
    filteredOptions
        |> List.indexedMap (option model props)
        |> div
            [ classList
                [ ( "ess-option-list-container", True )
                , ( "ess-option-list-container--open", showOptions )
                ]
            ]


{-| The view for displaying the element.
-}
view : Model -> Props a -> Html (Msg a)
view model props =
    div [ class "ess-container" ]
        [ TextInput.view
            model.textInputData
            (textInputSettings model props)
            |> Html.map TextInputMsg
        , span
            [ classList
                [ ( "ess-clear-button", True )
                , ( "ess-clear-button--show"
                  , Maybe.map (\_ -> True) props.value |> Maybe.withDefault False
                  )
                ]
            , onClick Clear
            ]
            [ text "X" ]
        , optionList model props
        ]
