module FormElements.SuperSelect exposing (view, init, update, subscriptions, defaultProps, Msg(..), Model, Props, Option)

{-| A super select widget for Elm


# TEA

@docs view, init, update, subscriptions, defaultProps, Msg, Model, Props, Option

-}

import Array
import Browser.Events as BrowserEvents
import FormElements.TextInput as TextInput exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Regex
import String
import Task


{-| Msg

  - `TextInputMsg` the messages used by the underlying `TextInput` element
  - `OptionSelected` fired when an option is selected

-}
type Msg a
    = NoOp
    | TextInputMsg TextInput.Msg
    | UnsetFocusedOption
    | SetFocusedOption Int
    | OptionSelected (Option a)
    | KeyPress (Props a) Int


{-| model

  - `id` The unique id of the element. This is passed as the first argument to `init`
  - `value` The currently selected option
  - `inputValue` The text input value for the underlying `TextInput` element
  - `textInputData` Key for allocating the model of the underlying `TextInput` element

-}
type alias Model =
    { id : String
    , textInputData : TextInput.Model
    , focusedOption : Maybe Int
    , hasFocus : Bool
    }


{-| Option
Option for the select menu
-}
type alias Option a =
    ( String, a )


{-| props

  - `errorText` If you wish to communicate the selection as invalid, set it to the error message
  - `helperText` Helper text to guide the user's input. Not shown if `errorText` is set to non-`Nothing`
  - `label` input label for the text input

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


{-| defaultProps
Default properties for the element. The props actually passed in should generally have `label` set to
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
    List.filter (\k -> k == keyCode) [ 38, 40, 9, 13 ]
        |> List.head
        |> maybeToBool


{-| subscriptions
As this element requires listening to keyboard events, the subscriptions must be added
to your main function so it may do so.
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


{-| Init
Creates the initial model for the element. `id` should be a _unique_ string identifier.
The second argument are the `Option` records since these are often initially decided.
If you are loading the options asynchronously as a response to user input, the options
should be set via a `setNewOptions` in your update function.
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


handleKeyPress : Model -> Props a -> Int -> ( Model, Cmd (Msg a), ( Maybe a, String ) )
handleKeyPress model props keyCode =
    let
        optionIndex =
            Maybe.withDefault -1 model.focusedOption

        ( updateModel, updateValue ) =
            case keyCode of
                38 ->
                    ( { model
                        | focusedOption =
                            Just <|
                                if optionIndex - 1 >= 0 then
                                    optionIndex - 1

                                else
                                    optionIndex
                      }
                    , ( props.value, props.inputValue )
                    )

                40 ->
                    ( { model
                        | focusedOption =
                            Just <|
                                if optionIndex + 1 == List.length (getFilteredOptions model props) then
                                    optionIndex

                                else
                                    optionIndex + 1
                      }
                    , ( props.value, props.inputValue )
                    )

                13 ->
                    let
                        selectedOption =
                            getFocusedOption model props |> Maybe.map (\( label, value ) -> value)

                        inputValue =
                            Maybe.withDefault props.inputValue <|
                                Maybe.map Tuple.first (getFocusedOption model props)
                    in
                    ( { model
                        | focusedOption = Nothing
                      }
                    , ( selectedOption
                      , inputValue
                      )
                    )

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
                    ( model
                    , ( selectedOption
                      , inputValue
                      )
                    )

                _ ->
                    ( model
                    , ( props.value
                      , props.inputValue
                      )
                    )
    in
    ( updateModel, Cmd.none, updateValue )


{-| Update
The function for updating the element.
-}
update : Msg a -> Model -> Props a -> ( Model, Cmd (Msg a), ( Maybe a, String ) )
update msg model props =
    case msg of
        TextInputMsg textInputMsg ->
            TextInput.update textInputMsg model.textInputData
                |> (\( data, cmd ) ->
                        ( { model | textInputData = data }
                        , Cmd.map TextInputMsg cmd
                        )
                   )
                |> (\( newModel, cmd ) ->
                        case textInputMsg of
                            TextInput.OnInput value ->
                                ( newModel
                                , cmd
                                , ( Nothing, value )
                                )

                            TextInput.OnFocus ->
                                ( { newModel
                                    | focusedOption = Nothing
                                    , hasFocus = True
                                  }
                                , cmd
                                , ( props.value, props.inputValue )
                                )

                            TextInput.OnBlur ->
                                let
                                    inputValue =
                                        if props.value == Nothing then
                                            ""

                                        else
                                            props.inputValue
                                in
                                ( { newModel
                                    | hasFocus = False
                                  }
                                , Cmd.none
                                , ( props.value, inputValue )
                                )

                            TextInput.OnInputKeyPress keyCode ->
                                if keyCode == 9 then
                                    handleKeyPress newModel props keyCode

                                else
                                    ( newModel, cmd, ( props.value, props.inputValue ) )

                            _ ->
                                ( newModel, cmd, ( props.value, props.inputValue ) )
                   )

        SetFocusedOption optionIndex ->
            ( { model
                | focusedOption = Just optionIndex
              }
            , Cmd.none
            , ( props.value, props.inputValue )
            )

        UnsetFocusedOption ->
            ( { model | focusedOption = Nothing }, Cmd.none, ( props.value, props.inputValue ) )

        OptionSelected ( label, value ) ->
            ( { model
                | focusedOption = Nothing
              }
            , Cmd.none
            , ( Just value, label )
            )

        KeyPress superSelectProps keyCode ->
            if model.hasFocus then
                handleKeyPress model superSelectProps keyCode

            else
                ( model, Cmd.none, ( props.value, props.inputValue ) )

        NoOp ->
            ( model, Cmd.none, ( props.value, props.inputValue ) )


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


{-| View
The view for displaying the element.
-}
view : Model -> Props a -> Html (Msg a)
view model props =
    div [ class "ess-container" ]
        [ TextInput.view
            model.textInputData
            (textInputSettings model props)
            |> Html.map TextInputMsg
        , optionList model props
        ]
