module Demo exposing
    ( Chamber(..)
    , Model
    , Msg(..)
    , Place(..)
    , init
    , main
    , subscriptions
    , superSelectId
    , superSelectSettings
    , textInputId
    , textInputSettings
    , update
    , view
    )

import Browser
import Dict
import FormElements.CheckBox as CheckBox
import FormElements.RadioButton as RadioButton
import FormElements.SuperSelect as SuperSelect
import FormElements.Switch as Switch
import FormElements.TextInput as TextInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Chamber
    = House
    | Senate


type Msg
    = NoOp
    | ToggleSwitch Bool
    | TextInputMsg TextInput.Msg
    | SuperSelectMsg (SuperSelect.Msg ( String, Place ))
    | SelectChamber Chamber
    | CheckboxToggled CheckboxOption Bool


type CheckboxOption
    = Alpha
    | Bravo
    | Charlie
    | Delta


type alias Model =
    { switchToggled : Bool
    , textInputData : TextInput.Model
    , superSelectData : SuperSelect.Model
    , superSelectText : String
    , places : List (SuperSelect.Option ( String, Place ))
    , message : String
    , selected : Maybe ( String, Place )
    , chamber : Chamber
    , checkboxOptions : List ( CheckboxOption, String, Bool )
    }


textInputId =
    "my-text-input"


superSelectId =
    "my-super-select"


type Place
    = NY
    | DC
    | LA


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( textInputData, textInputCmd ) =
            TextInput.init textInputId

        ( superSelectData, superSelectCmd ) =
            SuperSelect.init superSelectId
    in
    ( { switchToggled = False
      , textInputData = textInputData
      , superSelectData = superSelectData
      , superSelectText = ""
      , message = ""
      , places =
            [ ( "The quick brown fox jumps over the lazy dog", ( "New York", NY ) )
            , ( "Washington DC", ( "Washington DC", DC ) )
            , ( "Los Angeles", ( "LosAngeles", LA ) )
            ]
      , selected = Nothing
      , chamber = Senate
      , checkboxOptions =
            [ ( Alpha, "alpha", False )
            , ( Bravo, "bravo", False )
            , ( Charlie, "charlie", True )
            , ( Delta, "delta", False )
            ]
      }
    , Cmd.batch
        [ Cmd.map TextInputMsg textInputCmd
        , Cmd.map SuperSelectMsg superSelectCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSwitch isToggled ->
            ( { model
                | switchToggled = isToggled
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        TextInputMsg textInputMsg ->
            let
                ( textInputData, textInputCmd ) =
                    TextInput.update textInputMsg model.textInputData

                updatedModel =
                    { model | textInputData = textInputData }

                cmd =
                    Cmd.map TextInputMsg textInputCmd
            in
            case textInputMsg of
                TextInput.OnInput value ->
                    ( { updatedModel | message = value }, cmd )

                _ ->
                    ( updatedModel, cmd )

        SuperSelectMsg superSelectMsg ->
            let
                ( superSelectData, superSelectCmd, ( selected, inputValue ) ) =
                    SuperSelect.update
                        superSelectMsg
                        model.superSelectData
                        (superSelectSettings model)

                updatedModel =
                    { model
                        | superSelectData = superSelectData
                        , selected = selected
                        , superSelectText = inputValue
                    }

                cmd =
                    Cmd.map SuperSelectMsg superSelectCmd
            in
            ( updatedModel, cmd )

        SelectChamber chamber ->
            ( { model | chamber = chamber }, Cmd.none )

        CheckboxToggled val isChecked ->
            let
                checkboxOptions =
                    List.map
                        (\option ->
                            let
                                ( optionVal, optionLabel, optionIsChecked ) =
                                    option
                            in
                            if optionVal == val then
                                ( optionVal, optionLabel, isChecked )

                            else
                                ( optionVal, optionLabel, optionIsChecked )
                        )
                        model.checkboxOptions
            in
            ( { model | checkboxOptions = checkboxOptions }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SuperSelectMsg
            (SuperSelect.subscriptions <|
                superSelectSettings model
            )
        ]


textInputSettings : Model -> TextInput.Props
textInputSettings model =
    let
        defaults =
            TextInput.defaultProps
    in
    { defaults
        | id = textInputId
        , label = "Text Input"
        , value = model.message
        , errorText = Nothing
        , helperText = Just "Some helper text"
    }


superSelectSettings : Model -> SuperSelect.Props ( String, Place )
superSelectSettings model =
    let
        defaults =
            SuperSelect.defaultProps
    in
    { defaults
        | id = superSelectId
        , helperText = Just "Some helper text"
        , label = "Super Select!"
        , value = model.selected
        , inputValue = model.superSelectText
        , options = model.places
    }


checkboxOption : ( CheckboxOption, String, Bool ) -> Html Msg
checkboxOption ( val, label, isChecked ) =
    CheckBox.view
        isChecked
        label
        (CheckboxToggled val)


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "padding-top" "24px"
        , style "flex-flow" "column"
        ]
        [ div
            [ style "max-width" "320px"
            , style "margin-top" "48px"
            ]
            [ Switch.view
                model.switchToggled
                "Show Filters"
                ToggleSwitch
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-top" "48px"
            ]
            [ TextInput.view
                model.textInputData
                (textInputSettings model)
                |> Html.map TextInputMsg
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-top" "48px"
            ]
            [ SuperSelect.view
                model.superSelectData
                (superSelectSettings model)
                |> Html.map SuperSelectMsg
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-top" "48px"
            ]
            [ RadioButton.view
                { options =
                    [ ( "Senate", Senate )
                    , ( "House", House )
                    ]
                , onSelect = SelectChamber
                , selected = model.chamber
                }
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-top" "48px"
            ]
            (List.map checkboxOption model.checkboxOptions)
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
