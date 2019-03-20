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
import ComponentResult as CR
import Date
import Dict
import FormElements.CheckBox as CheckBox
import FormElements.DatePicker as DatePicker
import FormElements.RadioButtons as RadioButtons
import FormElements.SuperSelect as SuperSelect
import FormElements.Switch as Switch
import FormElements.TextInput as TextInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task


type Chamber
    = House
    | Senate


type Msg
    = NoOp
    | GetToday Date.Date
    | ToggleSwitch Bool
    | TextInputMsg TextInput.Msg
    | SuperSelectMsg (SuperSelect.Msg ( String, Place ))
    | DatePickerMsg DatePicker.Model DatePicker.Msg
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
    , datePickerData : Maybe DatePicker.Model
    , superSelectText : String
    , places : List (SuperSelect.Option ( String, Place ))
    , message : String
    , selected : Maybe ( String, Place )
    , selectedDate : Maybe Date.Date
    , chamber : Chamber
    , checkboxOptions : List ( CheckboxOption, String, Bool )
    }


textInputId =
    "my-text-input"


superSelectId =
    "my-super-select"


datePickerProps : Model -> DatePicker.Props
datePickerProps model =
    let
        defaultProps =
            DatePicker.defaultProps "datepicker"
    in
    { defaultProps | selectedDate = model.selectedDate }


type Place
    = NY
    | DC
    | LA


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        textInputData =
            TextInput.init textInputId

        superSelectData =
            SuperSelect.init superSelectId
    in
    ( { switchToggled = False
      , textInputData = textInputData
      , superSelectData = superSelectData
      , datePickerData = Nothing
      , superSelectText = ""
      , message = ""
      , places =
            [ ( "New York", ( "New York", NY ) )
            , ( "Connecticut", ( "New York", NY ) )
            , ( "Washington DC", ( "Washington DC", DC ) )
            , ( "Oregon", ( "LosAngeles", LA ) )
            ]
      , selected = Nothing
      , selectedDate = Nothing
      , chamber = Senate
      , checkboxOptions =
            [ ( Alpha, "alpha", False )
            , ( Bravo, "bravo", False )
            , ( Charlie, "charlie", True )
            , ( Delta, "delta", False )
            ]
      }
    , Task.perform GetToday Date.today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetToday date ->
            let
                ( datePickerData, datePickerCmd ) =
                    DatePicker.init date
            in
            ( { model
                | datePickerData = Just datePickerData
              }
            , Cmd.map (DatePickerMsg datePickerData) datePickerCmd
            )

        ToggleSwitch isToggled ->
            ( { model
                | switchToggled = isToggled
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        DatePickerMsg datePickerModel datePickerMsg ->
            DatePicker.update datePickerMsg datePickerModel (datePickerProps model)
                |> CR.mapMsg (DatePickerMsg datePickerModel)
                |> CR.mapModel (\datePickerData -> { model | datePickerData = Just datePickerData })
                |> CR.applyExternalMsg
                    (\extMsg result ->
                        case extMsg of
                            DatePicker.DateSelected date ->
                                result
                                    |> CR.mapModel (\m -> { m | selectedDate = Just date })
                    )
                |> CR.resolve

        TextInputMsg textInputMsg ->
            TextInput.update textInputMsg model.textInputData
                |> CR.mapMsg TextInputMsg
                |> CR.mapModel (\textInputData -> { model | textInputData = textInputData })
                |> CR.applyExternalMsg
                    (\extMsg result ->
                        case extMsg of
                            TextInput.ValueChanged newVal ->
                                result |> CR.mapModel (\m -> { m | message = newVal })
                    )
                |> CR.resolve

        SuperSelectMsg superSelectMsg ->
            SuperSelect.update superSelectMsg model.superSelectData (superSelectSettings model)
                |> CR.mapMsg SuperSelectMsg
                |> CR.mapModel (\superSelectData -> { model | superSelectData = superSelectData })
                |> CR.applyExternalMsg
                    (\extMsg result ->
                        case extMsg of
                            SuperSelect.ValueChanged textInputValue selectedOption ->
                                result
                                    |> CR.mapModel
                                        (\m ->
                                            { m
                                                | selected = selectedOption
                                                , superSelectText = textInputValue
                                            }
                                        )
                    )
                |> CR.resolve

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
    CheckBox.view <|
        { selected = isChecked
        , label = label
        , handleToggle = CheckboxToggled val
        }


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "padding-top" "24px"
        , style "flex-flow" "row"
        , style "flex-wrap" "wrap"
        , style "max-width" "768px"
        , style "margin" "auto"
        ]
        [ div
            [ style "max-width" "320px"
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            [ div
                [ style "min-width" "130px"
                ]
                [ Switch.view
                    { isOn = model.switchToggled
                    , label =
                        "Developer Mode "
                            ++ (if model.switchToggled then
                                    "(On)"

                                else
                                    "(Off)"
                               )
                    , handleToggle = ToggleSwitch
                    }
                ]
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            [ TextInput.view
                model.textInputData
                (textInputSettings model)
                |> Html.map TextInputMsg
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            [ SuperSelect.view
                model.superSelectData
                (superSelectSettings model)
                |> Html.map SuperSelectMsg
            ]
        , div
            [ style "max-width" "320px"
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            [ RadioButtons.view
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
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            (List.map checkboxOption model.checkboxOptions)
        , div
            [ style "max-width" "320px"
            , style "margin-right" "48px"
            , style "margin-top" "48px"
            ]
            [ case model.datePickerData of
                Just datePickerData ->
                    DatePicker.view
                        datePickerData
                        (datePickerProps model)
                        |> Html.map (DatePickerMsg datePickerData)

                Nothing ->
                    text ""
            ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
