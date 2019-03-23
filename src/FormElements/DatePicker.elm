module FormElements.DatePicker exposing (view, init, update, defaultProps, Msg(..), Model, Props, DatePickerResult, ExternalMsg(..))

{-| A "Date Picker" control for selecting dates from a calendar-like interface.


# Definitions

@docs view, init, update, defaultProps, Msg, Model, Props, DatePickerResult, ExternalMsg

-}

import ComponentResult as CR
import Date
import FormElements.Util.DateUtils as Utils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as D
import Maybe.Extra as M
import Task
import Time exposing (Month(..), Weekday(..))


type alias MonthMap =
    List ( String, Date.Date )


{-| The model the element uses to render itself.
-}
type alias Model =
    { showDatePicker : Bool
    , today : Date.Date
    , monthMap : MonthMap
    , outroMonthMap : Maybe MonthMap
    }


selectedDateDisplay : Date.Date -> String
selectedDateDisplay date =
    let
        year =
            Date.year date

        month =
            monthDisplay (Date.month date)

        day =
            Date.day date
    in
    month ++ " " ++ String.fromInt day ++ ", " ++ String.fromInt year


monthDisplay : Month -> String
monthDisplay month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayDisplay : Time.Weekday -> String
weekdayDisplay day =
    case day of
        Sun ->
            "Sun"

        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"


{-| Configurable properties for rendering the view
-}
type alias Props =
    { id : String
    , label : String
    , selectedDateDisplay : Date.Date -> String
    , selectedDate : Maybe Date.Date
    , weekdayDisplay : Weekday -> String
    , monthDisplay : Month -> String
    , dateIsHighlighted : Date.Date -> Bool
    , canSelectDate : Date.Date -> Bool
    , canSelectMonth : ( Month, Int ) -> Bool
    }


dateIsHighlighted : Date.Date -> Bool
dateIsHighlighted date =
    False


canSelectMonth : ( Month, Int ) -> Bool
canSelectMonth ( month, year ) =
    True


canSelectDate : Date.Date -> Bool
canSelectDate date =
    True


{-| Convenience function for getting a default `Props` record. Requires that
the element's id be passed in.
-}
defaultProps : String -> Props
defaultProps id =
    { id = id
    , label = "Select a Date"
    , selectedDateDisplay = selectedDateDisplay
    , selectedDate = Nothing
    , weekdayDisplay = weekdayDisplay
    , monthDisplay = monthDisplay
    , dateIsHighlighted = dateIsHighlighted
    , canSelectMonth = canSelectMonth
    , canSelectDate = canSelectDate
    }


{-| Messages return from the view
-}
type Msg
    = NoOp
    | ToggleShowDatePicker Bool
    | GetToday Date.Date
    | OnDateSelected Date.Date
    | NextMonth Date.Date
    | PreviousMonth Date.Date


{-| The external message type for the [Component Result type](https://package.elm-lang.org/packages/z5h/component-result/latest/)
-}
type ExternalMsg
    = DateSelected Date.Date


{-| Alias for the element's [Component Result type](https://package.elm-lang.org/packages/z5h/component-result/latest/) type.
-}
type alias DatePickerResult =
    CR.ComponentResult Model Msg ExternalMsg Never


{-| Creates the initial model for the element, using an "index date" to determine what
starting date the calendar opens up to. This does not have to be `Date.today`
-}
init : Date.Date -> ( Model, Cmd Msg )
init initialIndexDate =
    ( { showDatePicker = False
      , today = Date.fromCalendarDate 1970 Jan 1
      , monthMap = Utils.getMonthMap initialIndexDate
      , outroMonthMap = Nothing
      }
    , Task.perform GetToday Date.today
    )


{-| The main function for updating the element in response to `Msg`
-}
update : Msg -> Model -> Props -> DatePickerResult
update msg model props =
    case msg of
        ToggleShowDatePicker showDatePicker ->
            CR.withModel { model | showDatePicker = showDatePicker }

        NextMonth indexDate ->
            let
                nextMonth =
                    Utils.getMonthMap <| Date.add Date.Months 1 indexDate
            in
            CR.withModel { model | monthMap = nextMonth, outroMonthMap = Just model.monthMap }

        PreviousMonth indexDate ->
            let
                previousMonth =
                    Utils.getMonthMap <| Date.add Date.Months -1 indexDate
            in
            CR.withModel { model | monthMap = previousMonth, outroMonthMap = Just model.monthMap }

        GetToday today ->
            CR.withModel { model | today = today }

        OnDateSelected date ->
            CR.withModel { model | showDatePicker = False }
                |> CR.withExternalMsg (DateSelected date)

        _ ->
            CR.withModel model


yearMonthHeaderView : Model -> Props -> Date.Date -> Html Msg
yearMonthHeaderView model props indexDate =
    let
        currentYear =
            Date.year indexDate

        currentMonth =
            Date.month indexDate

        canSelectNext =
            Date.add Date.Months 1 indexDate
                |> Date.month
                |> (\month -> props.canSelectMonth ( month, Date.year indexDate ))

        canSelectPrevious =
            Date.add Date.Months -1 indexDate
                |> Date.month
                |> (\month -> props.canSelectMonth ( month, Date.year indexDate ))
    in
    div
        [ class "_datepicker_yearmonthheader"
        ]
        [ button
            [ type_ "button"
            , classList
                [ ( "_datepicker_yearmonthheader__monthchangebutton", True )
                , ( "_datepicker_yearmonthheader__monthchangebutton--disabled", not canSelectPrevious )
                ]
            , disabled (not canSelectPrevious)
            , onClick (PreviousMonth indexDate)
            ]
            [ span
                [ classList
                    [ ( "_datepicker_yearmonthheader__monthchange", True )
                    , ( "_datepicker_yearmonthheader__monthchange--previous", True )
                    ]
                ]
                []
            ]
        , div
            [ class "_datepicker_yearmonthheader__title"
            ]
            [ text <| props.monthDisplay currentMonth ++ " " ++ String.fromInt currentYear
            ]
        , button
            [ type_ "button"
            , classList
                [ ( "_datepicker_yearmonthheader__monthchangebutton", True )
                , ( "_datepicker_yearmonthheader__monthchangebutton--disabled", not canSelectNext )
                ]
            , disabled (not canSelectNext)
            , onClick (NextMonth indexDate)
            ]
            [ span
                [ classList
                    [ ( "_datepicker_yearmonthheader__monthchange", True )
                    , ( "_datepicker_yearmonthheader__monthchange--next", True )
                    ]
                ]
                []
            ]
        ]


weekHeaderView : Model -> Props -> Html Msg
weekHeaderView model props =
    div
        [ class "_datepicker_weekheader"
        ]
        (List.map
            (\day ->
                span
                    [ class "_datepicker_weekheader__day"
                    ]
                    [ text <| weekdayDisplay day
                    ]
            )
            [ Sun
            , Mon
            , Tue
            , Wed
            , Thu
            , Fri
            , Sat
            ]
        )


dayView : Model -> Props -> ( String, Date.Date ) -> Html Msg
dayView model props ( dayNum, date ) =
    let
        isSelectable =
            props.canSelectDate date
    in
    button
        [ type_ "button"
        , classList
            [ ( "_datepicker_day", True )
            , ( "_datepicker_day--empty", dayNum == "0" )
            , ( "_datepicker_day--disabled", not isSelectable )
            ]
        , disabled (not isSelectable)
        , onClick (OnDateSelected date)
        ]
        [ if dayNum == "0" then
            text ""

          else
            text dayNum
        ]


monthView : Model -> Props -> String -> List ( String, Date.Date ) -> Html Msg
monthView model props slideModifier monthMap =
    div
        [ classList
            [ ( "_datepicker_month", True )
            , ( "_datepicker_month--" ++ slideModifier, True )
            ]
        ]
    <|
        List.map (dayView model props) monthMap


{-| Given a "monthMap" remove all the "placeholder days" (those which are "0" strings)
-}
validDays : List ( String, Date.Date ) -> List ( String, Date.Date )
validDays =
    List.filter (\( dayNum, _ ) -> dayNum /= "0")


{-| The view for displaying the element.
-}
view : Model -> Props -> Html Msg
view model props =
    div []
        [ div
            [ classList
                [ ( "eti-container", True )
                , ( "eti-outline", True )
                , ( "eti-outline--focused", model.showDatePicker )
                ]
            ]
            [ div
                [ class "eti-text-input__wrapper"
                ]
                [ div
                    [ classList
                        [ ( "eti-text-input__label", True )
                        , ( "eti-text-input__label--raised", M.isJust props.selectedDate )
                        ]
                    ]
                    [ span [] [ text props.label ]
                    ]
                , button
                    [ type_ "button"
                    , onClick (ToggleShowDatePicker <| not model.showDatePicker)
                    , classList
                        [ ( "eti-text-input__input", True )
                        , ( "_datepicker_textinput__input", True )
                        ]
                    ]
                    [ text
                        (Maybe.map
                            selectedDateDisplay
                            props.selectedDate
                            |> Maybe.withDefault ""
                        )
                    ]
                ]
            ]
        , div [ class "_datepicker_popupcontainer" ]
            [ div
                [ classList
                    [ ( "_datepicker_popup", True )
                    , ( "_datepicker_popup--show", True )
                    ]
                ]
                [ if model.showDatePicker then
                    datePickerView model props

                  else
                    text ""
                ]
            ]
        ]


datePickerView : Model -> Props -> Html Msg
datePickerView model props =
    let
        -- we need to determine if the outro month
        -- is previous or next so we play the right animation
        -- TODO: investigate the need to cache this in the model
        currentDay =
            model.monthMap
                |> validDays
                |> List.head
                |> Maybe.map (Tuple.second >> Date.toRataDie)
                |> Maybe.withDefault 1
                |> String.fromInt

        outroDay =
            model.outroMonthMap
                |> Maybe.map validDays
                |> Maybe.andThen List.head
                |> Maybe.map (Tuple.second >> Date.toRataDie)
                |> Maybe.withDefault 0
                |> String.fromInt

        slide =
            if currentDay > outroDay then
                "left"

            else
                "right"

        mIndexDate =
            model.monthMap
                |> validDays
                |> List.head
                |> Maybe.map Tuple.second
    in
    Maybe.map
        (\indexDate ->
            div
                [ class "_datepicker_container"

                -- , stopPropagationOn "click" (D.map (\a -> ( a, True )) (D.succeed NoOp))
                ]
                [ yearMonthHeaderView model props indexDate
                , weekHeaderView model props
                , Keyed.node "div"
                    [ class "_datepicker_monthcontainer" ]
                    [ ( props.id ++ currentDay, monthView model props ("slidein" ++ slide) model.monthMap )
                    , ( props.id ++ "out" ++ outroDay
                      , Maybe.map (monthView model props ("slideout" ++ slide)) model.outroMonthMap
                            |> Maybe.withDefault (text "")
                      )
                    ]
                ]
        )
        mIndexDate
        |> Maybe.withDefault (text "")
