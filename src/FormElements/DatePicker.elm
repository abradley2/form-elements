module FormElements.DatePicker exposing (DatePickerResult, ExternalMsg(..), Model, Msg(..), Props, defaultProps, init, update, view)

import ComponentResult as CR
import Date
import FormElements.Util.DateUtils as Utils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Task
import Time exposing (Month(..), Weekday(..))


type alias MonthMap =
    List ( String, Date.Date )


type alias Model =
    { today : Date.Date
    , monthMap : MonthMap
    , outroMonthMap : Maybe MonthMap
    }


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


type alias Props =
    { id : String
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


defaultProps : String -> Props
defaultProps id =
    { id = id
    , weekdayDisplay = weekdayDisplay
    , monthDisplay = monthDisplay
    , dateIsHighlighted = dateIsHighlighted
    , canSelectMonth = canSelectMonth
    , canSelectDate = canSelectDate
    }


type Msg
    = NoOp
    | GetToday Date.Date
    | OnDateSelected Date.Date
    | NextMonth Date.Date
    | PreviousMonth Date.Date


type ExternalMsg
    = DateSelected Date.Date


type alias DatePickerResult =
    CR.ComponentResult Model Msg ExternalMsg Never


init : Date.Date -> ( Model, Cmd Msg )
init initialIndexDate =
    ( { today = Date.fromCalendarDate 1970 Jan 1
      , monthMap = Utils.getMonthMap initialIndexDate
      , outroMonthMap = Nothing
      }
    , Task.perform GetToday Date.today
    )


update : Msg -> Model -> Props -> DatePickerResult
update msg model props =
    case msg of
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
            CR.withModel model
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
                [ ( "_datepicker_yearmonthheader__monthchange", True )
                , ( "_datepicker_yearmonthheader__monthchange--previous", True )
                ]
            , disabled (not canSelectPrevious)
            , onClick (PreviousMonth indexDate)
            ]
            []
        , div
            [ class "_datepicker_yearmonthheader__title"
            ]
            [ text <| props.monthDisplay currentMonth ++ ", " ++ String.fromInt currentYear
            ]
        , button
            [ type_ "button"
            , classList
                [ ( "_datepicker_yearmonthheader__monthchange", True )
                , ( "_datepicker_yearmonthheader__monthchange--next", True )
                ]
            , disabled (not canSelectNext)
            , onClick (NextMonth indexDate)
            ]
            []
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
    button
        [ type_ "button"
        , classList
            [ ( "_datepicker_day", True )
            , ( "_datepicker_day--empty", dayNum == "0" )
            ]
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


view : Model -> Props -> Html Msg
view model props =
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
