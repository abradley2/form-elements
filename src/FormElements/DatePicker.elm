module FormElements.DatePicker exposing (DatePickerResult, ExternalMsg(..), Model, Msg(..), Props, init, update, view)

import ComponentResult as CR
import Date
import FormElements.Util.DateUtils as Utils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed


type alias MonthMap =
    List ( Int, Date.Date )


type alias Model =
    { monthMap : MonthMap
    , outroMonthMap : Maybe MonthMap
    }


type alias Props =
    { id : String
    }


type Msg
    = NoOp


type ExternalMsg
    = DateSelected Date.Date


type alias DatePickerResult =
    CR.ComponentResult Model Msg ExternalMsg Never


init : Date.Date -> Model
init initialIndexDate =
    { monthMap = Utils.getMonthMap initialIndexDate
    , outroMonthMap = Nothing
    }


update : Msg -> Model -> Props -> DatePickerResult
update msg model props =
    CR.withModel model


yearMonthHeaderView : Model -> Props -> Html Msg
yearMonthHeaderView model props =
    div [] [ text "" ]


weekHeaderView : Model -> Props -> Html Msg
weekHeaderView model props =
    div [] [ text "" ]


dayView : Model -> Props -> ( Int, Date.Date ) -> Html Msg
dayView model props ( dayNum, date ) =
    button
        [ type_ "button"
        , classList
            [ ( "_datepicker_day", True )
            , ( "_datepicker_day--empty", dayNum == 0 )
            ]
        ]
        [ if dayNum == 0 then
            text ""

          else
            (String.fromInt >> text) dayNum
        ]


monthView : Model -> Props -> Bool -> List ( Int, Date.Date ) -> Html Msg
monthView model props slideRight monthMap =
    let
        slide =
            if slideRight then
                "right"

            else
                "left"
    in
    div
        [ classList
            [ ( "_datepicker_month", True )
            , ( "_datepicker_month--" ++ slide, True )
            ]
        ]
    <|
        List.map (dayView model props) monthMap


view : Model -> Props -> Html Msg
view model props =
    let
        -- we need to determine if the outro month
        -- is previous or next so we play the right animation
        currentDay =
            List.head model.monthMap
                |> Maybe.map (Tuple.second >> Date.toRataDie)
                |> Maybe.withDefault 1
                |> String.fromInt

        outroDay =
            model.outroMonthMap
                |> Maybe.andThen List.head
                |> Maybe.map (Tuple.second >> Date.toRataDie)
                |> Maybe.withDefault 0
                |> String.fromInt

        slideRight =
            currentDay > outroDay
    in
    div
        [ class "_datepicker_container"
        ]
        [ yearMonthHeaderView model props
        , weekHeaderView model props
        , Keyed.node "div"
            [ class "_datepicker_monthcontainer" ]
            [ ( props.id ++ currentDay, monthView model props slideRight model.monthMap )
            , ( props.id ++ "out" ++ outroDay
              , Maybe.map (monthView model props slideRight) model.outroMonthMap
                    |> Maybe.withDefault (text "")
              )
            ]
        ]
