module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Plot
import Plot.Line as Line
import Plot.Axis as Axis
import Plot.Grid as Grid
import Plot.Line as Line
import Svg
import Svg.Events
import Table


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { active : String
    , plotState : Plot.State
    , tableState : Table.State
    , casesByFilter : List CasesByFilter
    }


type alias CasesByFilter =
    { filterName : String
    , numberOfCases : Int
    , oldest : Int
    }


casesByFilter : List CasesByFilter
casesByFilter =
    [ CasesByFilter "Support" 10 10
    , CasesByFilter "Development" 25 53
    , CasesByFilter "Product" 21 18
    , CasesByFilter "VIP" 13 22
    , CasesByFilter "Feedback" 22 14
    ]


initialModel : Model
initialModel =
    { active = "black"
    , plotState = Plot.initialState
    , tableState = Table.initialSort "Oldest"
    , casesByFilter = casesByFilter
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = PlotInteraction (Plot.Interaction Msg)
    | SetTableState Table.State
    | LineToggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LineToggle ->
            let
                color =
                    case model.active of
                        "black" ->
                            "red"

                        _ ->
                            "black"
            in
                ( { model | active = color }, Cmd.none )

        PlotInteraction interaction ->
            case interaction of
                Plot.Internal internalMsg ->
                    ( { model | plotState = Plot.update internalMsg model.plotState }, Cmd.none )

                Plot.Custom e ->
                    update e model

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.section []
        [ casesByFilterCard model
        , Html.map PlotInteraction (viewPlot model)
        ]


casesByFilterCard : Model -> Html Msg
casesByFilterCard model =
    Html.div [ class "card" ]
        [ Html.div [ class "cardTitle" ] [ text "Open Cases By Filter" ]
        , Html.div [ class "cardBody" ]
            [ Table.view casesByFilterConfig model.tableState model.casesByFilter ]
        ]


casesByFilterConfig : Table.Config CasesByFilter Msg
casesByFilterConfig =
    Table.config
        { toId = .filterName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Filter" .filterName
            , Table.intColumn "Cases" .numberOfCases
            , Table.intColumn "Oldest" .oldest
            ]
        }


viewPlot : Model -> Svg.Svg (Plot.Interaction Msg)
viewPlot model =
    Plot.plotInteractive
        [ Plot.size ( 580, 325 )
        , Plot.margin ( 10, 50, 50, 10 )
        , Plot.domainLowest (always 0)
        , Plot.domainHighest (always 100)
        ]
        [ Plot.verticalGrid
            [ Grid.values [ 6, 12, 18, 24 ]
            , Grid.lines
                [ Line.stroke "#f9f8f8"
                , Line.strokeWidth 50
                ]
            ]
        , Plot.horizontalGrid
            [ Grid.values [ 10, 25, 50, 75, 100 ]
            , Grid.lines
                [ Line.stroke "lightgrey" ]
            ]
        , Plot.line
            [ Line.stroke model.active
            , Line.strokeWidth 2
            , Line.customAttrs
                [ Svg.Events.onMouseOver (Plot.Custom LineToggle)
                , Svg.Events.onMouseOut (Plot.Custom LineToggle)
                ]
            ]
            data1
        , Plot.line
            [ Line.stroke "grey"
            , Line.strokeWidth 2
            ]
            data2
        , Plot.xAxis
            [ Axis.tickDelta 1
            , Axis.line [ Line.stroke "black" ]
            ]
        , Plot.yAxis
            [ Axis.labelValues [ 0, 10, 25, 50, 75, 100 ]
            , Axis.positionHighest
            , Axis.anchorInside
            ]
        ]


data1 : List ( Float, Float )
data1 =
    List.indexedMap (,)
        [ 28
        , 22
        , 20
        , 19
        , 16
        , 23
        , 25
        , 27
        , 28
        , 50
        , 55
        , 50
        , 25
        , 14
        , 12
        , 14
        , 25
        , 35
        , 40
        , 55
        , 65
        , 69
        , 72
        , 45
        , 33
        , 21
        , 65
        ]
        |> List.map (\( a, b ) -> ( toFloat a + 1, b ))


data2 : List ( Float, Float )
data2 =
    [ ( 1.0, 18.0 )
    , ( 2.0, 23.0 )
    , ( 3.0, 25.0 )
    , ( 4.0, 56.0 )
    , ( 5.0, 48.0 )
    , ( 6.0, 28.0 )
    ]
