module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Plot
import Plot.Line as Line
import Plot.Axis as Axis
import Plot.Grid as Grid
import Plot.Line as Line
import Plot.Area as Area
import Plot.Label as Label
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
    { active = "yellow"
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
            ( model, Cmd.none )

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
        , caseVolumeByStatusCard model
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


caseVolumeByStatusCard : Model -> Html Msg
caseVolumeByStatusCard model =
    Html.div [ class "card" ]
        [ Html.div [ class "cardTitle" ] [ text "Case Volume By Status" ]
        , Html.div [ class "cardBody" ]
            [ Html.map PlotInteraction (viewPlot model) ]
        ]


viewPlot : Model -> Svg.Svg (Plot.Interaction Msg)
viewPlot model =
    Plot.plotInteractive
        [ Plot.size ( 580, 325 )
        , Plot.margin ( 50, 50, 60, 90 )
        , Plot.domainLowest (always 0)
        , Plot.domainHighest (always 100)
        ]
        [ Plot.verticalGrid
            [ Grid.values [ 6, 12, 18, 24 ]
            , Grid.lines
                [ Line.stroke "#2d2d2d"
                , Line.strokeWidth 50
                ]
            ]
        , Plot.horizontalGrid
            [ Grid.values [ 10, 25, 50, 75, 100 ]
            , Grid.lines
                [ Line.stroke "#2d2d2d" ]
            ]
        , Plot.area
            [ Area.stroke "#ffc600"
            , Area.strokeWidth 5
            , Area.fill "#de7d00"
            , Area.opacity 0.35
            , Area.customAttrs
                [ Svg.Events.onMouseOver (Plot.Custom LineToggle)
                , Svg.Events.onMouseOut (Plot.Custom LineToggle)
                ]
            ]
            data1
        , Plot.area
            [ Area.stroke "blue"
            , Area.strokeWidth 5
            , Area.fill "#0075C7"
            , Area.opacity 0.75
            ]
            data2
        , Plot.xAxis
            [ Axis.tickDelta 1
            , Axis.line [ Line.stroke "black" ]
            ]
        , Plot.yAxis
            [ Axis.labelValues [ 0, 10, 25, 50, 75, 100 ]
            ]
        ]


data1 : List ( Float, Float )
data1 =
    List.indexedMap (,)
        [ 59
        , 47
        , 46
        , 49
        , 49
        , 39
        , 48
        , 47
        , 48
        , 50
        , 55
        , 50
        , 45
        , 44
        , 42
        , 54
        , 45
        , 45
        , 40
        , 55
        , 65
        , 69
        , 72
        , 45
        , 43
        , 41
        , 65
        ]
        |> List.map (\( a, b ) -> ( toFloat a + 1, b ))


data2 : List ( Float, Float )
data2 =
    let
        decrement =
            flip (-) 10
    in
        List.map (Tuple.mapSecond decrement) data1
