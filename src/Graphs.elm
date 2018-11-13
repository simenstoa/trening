module Graphs exposing (renderBarChart, renderGraph)

import Activities exposing (Activity)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import Plot
    exposing
        ( Bars
        , Point
        , area
        , circle
        , defaultBarsPlotCustomizations
        , histogram
        , histogramBar
        , viewBars
        , viewBarsCustom
        , viewSeries
        )
import Svg exposing (Svg, linearGradient, stop)
import Svg.Attributes exposing (id, offset, stopColor, stopOpacity, stroke)


renderGraph : List Point -> Html Msg
renderGraph plots =
    div [ class "graph" ]
        [ viewSeries
            [ area (List.map (\{ x, y } -> circle x y)) ]
          <|
            plots
        ]


renderBarChart : List Point -> Html Msg
renderBarChart plots =
    div [ class "graph" ]
        [ viewBarsCustom defaultBarsPlotCustomizations renderHistogram plots ]


renderHistogram : Bars (List Point) Msg
renderHistogram =
    histogram
        (List.map
            (\data -> histogramBar data.y)
        )
