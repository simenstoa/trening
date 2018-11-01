module Graphs exposing (renderBarChart, renderGraph)

import Activities exposing (Activity)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import Plot exposing (area, circle, histogram, histogramBar, viewBars, viewSeries)
import Svg exposing (Svg)


renderGraph : List { x : Float, y : Float } -> Html Msg
renderGraph plots =
    div [ class "graph" ]
        [ viewSeries
            [ area (List.map (\{ x, y } -> circle x y)) ]
          <|
            plots
        ]


renderBarChart : List { x : Float, y : Float } -> Html Msg
renderBarChart plots =
    div [ class "graph" ]
        [ viewBars histo plots ]


histo : Plot.Bars (List { x : Float, y : Float }) msg
histo =
    histogram
        (List.map
            (\data -> histogramBar data.y)
        )
