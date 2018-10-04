module Footer exposing (render)

import Html exposing (Html, footer)
import Messages exposing (Msg)
import Svg exposing (path, polygon, svg)
import Svg.Attributes as SvgAttr exposing (d, points, viewBox)


render : Html Msg
render =
    footer [] [ poweredByStrava ]


poweredByStrava : Html Msg
poweredByStrava =
    svg [ SvgAttr.id "strava_outlined", viewBox "0 0 162.02 30.21" ]
        [ path
            [ SvgAttr.class "cls-1"
            , d "M81.34,22.94A14.15,14.15,0,0,1,77,22.3a9.54,9.54,0,0,1-3.44-1.91l2.7-3.21a8,8,0,0,0,2.59,1.36,9.31,9.31,0,0,0,2.7.41,2.13,2.13,0,0,0,1-.17,0.53,0.53,0,0,0,.3-0.47v0a0.63,0.63,0,0,0-.44-0.54,7.69,7.69,0,0,0-1.65-.45q-1.27-.26-2.43-0.61a8.35,8.35,0,0,1-2-.88,4.27,4.27,0,0,1-1.39-1.36,3.69,3.69,0,0,1-.52-2v0a4.78,4.78,0,0,1,.42-2,4.57,4.57,0,0,1,1.23-1.62,5.85,5.85,0,0,1,2-1.08,8.9,8.9,0,0,1,2.75-.39,12.87,12.87,0,0,1,3.85.52,9.18,9.18,0,0,1,3,1.55l-2.46,3.41a7.57,7.57,0,0,0-2.28-1.13,7.93,7.93,0,0,0-2.26-.36,1.56,1.56,0,0,0-.83.17,0.51,0.51,0,0,0-.27.45v0a0.62,0.62,0,0,0,.41.52,7,7,0,0,0,1.6.45,22.37,22.37,0,0,1,2.64.62,7.8,7.8,0,0,1,2,.94,4.16,4.16,0,0,1,1.32,1.37A3.81,3.81,0,0,1,88,17.78v0A4.69,4.69,0,0,1,87.54,20a4.57,4.57,0,0,1-1.34,1.61,6.35,6.35,0,0,1-2.09,1A9.87,9.87,0,0,1,81.34,22.94Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-1"
            , d "M92.18,11.82H87.73V7.55h13.95v4.27H97.23V22.66H92.18V11.82Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-1"
            , d "M102.39,7.55h7.38A10.1,10.1,0,0,1,113.1,8a5.54,5.54,0,0,1,2.1,1.26,4.61,4.61,0,0,1,1,1.55,5.48,5.48,0,0,1,.35,2v0a4.77,4.77,0,0,1-.8,2.8,5.5,5.5,0,0,1-2.18,1.81l3.52,5.14h-5.76l-2.85-4.32h-1.08v4.32h-5.05V7.55Zm7.23,7.19a2.32,2.32,0,0,0,1.42-.39,1.28,1.28,0,0,0,.52-1.08v0a1.23,1.23,0,0,0-.52-1.09,2.44,2.44,0,0,0-1.4-.36h-2.2v3h2.18Z"
            ]
            []
        , polygon
            [ SvgAttr.class "cls-1"
            , points "146.34 16.16 149.63 22.66 154.47 22.66 146.34 6.61 138.2 22.66 143.04 22.66 146.34 16.16"
            ]
            []
        , polygon
            [ SvgAttr.class "cls-1"
            , points "123.7 16.16 126.99 22.66 131.83 22.66 123.7 6.61 115.58 22.66 120.41 22.66 123.7 16.16"
            ]
            []
        , polygon
            [ SvgAttr.class "cls-1"
            , points "135.02 14.05 131.73 7.55 126.89 7.55 135.02 23.61 143.15 7.55 138.31 7.55 135.02 14.05"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M7.55,22.8V16.5h2.51a2.25,2.25,0,0,1,1.56.53,1.85,1.85,0,0,1,.59,1.46,1.86,1.86,0,0,1-.58,1.44,2.23,2.23,0,0,1-1.57.53H8.61V22.8H7.55Zm1.06-3.32H10a1.14,1.14,0,0,0,.77-0.27,1,1,0,0,0,0-1.49A1.23,1.23,0,0,0,10,17.48H8.61v2Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M17.11,22.75a2.74,2.74,0,0,1-1,.18,2.7,2.7,0,0,1-1-.19,2.5,2.5,0,0,1-1.48-1.62,4.75,4.75,0,0,1-.23-1.53,3.33,3.33,0,0,1,.8-2.38,2.55,2.55,0,0,1,1.92-.85,2.52,2.52,0,0,1,1.9.85,3.36,3.36,0,0,1,.8,2.39,4.75,4.75,0,0,1-.23,1.53,2.63,2.63,0,0,1-.61,1A2.53,2.53,0,0,1,17.11,22.75Zm-2.17-1.37a1.46,1.46,0,0,0,2.32,0,2.89,2.89,0,0,0,.45-1.78,2.64,2.64,0,0,0-.46-1.67,1.43,1.43,0,0,0-2.29,0,2.62,2.62,0,0,0-.46,1.67A2.88,2.88,0,0,0,14.94,21.38Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M21.22,22.8L20,16.5H21.1L22,20.93h0l1.49-4.44h0.6l1.49,4.44h0l0.86-4.44h1.06l-1.21,6.3H25.17l-1.42-4.32h0L22.34,22.8H21.22Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M29.24,22.8V16.5h3.92v1H30.3V19.1h2.61V20H30.3v1.77h2.86v1H29.24Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M35.15,22.8V16.5h2.51a2.28,2.28,0,0,1,1.56.52,2.06,2.06,0,0,1,.21,2.63,1.9,1.9,0,0,1-1,.64l1.36,2.51H38.58l-1.28-2.42h-1.1V22.8H35.15Zm1.06-3.34h1.37a1,1,0,1,0,0-2H36.21v2Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M41.56,22.8V16.5h3.92v1H42.63V19.1h2.61V20H42.63v1.77h2.86v1H41.56Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M47.47,22.8V16.5h2.06a2.73,2.73,0,0,1,1,.19,2.67,2.67,0,0,1,.86.56,2.57,2.57,0,0,1,.62,1,4,4,0,0,1,.23,1.41,4.39,4.39,0,0,1-.23,1.5,2.31,2.31,0,0,1-.64,1,2.68,2.68,0,0,1-.88.51,3.17,3.17,0,0,1-1,.16h-2Zm1.06-1h0.93a1.58,1.58,0,0,0,1.23-.48,2.54,2.54,0,0,0,.44-1.7A2.53,2.53,0,0,0,50.68,18a1.44,1.44,0,0,0-1.17-.53h-1v4.34Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M57,22.8V16.5h2.59a1.86,1.86,0,0,1,1.34.48,1.58,1.58,0,0,1,.5,1.19,1.32,1.32,0,0,1-.73,1.21v0a1.79,1.79,0,0,1,.68.54,1.46,1.46,0,0,1,.28.92q0,1.93-2.31,1.93H57ZM58,19.08h1.39a1.06,1.06,0,0,0,.69-0.2,0.72,0.72,0,0,0,.24-0.59,0.74,0.74,0,0,0-.25-0.6,1.06,1.06,0,0,0-.7-0.21H58v1.6Zm0,2.74h1.58A1.89,1.89,0,0,0,60,21.74a0.77,0.77,0,0,0,.32-0.15,0.8,0.8,0,0,0,.19-0.28,1.06,1.06,0,0,0,.08-0.44,0.74,0.74,0,0,0-.35-0.73A2.43,2.43,0,0,0,59.18,20H58v1.85Z"
            ]
            []
        , path
            [ SvgAttr.class "cls-2"
            , d "M64.57,22.8V20.14L62.51,16.5h1.13l1.44,2.68h0l1.45-2.68h1.13l-2.06,3.65V22.8h-1Z"
            ]
            []
        ]
