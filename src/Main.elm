module Main exposing (AccessToken(..), Activity, ActivityType(..), ApplicationState(..), Athlete, Config, LogInResult, Model, Msg(..), Route(..), WorkoutType(..), activityDecoder, athleteDecoder, fetchActivities, getAccessToken, getRaces, init, initialModel, main, parseLocation, renderActivity, typeDecoder, update, updateRoute, view, workoutTypeDecoder)

import Browser
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, frenchLocale)
import Html exposing (Html, a, div, footer, h1, h2, h3, header, img, p, section, span, text)
import Html.Attributes exposing (alt, class, href, id, src, tabindex)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, float, int, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Svg exposing (path, polygon, svg)
import Svg.Attributes as SvgAttr exposing (d, points, viewBox)
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>), map, oneOf, s)
import Url.Parser.Query as Query



---- MODEL ----


type AccessToken
    = AccessToken String


type ApplicationState
    = LoggedIn AccessToken Athlete
    | NotLoggedIn
    | Error Http.Error


type alias Athlete =
    { id : Int
    , firstname : String
    , profile : String
    , profile_medium : String
    }


type WorkoutType
    = Normal
    | Race


type ActivityType
    = Run
    | Ride
    | Other


type alias Activity =
    { id : Int
    , name : String
    , distance : Float
    , moving_time : Int
    , total_elevation_gain : Float
    , has_heartrate : Bool
    , workout_type : WorkoutType
    , activity_type : ActivityType
    , start_date_local : Time.Posix
    , active : Bool
    }


type alias LogInResult =
    { athlete : Athlete
    , accessToken : String
    }


type alias Config =
    { clientSecret : String
    , clientId : String
    , origin : String
    }


type alias Model =
    { applicationState : ApplicationState
    , activities : List Activity
    , races : List Activity
    , activityPage : Int
    , moreActivites : Bool
    , config : Config
    , key : Nav.Key
    }


formatDistance : Float -> String
formatDistance distance =
    format { frenchLocale | decimals = 1 } <| distance / 1000.0


formatHours : Int -> String
formatHours seconds =
    (toFloat seconds / (60 * 60)) |> round |> String.fromInt


formatTime : Int -> String
formatTime seconds =
    let
        seconds_float =
            toFloat seconds

        hours =
            (seconds_float / (60 * 60)) |> floor |> modBy 60

        min =
            (seconds_float / 60.0) |> floor |> modBy 60

        sec =
            modBy 60 seconds
    in
    (if hours > 0 then
        String.fromInt hours ++ ":"

     else
        ""
    )
        ++ (if hours > 0 then
                String.padLeft 2 '0' <|
                    String.fromInt min

            else
                String.fromInt min
           )
        ++ ":"
        ++ (String.padLeft 2 '0' <|
                String.fromInt sec
           )


calculateSecPerKm : Float -> Int -> Int
calculateSecPerKm distance moving_time =
    let
        sec_per_km =
            toFloat moving_time / (distance / 1000)
    in
    round sec_per_km


initialModel : Nav.Key -> Config -> Model
initialModel key config =
    { applicationState = NotLoggedIn
    , activities = []
    , races = []
    , activityPage = 0
    , moreActivites = True
    , config = config
    , key = key
    }


init : Config -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init config location key =
    updateRoute (initialModel key config) <| parseLocation location


getRaces : List Activity -> List Activity
getRaces races =
    List.filter (\race -> race.workout_type == Race) races


getActivitiesForLastThreeMonths : List Activity -> Activity -> List Activity
getActivitiesForLastThreeMonths activities activity =
    let
        to =
            activity.start_date_local

        from =
            Time.posixToMillis to - 7776000000 |> Time.millisToPosix
    in
    getActivitiesByInterval from to activities


getActivitiesByInterval : Time.Posix -> Time.Posix -> List Activity -> List Activity
getActivitiesByInterval from to =
    List.filter
        (\activity ->
            Time.posixToMillis activity.start_date_local
                > Time.posixToMillis from
                && Time.posixToMillis activity.start_date_local
                < Time.posixToMillis to
        )


getDistanceForActivities : List Activity -> Float
getDistanceForActivities =
    List.foldl (\activity sum -> sum + activity.distance) 0


getTimeForActivities : List Activity -> Int
getTimeForActivities =
    List.foldl (\activity sum -> sum + activity.moving_time) 0


getRuns : List Activity -> List Activity
getRuns =
    filterOnActivityType Run


getRides : List Activity -> List Activity
getRides =
    filterOnActivityType Ride


getOther : List Activity -> List Activity
getOther =
    filterOnActivityType Other


filterOnActivityType : ActivityType -> List Activity -> List Activity
filterOnActivityType filter =
    List.filter (\{ activity_type } -> activity_type == filter)



---- UPDATE ----


type Msg
    = UrlChange Route
    | ClickedLink Browser.UrlRequest
    | SetLoginResult (Result Http.Error LogInResult)
    | UpdateActivities (Result Http.Error (List Activity))
    | FetchMoreActivities
    | ClickedRace Activity
    | NoOp


athleteDecoder : Decode.Decoder Athlete
athleteDecoder =
    Decode.map4
        Athlete
        (Decode.field
            "id"
            Decode.int
        )
        (Decode.field
            "firstname"
            Decode.string
        )
        (Decode.field
            "profile"
            Decode.string
        )
        (Decode.field
            "profile_medium"
            Decode.string
        )


getAccessToken : Model -> String -> Cmd Msg
getAccessToken model code =
    Http.send SetLoginResult <|
        Http.post
            ("https://www.strava.com/oauth/token?client_id="
                ++ model.config.clientId
                ++ "&client_secret="
                ++ model.config.clientSecret
                ++ "&code="
                ++ code
            )
            Http.emptyBody
        <|
            Decode.map2
                LogInResult
                (Decode.field "athlete" athleteDecoder)
                (Decode.field
                    "access_token"
                    Decode.string
                )


typeDecoder : String -> Decode.Decoder ActivityType
typeDecoder activityAsString =
    case activityAsString of
        "Run" ->
            Decode.succeed Run

        "Ride" ->
            Decode.succeed Ride

        _ ->
            Decode.succeed Other


workoutTypeDecoder : Decoder WorkoutType
workoutTypeDecoder =
    int
        |> Decode.andThen
            (\typeAsInt ->
                case typeAsInt of
                    1 ->
                        Decode.succeed Race

                    _ ->
                        Decode.succeed Normal
            )


activityDecoder : Decode.Decoder Activity
activityDecoder =
    Decode.succeed Activity
        |> required "id" int
        |> required "name" string
        |> required "distance" float
        |> required "moving_time" int
        |> required "total_elevation_gain" float
        |> required "has_heartrate" bool
        |> optional "workout_type" workoutTypeDecoder Normal
        |> required "type" (string |> Decode.andThen typeDecoder)
        |> required "start_date_local" Iso8601.decoder
        |> hardcoded False


fetchActivities : Model -> Cmd Msg
fetchActivities model =
    case model.applicationState of
        LoggedIn (AccessToken token) _ ->
            Http.send UpdateActivities <|
                Http.request
                    { method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://www.strava.com/api/v3/athlete/activities?per_page=200&page=" ++ String.fromInt (model.activityPage + 1)
                    , body = Http.emptyBody
                    , expect = Http.expectJson (Decode.list activityDecoder)
                    , timeout = Nothing
                    , withCredentials = False
                    }

        _ ->
            Cmd.none


type Route
    = LoginRoute (Maybe String)
    | RacesRoute
    | NotFoundRoute


handleUrlChange : Url.Url -> Msg
handleUrlChange url =
    UrlChange <| parseLocation url


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        LoginRoute (Just code) ->
            ( model, getAccessToken model code )

        RacesRoute ->
            ( model, fetchActivities model )

        _ ->
            ( model, Cmd.none )


parseLocation : Url.Url -> Route
parseLocation location =
    let
        parser =
            oneOf
                [ map RacesRoute (s "races")
                , map LoginRoute (s "login" <?> Query.string "code")
                ]
    in
    case Url.Parser.parse parser location of
        Just r ->
            r

        Nothing ->
            NotFoundRoute


handleUrlRequest : Browser.UrlRequest -> Msg
handleUrlRequest url =
    ClickedLink url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            updateRoute model location

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ClickedRace race ->
            ( { model
                | races =
                    List.map
                        (\r ->
                            if r == race then
                                { r | active = not r.active }

                            else
                                r
                        )
                        model.races
              }
            , Cmd.none
            )

        SetLoginResult (Ok logInResult) ->
            ( { model
                | applicationState =
                    LoggedIn
                        (AccessToken logInResult.accessToken)
                        logInResult.athlete
              }
            , Nav.pushUrl model.key (Url.Builder.relative [ "races" ] [])
            )

        SetLoginResult (Err error) ->
            ( { model | applicationState = Error error }, Cmd.none )

        UpdateActivities (Ok activities) ->
            let
                newModel =
                    { model
                        | activities = List.concat [ model.activities, activities ]
                        , races = List.concat [ model.races, getRaces activities ]
                        , activityPage = model.activityPage + 1
                        , moreActivites = List.length activities > 0
                    }
            in
            ( newModel
            , if newModel.moreActivites then
                fetchActivities newModel

              else
                Cmd.none
            )

        UpdateActivities (Err error) ->
            ( { model | applicationState = Error error }, Cmd.none )

        FetchMoreActivities ->
            ( model, fetchActivities model )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


renderActivity : Activity -> Html Msg
renderActivity race =
    div
        [ class "activity"
        , onClick <| ClickedRace race
        , tabindex 0
        ]
        [ div
            [ class
                (if race.active then
                    " active"

                 else
                    ""
                )
            ]
            [ text race.name
            ]
        ]


renderRaces : List Activity -> Html Msg
renderRaces races =
    section [ class "box activities" ] <|
        List.concat
            [ [ h2
                    [ class "small-header" ]
                    [ text "Løp" ]
              ]
            , List.map renderActivity races
            ]


moreInformation : Activity -> Html Msg
moreInformation activity =
    section [ class "fields" ]
        [ (activity.distance
            |> formatDistance
          )
            ++ " km"
            |> field "Lengde"
        , activity.moving_time
            |> formatTime
            |> field "Tid"
        , (calculateSecPerKm activity.distance activity.moving_time
            |> formatTime
          )
            ++ " /km"
            |> field "Fart"
        ]


statisticsFieldCommon : String -> String -> String -> Html Msg
statisticsFieldCommon className label val =
    p [ class className ]
        [ text label
        , span []
            [ text val
            ]
        ]


statisticsHeader : String -> String -> Html Msg
statisticsHeader =
    statisticsFieldCommon "statistics-small-header statistics-field"


statisticsField : String -> String -> Html Msg
statisticsField =
    statisticsFieldCommon "statistics-field"


statistics : Activity -> List Activity -> Html Msg
statistics race activities =
    section [ class "statistics" ]
        [ h3 [ class "statistics-header" ] [ text race.name ]
        , moreInformation race
        , p [] [ text "Din aktivitet siste 90 dager før løpet:" ]
        , statisticsHeader "Antall aktiviteter" <| String.fromInt <| List.length activities
        , statisticsHeader "Total lengde (km)" <| formatDistance (getDistanceForActivities activities)
        , statisticsField "Løping" <| formatDistance (getDistanceForActivities <| getRuns activities)
        , statisticsField "Sykling" <| formatDistance (getDistanceForActivities <| getRides activities)
        , statisticsField "Annet" <| formatDistance (getDistanceForActivities <| getOther activities)
        , statisticsHeader "Total tid (timer)" <| formatHours (getTimeForActivities activities)
        , statisticsField "Løping" <| formatHours (getTimeForActivities <| getRuns activities)
        , statisticsField "Sykling" <| formatHours (getTimeForActivities <| getRides activities)
        , statisticsField "Annet" <| formatHours (getTimeForActivities <| getOther activities)
        ]


field : String -> String -> Html Msg
field header value =
    div [ class "field" ]
        [ div [ class "field-header" ] [ text header ]
        , div [ class "field-value" ] [ text value ]
        ]


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


view : Model -> Browser.Document Msg
view model =
    { title = "S33 trening"
    , body =
        [ case model.applicationState of
            NotLoggedIn ->
                section [ class "content" ]
                    [ section [ class "logIn" ]
                        [ h1 [ class "logInHeader" ] [ text "Logg inn" ]
                        , a
                            [ href
                                ("https://www.strava.com/oauth/authorize?client_id="
                                    ++ model.config.clientId
                                    ++ "&redirect_uri="
                                    ++ model.config.origin
                                    ++ "/login&response_type=code"
                                )
                            , class "logInButton"
                            ]
                            []
                        ]
                    ]

            LoggedIn _ athlete ->
                section [ class "content" ]
                    [ header [ class "header" ]
                        [ div [ class "profile" ]
                            [ span [ class "profile-name" ] [ text <| "Logget inn som " ++ athlete.firstname ]
                            , img [ src athlete.profile_medium, alt "avatar", class "profile-pic" ] []
                            ]
                        ]
                    , if model.moreActivites then
                        text "Henter aktiviteter..."

                      else
                        section [ class "flex-content" ]
                            [ renderRaces model.races
                            , section [ class "main-content" ] <|
                                List.map (\race -> statistics race <| getActivitiesForLastThreeMonths model.activities race) <|
                                    List.filter
                                        (\race -> race.active)
                                        model.races
                            ]
                    ]

            Error err ->
                text <|
                    case err of
                        Http.BadUrl msg ->
                            msg

                        Http.Timeout ->
                            "Timed out"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadStatus response ->
                            String.fromInt response.status.code ++ ": " ++ response.status.message

                        Http.BadPayload msg _ ->
                            msg
        , footer [] [ poweredByStrava ]
        ]
    }



---- PROGRAM ----


main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = handleUrlRequest
        , onUrlChange = handleUrlChange
        }
