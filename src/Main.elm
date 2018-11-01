module Main exposing (main)

import Activities exposing (Activity, activityDecoder, calculateSecPerKm, getActivitiesForLastThreeMonths, getDistanceByWeeks, getDistanceForActivities, getLastThreeMonths, getOther, getRaces, getRides, getRuns, getTimeForActivities)
import Athlete exposing (Athlete, athleteDecoder)
import Browser
import Browser.Navigation as Nav
import Footer
import Formatting exposing (formatDistance, formatHours, formatTime)
import Graphs exposing (renderBarChart, renderGraph)
import Html exposing (Html, a, div, h1, h2, h3, header, img, p, section, span, text)
import Html.Attributes exposing (alt, class, href, src, tabindex)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import Messages exposing (LogInResult, Msg(..), Route(..))
import Url
import Url.Builder
import Url.Parser exposing ((<?>), map, oneOf, s)
import Url.Parser.Query as Query



---- MODEL ----


type AccessToken
    = AccessToken String


type ApplicationState
    = LoggedIn AccessToken Athlete
    | NotLoggedIn
    | Error Http.Error


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



---- UPDATE ----


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


raceKeyPressedDecoder : Activity -> KeyboardEvent -> Maybe Msg
raceKeyPressedDecoder race event =
    case event.keyCode of
        Enter ->
            Just <| ClickedRace race

        Spacebar ->
            Just <| ClickedRace race

        _ ->
            Nothing


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
        [ class <|
            "activity"
                ++ (if race.active then
                        " active"

                    else
                        ""
                   )
        , onClick <| ClickedRace race
        , tabindex 0
        , on "keypress" <| considerKeyboardEvent <| raceKeyPressedDecoder race
        ]
        [ div
            []
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
    let
        ( from, to ) =
            getLastThreeMonths race
    in
    section [ class "statistics" ]
        [ h3 [ class "statistics-header" ] [ text race.name ]
        , moreInformation race
        , p [] [ text "Din aktivitet siste 90 dager før løpet:" ]
        , statisticsHeader "Antall aktiviteter" <| String.fromInt <| List.length activities
        , statisticsHeader "Total lengde (km)" <| formatDistance (getDistanceForActivities activities)
        , statisticsField "Løping" <| formatDistance (getDistanceForActivities <| getRuns activities)
        , renderBarChart <| List.map (\act -> { act | y = act.y / 1000 }) <| getDistanceByWeeks from to <| getRuns activities
        , statisticsField "Sykling" <| formatDistance (getDistanceForActivities <| getRides activities)
        , statisticsField "Annet" <| formatDistance (getDistanceForActivities <| getOther activities)
        , statisticsHeader "Total tid (timer)" <| formatHours (getTimeForActivities activities)
        , statisticsField "Løping" <| formatHours (getTimeForActivities <| getRuns activities)
        , statisticsField "Sykling" <| formatHours (getTimeForActivities <| getRides activities)
        , statisticsField "Annet" <| formatHours (getTimeForActivities <| getOther activities)
        ]


field : String -> String -> Html Msg
field header_ value =
    div [ class "field" ]
        [ div [ class "field-header" ] [ text header_ ]
        , div [ class "field-value" ] [ text value ]
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
                        section [ class "fetching-activities" ] [ text "Henter aktiviteter..." ]

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
        , Footer.render
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
