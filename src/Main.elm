module Main exposing (AccessToken(..), Activity, ActivityType(..), ApplicationState(..), Athlete, Config, LogInResult, Model, Msg(..), Route(..), WorkoutType(..), activityDecoder, athleteDecoder, fetchActivities, getAccessToken, getRaces, init, initialModel, main, parseLocation, renderActivity, typeDecoder, update, updateRoute, view, workoutTypeDecoder)

import Browser
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, frenchLocale)
import Html exposing (Html, a, div, h1, h2, header, img, section, span, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
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
    | Hike
    | NordicSki
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
formatDistance =
    format { frenchLocale | decimals = 1 }


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
        ++ String.fromInt min
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

        "Hike" ->
            Decode.succeed Hike

        "NordicSki" ->
            Decode.succeed NordicSki

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
    div [ class "activity", onClick <| ClickedRace race ] [ text race.name ]


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
    section [ class "box more-information" ]
        [ div [ class "more-information-header" ] [ text activity.name ]
        , section [ class "fields" ]
            [ (activity.distance
                / 1000.0
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
        ]


field : String -> String -> Html Msg
field header value =
    div [ class "field" ]
        [ div [ class "field-header" ] [ text header ]
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
                        text "Henter aktiviteter..."

                      else
                        section [ class "flex-content" ] <|
                            [ renderRaces model.races
                            , section [ class "flex-content main-content" ] <|
                                List.map (\race -> moreInformation race) <|
                                    List.filter (\race -> race.active) model.races
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
