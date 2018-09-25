module Main exposing (AccessToken(..), Activity, ActivityType(..), ApplicationState(..), Athlete, Config, LogInResult, Model, Msg(..), Route(..), WorkoutType(..), athleteDecoder, fetchRaces, getAccessToken, getRaces, init, initialModel, main, parseLocation, raceDecoder, renderActivity, typeDecoder, update, updateRoute, view, workoutTypeDecoder)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, h1, header, img, section, span, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Url
import Url.Builder
import Url.Parser exposing (..)
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
    , workout_type : Maybe WorkoutType
    , actvity_type : ActivityType
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
    , activityPage : Int
    , moreActivites : Bool
    , config : Config
    , key : Nav.Key
    }


initialModel : Nav.Key -> Config -> Model
initialModel key config =
    { applicationState = NotLoggedIn
    , activities = []
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
    List.filter (\race -> race.workout_type == Just Race) races



---- UPDATE ----


type Msg
    = UrlChange Route
    | ClickedLink Browser.UrlRequest
    | SetLoginResult (Result Http.Error LogInResult)
    | UpdateActivities (Result Http.Error (List Activity))
    | FetchMoreActivities
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


workoutTypeDecoder : Maybe Int -> Decode.Decoder WorkoutType
workoutTypeDecoder typeAsInt =
    case typeAsInt of
        Just 1 ->
            Decode.succeed Race

        _ ->
            Decode.succeed Normal


raceDecoder : Decode.Decoder Activity
raceDecoder =
    Decode.map5
        Activity
        (Decode.field "id" Decode.int)
        (Decode.field
            "name"
            Decode.string
        )
        (Decode.field "distance" Decode.float)
        (Decode.maybe (Decode.field "workout_type" (Decode.nullable Decode.int |> Decode.andThen workoutTypeDecoder)))
        (Decode.field "type" (Decode.string |> Decode.andThen typeDecoder))


fetchRaces : Model -> Cmd Msg
fetchRaces model =
    case model.applicationState of
        LoggedIn (AccessToken token) _ ->
            Http.send UpdateActivities <|
                Http.request
                    { method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://www.strava.com/api/v3/athlete/activities?per_page=200&page=" ++ String.fromInt (model.activityPage + 1)
                    , body = Http.emptyBody
                    , expect = Http.expectJson (Decode.list raceDecoder)
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
            ( model, fetchRaces model )

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
                        , activityPage = model.activityPage + 1
                        , moreActivites = List.length activities > 0
                    }
            in
            ( newModel
            , if newModel.moreActivites then
                fetchRaces newModel

              else
                Cmd.none
            )

        UpdateActivities (Err error) ->
            ( { model | applicationState = Error error }, Cmd.none )

        FetchMoreActivities ->
            ( model, fetchRaces model )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


renderActivity : Activity -> Html Msg
renderActivity race =
    div [ class "activity" ] [ text race.name ]


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
                        section [ class "activities" ] <| List.map renderActivity (getRaces model.activities)
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
