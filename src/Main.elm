module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, href, class, alt)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Navigation
import UrlParser exposing (..)


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
    }


initialModel : Config -> Model
initialModel config =
    { applicationState = NotLoggedIn
    , activities = []
    , activityPage = 0
    , moreActivites = True
    , config = config
    }


init : Config -> Navigation.Location -> ( Model, Cmd Msg )
init config location =
    updateRoute (initialModel config) <| parseLocation location


getRaces : List Activity -> List Activity
getRaces races =
    List.filter (\race -> race.workout_type == Just Race) races



---- UPDATE ----


type Msg
    = UrlChange Navigation.Location
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
                    , url = ("https://www.strava.com/api/v3/athlete/activities?per_page=200&page=" ++ toString (model.activityPage + 1))
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


updateRoute : Model -> Route -> ( Model, Cmd Msg )
updateRoute model route =
    case route of
        LoginRoute (Just code) ->
            ( model, getAccessToken model code )

        RacesRoute ->
            ( model, fetchRaces model )

        _ ->
            ( model, Cmd.none )


parseLocation : Navigation.Location -> Route
parseLocation location =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map RacesRoute (UrlParser.s "races")
                , UrlParser.map LoginRoute (UrlParser.s "login" <?> stringParam "code")
                ]
    in
        case UrlParser.parsePath parser location of
            Just r ->
                r

            Nothing ->
                NotFoundRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            updateRoute model <| parseLocation location

        SetLoginResult (Ok logInResult) ->
            ( { model
                | applicationState =
                    LoggedIn
                        (AccessToken logInResult.accessToken)
                        logInResult.athlete
              }
            , Navigation.modifyUrl "/races"
            )

        SetLoginResult (Err error) ->
            ( { model | applicationState = Error error }, Cmd.none )

        UpdateActivities (Ok activities) ->
            ( { model
                | activities = List.concat [ model.activities, activities ]
                , activityPage = model.activityPage + 1
                , moreActivites = (List.length activities) > 0
              }
            , Cmd.none
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


view : Model -> Html Msg
view model =
    case model.applicationState of
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
                        [ span [ class "profile-name" ] [ text athlete.firstname ]
                        , img [ src athlete.profile_medium, alt "avatar", class "profile-pic" ] []
                        ]
                    ]
                , section [ class "activities" ] <| List.map renderActivity (getRaces model.activities)
                , button [ onClick FetchMoreActivities ] [ text "Hent flere" ]
                ]

        Error err ->
            text <| toString err



---- PROGRAM ----


main =
    Navigation.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
