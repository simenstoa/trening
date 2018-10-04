module Messages exposing (LogInResult, Msg(..), Route(..))

import Activities exposing (Activity)
import Athlete exposing (Athlete)
import Browser
import Http


type alias LogInResult =
    { athlete : Athlete
    , accessToken : String
    }


type Route
    = LoginRoute (Maybe String)
    | RacesRoute
    | NotFoundRoute


type Msg
    = UrlChange Route
    | ClickedLink Browser.UrlRequest
    | SetLoginResult (Result Http.Error LogInResult)
    | UpdateActivities (Result Http.Error (List Activity))
    | FetchMoreActivities
    | ClickedRace Activity
    | NoOp
