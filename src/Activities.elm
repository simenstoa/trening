module Activities exposing
    ( Activity
    , activityDecoder
    , calculateSecPerKm
    , getActivitiesForLastThreeMonths
    , getDistanceByWeeks
    , getDistanceForActivities
    , getLastThreeMonths
    , getOther
    , getRaces
    , getRides
    , getRuns
    , getTimeForActivities
    )

import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra exposing (groupWhile)
import Time exposing (Posix)
import Time.Extra exposing (Interval(..))



--- Model ---


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
    , start_date_local : Posix
    , active : Bool
    }



--- Decoders ---


typeDecoder : String -> Decoder ActivityType
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


activityDecoder : Decoder Activity
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



--- Utils ---


getRaces : List Activity -> List Activity
getRaces races =
    List.filter (\race -> race.workout_type == Race) races


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


getLastThreeMonths : Activity -> ( Posix, Posix )
getLastThreeMonths activity =
    let
        to =
            activity.start_date_local

        from =
            Time.posixToMillis to - 7776000000 |> Time.millisToPosix
    in
    ( from, to )


getActivitiesForLastThreeMonths : List Activity -> Activity -> List Activity
getActivitiesForLastThreeMonths activities activity =
    let
        ( from, to ) =
            getLastThreeMonths activity
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


calculateSecPerKm : Float -> Int -> Int
calculateSecPerKm distance moving_time =
    let
        sec_per_km =
            toFloat moving_time / (distance / 1000)
    in
    round sec_per_km


getDistanceByWeeks : Posix -> Posix -> List Activity -> List { x : Float, y : Float }
getDistanceByWeeks from to activities =
    List.map
        (\( bucket, acts ) ->
            { x = toFloat bucket, y = List.foldl (\a sum -> sum + a.distance) 0 acts }
        )
        (groupActivitiesByWeek from to activities)


groupActivitiesByWeek : Posix -> Posix -> List Activity -> List ( Int, List Activity )
groupActivitiesByWeek from to activities =
    let
        startOfFromWeek =
            from |> Time.Extra.floor Monday Time.utc

        endOfToWeek =
            to |> Time.Extra.ceiling Monday Time.utc
    in
    groupActivitiesByInterval startOfFromWeek endOfToWeek weekInterval activities


groupActivitiesByInterval : Posix -> Posix -> Int -> List Activity -> List ( Int, List Activity )
groupActivitiesByInterval from to duration activities =
    let
        filteredActivities =
            List.filter
                (\activity ->
                    let
                        time =
                            Time.posixToMillis activity.start_date_local
                    in
                    time >= Time.posixToMillis from && time < Time.posixToMillis to
                )
                activities

        buckets =
            List.foldl
                (\activity acc ->
                    let
                        bucket =
                            calculateBucket from duration activity
                    in
                    case Dict.get bucket acc of
                        Just acts ->
                            Dict.insert bucket (activity :: acts) acc

                        Nothing ->
                            Dict.insert bucket [ activity ] acc
                )
                Dict.empty
                filteredActivities
    in
    Dict.toList buckets


calculateBucket : Posix -> Int -> Activity -> Int
calculateBucket origo duration activity =
    let
        diff =
            Time.posixToMillis activity.start_date_local - Time.posixToMillis origo
    in
    ceiling (toFloat diff / toFloat duration)


weekInterval : Int
weekInterval =
    604800000
