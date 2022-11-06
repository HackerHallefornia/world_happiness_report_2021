module Displaydata exposing (..)
import Browser
import Html exposing (Html, text, pre)
import Http

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type Model
  = Failure
  | Loading
  | Success String



type alias Country_2021 =
    { country_name : String
    , regional_indicator : String
    , ladder_score : Maybe Float
    , se_ladder : Maybe Float
    , u_whisker : Maybe Float
    , l_whisker : Maybe Float
    , lg_gdp_pc : Maybe Float
    , social_support : Maybe Float
    , life_expectancy : Maybe Float
    , freedom_lc : Maybe Float
    , generosity : Maybe Float
    , pc_corruption : Maybe Float
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading , 
  Http.get
  { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report-2021.csv"
  , expect = Http.expectString GotText
  }
  )


csvString_to_data : String -> List NBA_Data
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvNBAdata
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeCsvNBAdata : Csv.Decode.Decoder (NBA_Data -> a ) a 
decodeCsvNBAdata =
    Csv.Decode.map NBA_Data
        (Csv.Decode.field "Country name" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "Country name" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Age"(String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Tm" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "X3P" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "X2P" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "TRB" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "AST" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "STL" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "BLK" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "TOV" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "PTS" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "PER" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "OWS" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "DWS" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "WS" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "VORP" (String.toFloat >> Result.fromMaybe "error parsing string"))


-- UPDATE
type Msg
  = GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [ text fullText ]