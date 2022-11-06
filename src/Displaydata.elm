
module Displaydata exposing (..)
import Browser
import Html exposing (Html, text, pre)
import Http

import Csv.Decode exposing (..)
import Csv exposing (..)

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
    , ladder_score :  Float
    , se_ladder :  Float
    , u_whisker :  Float
    , l_whisker :  Float
    , lg_gdp_pc :  Float
    , social_support :  Float
    , life_expectancy :  Float
    , freedom_lc :  Float
    , generosity :  Float
    , pc_corruption :  Float
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading , 
  Http.get
  { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report-2021_simple.csv"
  , expect = Http.expectString GotText
  })


csvString_to_data : String -> List Country_2021
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvCountry
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeCsvCountry : Csv.Decode.Decoder (Country_2021 -> a ) a 
decodeCsvCountry =
    Csv.Decode.map Country_2021
         (Csv.Decode.field "Country name" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "Regional indicator" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Ladder score"(String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Standard error of ladder score" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "upperwhisker" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "lowerwhisker" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Logged GDP per capita" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Social support" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Healthy life expectancy" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Freedom to make life choices" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Generosity" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Perceptions of corruption" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )
        
datListe :List String -> List Country_2021
datListe country_list =
    List.map(\x -> csvString_to_data x) country_list
        |> List.concat


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
      pre [] [ text  (String.fromInt (List.length (datListe [fullText])))]
      
      
      
      
      
      
      