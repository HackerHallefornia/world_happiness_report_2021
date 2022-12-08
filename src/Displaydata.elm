
module Displaydata exposing (..)
import Browser
import Html exposing (..)
import Http

import Statistics
import Axis
import Scale exposing (ContinuousScale)
import Shape exposing (..)
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


import Csv.Decode exposing (..)
import Csv exposing (..)

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
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

emptyCountry : Country_2021
emptyCountry = {
    country_name = "String"
    , regional_indicator ="String"
    , ladder_score =  0
    , se_ladder =  0
    , u_whisker =  0
    , l_whisker =  0
    , lg_gdp_pc =  0
    , social_support =  0
    , life_expectancy =  0
    , freedom_lc =  0
    , generosity =  0
    , pc_corruption =  0
    }


get_str_att : String -> Country_2021 -> String
get_str_att str cntry = 
    case str of
        "country_name" ->  .country_name cntry
        "regional_indicator" ->  .regional_indicator cntry
        _ -> str

get_float_att : String -> Country_2021 -> Float
get_float_att float cntry = 
    case float of
        "ladder_score" ->  .ladder_score cntry
        "se_ladder" ->  .se_ladder cntry
        "u_whisker" ->  .u_whisker cntry
        "l_whisker" ->  .l_whisker cntry
        "lg_gdp_pc" ->  .lg_gdp_pc cntry
        "social_support" ->  .social_support cntry
        "life_expectancy" ->  .life_expectancy cntry
        "freedom_lc" ->  .freedom_lc cntry
        "generosity" ->  .generosity cntry
        "pc_corruption" ->  .pc_corruption cntry
        _ -> 0


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
      viewData fullText

-- Displayfunction genrating the Visualization page      
viewData : String -> Html Msg
viewData ls = 
  let
    df = csvString_to_data ls

    x_values : List Float
    x_values = List.map (get_float_att "ladder_score") df
    y_values : List Float
    y_values = List.map (get_float_att "life_expectancy") df

    scat_desc : List String
    scat_desc = List.map (get_str_att "country_name") df
  in     
    Html.div []
        [ Html.p [] [Html.text "Mydata"], 
        scatterplot scat_desc x_values y_values "Ladder Score" "Life expectancy"]
        
-- Visualization functions  
-- Scatterplot 


-- Scatterplot Settings 


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


----- Scatterplot 


scatterplot : List String -> List Float -> List Float -> String -> String -> Svg msg
scatterplot descriptions xValues yValues xLabel yLabel =
    let

        xyPoints =
            List.map2 (\x y -> ( x, y )) xValues yValues
          
        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
                         
              
    in            
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) ( padding - 1 ) ]
            , class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            []
            
        , g 
            [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xValues
             , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                 , y 30                
                ]
                [ text xLabel ]
            ]
        , g 
            [transform [ Translate (padding - 1) padding ] ]
            [ yAxis yValues                             
            , text_
                [ x -40
                , y ( Scale.convert yScaleLocal labelPositions.y - 15)  
                ]
                [ text yLabel ]         
            ]
         , g 
             [transform [ Translate padding padding ] ]
                (List.map2 (point xScaleLocal yScaleLocal) descriptions xyPoints)      
        ]

point : ContinuousScale Float -> ContinuousScale Float -> String -> (Float, Float)  -> Svg msg
point scaleX scaleY description xyPoint   =
            g [ class [ "point" ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]       
                [ circle
                    [ cx (Scale.convert scaleX (Tuple.first xyPoint))
                    , cy (Scale.convert scaleY (Tuple.second xyPoint))
                    , r radius               
                    ]
                    [] 
                    , text_
                    [ x (Scale.convert scaleX (Tuple.first xyPoint))
                    , y (Scale.convert scaleY (Tuple.second xyPoint) - (radius + 3))
                    , textAnchor AnchorMiddle
                    ]
                    [Html.text description]
                ]
            