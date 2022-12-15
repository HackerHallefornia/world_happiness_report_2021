
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


matches_countryname : String -> Country_2021 -> Bool
matches_countryname countrystring country =
    country.country_name == countrystring

getcountry_by_name : String -> List Country_2021 -> Country_2021
getcountry_by_name countrystring countrylist = 
    Maybe.withDefault emptyCountry <| List.head <| List.filter (matches_countryname countrystring) countrylist

            
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


scatterplot : List String -> List String -> List Float -> List Float -> String -> String -> Svg msg
scatterplot descriptions regions xValues yValues xLabel yLabel =
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
        [ style [] [ TypedSvg.Core.text cssbycountry]
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
                (List.map3 (point xScaleLocal yScaleLocal) descriptions regions xyPoints)      
        ]

point : ContinuousScale Float -> ContinuousScale Float -> String -> String -> (Float, Float)  -> Svg msg
point scaleX scaleY description region_id xyPoint   =
        let
            
            css_region: String
            css_region = country_group_to_css region_id
        in
        
            g [ class [ css_region ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]       
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
                 

country_group_to_css : String -> String
country_group_to_css region_string = 
    case region_string of
        "Western Europe" -> "westerneurope"
        "North America and ANZ" -> "northamericaandanz"
        "Middle East and North Africa" -> "middleeastandnorthafrica"
        "Latin America and Caribbean" -> "latinamericaandcaribbean"
        "Central and Eastern Europe" -> "centralandeasterneurope"
        "East Asia" -> "eastasia"
        "Southeast Asia" -> "southeastasia"
        "Commonwealth of Independent States" -> "commonwealthofindependentstates"
        "Sub-Saharan Africa" -> "sub-saharanafrica"
        "South Asia" -> "southasia"
        _  -> " "
        
cssbycountry : String
cssbycountry =
    """
    .westerneurope circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(21, 111, 187, 1); }
    .westerneurope text { display: none; }
    .westerneurope:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .westerneurope:hover text { display: inline; }

    .northamericaandanz circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 153, 51); }
    .northamericaandanz text { display: none; }
    .northamericaandanz:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .northamericaandanz:hover text { display: inline; }

    .middleeastandnorthafrica circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 153, 0); }
    .middleeastandnorthafrica text { display: none; }
    .middleeastandnorthafrica:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .middleeastandnorthafrica:hover text { display: inline; }

    .latinamericaandcaribbean circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(204, 0, 0); }
    .latinamericaandcaribbean text { display: none; }
    .latinamericaandcaribbean:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .latinamericaandcaribbean:hover text { display: inline; }
    
    .centralandeasterneurope circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 204, 255); }
    .centralandeasterneurope text { display: none; }
    .centralandeasterneurope:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .centralandeasterneurope:hover text { display: inline; }

    .eastasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(153, 51, 255); }
    .eastasia text { display: none; }
    .eastasia:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .eastasia:hover text { display: inline; }

    .southeastasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(51, 51, 0); }
    .southeastasia text { display: none; }
    .southeastasia:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .southeastasia:hover text { display: inline; }

    .commonwealthofindependentstates circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 255, 102); }
    .commonwealthofindependentstates text { display: none; }
    .commonwealthofindependentstates:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .commonwealthofindependentstates:hover text { display: inline; }

    .sub-saharanafrica circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 102, 204); }
    .sub-saharanafrica text { display: none; }
    .sub-saharanafrica:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .sub-saharanafrica:hover text { display: inline; }

    .southasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(51, 51, 204); }
    .southasia text { display: none; }
    .southasia:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
    .southasia:hover text { display: inline; }

    """