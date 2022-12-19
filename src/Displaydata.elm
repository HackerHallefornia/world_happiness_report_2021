
module Displaydata exposing (..)
import Browser
import Html exposing (..)
import Http
import Color exposing (rgb)
import Statistics
import Axis
import Scale exposing (ContinuousScale)
import Shape exposing (..)
import TypedSvg exposing (circle, g, style, svg, text_,rect )
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, fontWeight, transform, viewBox, fill)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y, height,width)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..), FontWeight(..))


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


emptyts: Ts_data
emptyts = {
    country_name = "String"
    -- add country indicator later
    , year = 0
    , ladder_score =  0
    , lg_gdp_pc =  0
    , social_support =  0
    , life_expectancy =  0
    , freedom_lc =  0
    , generosity =  0
    , pc_corruption =  0
    , positive_affect = 0
    , negative_affect = 0 
    }

type alias Ts_data = 
      { country_name : String
    -- add country indicator later
    , year : Float
    , ladder_score :  Float
    , lg_gdp_pc :  Float
    , social_support :  Float
    , life_expectancy :  Float
    , freedom_lc :  Float
    , generosity :  Float
    , pc_corruption :  Float
    , positive_affect : Float
    , negative_affect : Float 
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
        "Happiness Score" ->  .ladder_score cntry
        "se_ladder" ->  .se_ladder cntry
        "u_whisker" ->  .u_whisker cntry
        "l_whisker" ->  .l_whisker cntry
        "Logged GDP per capita" ->  .lg_gdp_pc cntry
        "Social support" ->  .social_support cntry
        "Life expectancy" ->  .life_expectancy cntry
        "Freedom to make life choices" ->  .freedom_lc cntry
        "Generosity" ->  .generosity cntry
        "Perceived corruption" ->  .pc_corruption cntry
        _ -> 0

csvString_to_data : String -> List Country_2021
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvCountry
        |> Result.toMaybe
        |> Maybe.withDefault []


datListe :List String -> List Country_2021
datListe country_list =
    List.map(\x -> csvString_to_data x) country_list
        |> List.concat


csvString_to_data_ts : String -> List Ts_data
csvString_to_data_ts csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvtimeseries
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

decodeCsvtimeseries : Csv.Decode.Decoder (Ts_data -> a ) a 
decodeCsvtimeseries =
    Csv.Decode.map Ts_data
         (Csv.Decode.field "Country name" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "year"(String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Life Ladder" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Log GDP per capita" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Social support" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Healthy life expectancy at birth" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Freedom to make life choices" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Generosity" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Perceptions of corruption" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Positive affect" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Negative affect" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )







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
    ( Tuple.first closeExtent - extension |> max -5
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
         -- labels    
        , g[] [text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 200] [ text ("Western Europe")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 200] [ text ("Central and Eastern Europe")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 350] [ text ("North America and ANZ")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 350] [ text ("Latin America and Caribbean")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 500] [ text ("Sub-Saharan Africa")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 500] [ text ("East Asia")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 100] [ text ("Southeast Asia")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 100] [ text ("South Asia")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 610] [ text ("Middle East and North Africa")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 610] [ text ("Commonwealth of Independent States")]
                ]    
         -- colors        
        , g[class [ "southeastasia" ]][rect  [ x 85 , y 3, width 8, height 8][]]
        , g[class [ "southasia" ]][rect  [ x 85 , y 18, width 8, height 8][]]
        , g[class [ "westerneurope" ]][rect  [ x 185 , y 3, width 8, height 8][]]       
        , g[class [ "centralandeasterneurope" ]][rect  [ x 185 , y 18, width 8, height 8][]]
        , g[class [ "northamericaandanz" ]][rect  [ x 335 , y 3, width 8, height 8][]]    
        , g[class [ "latinamericaandcaribbean" ]][rect  [ x 335 , y 18, width 8, height 8][]]    
        , g[class [ "sub-saharanafrica" ]][rect  [ x 485 , y 3, width 8, height 8][]]    
        , g[class [ "eastasia" ]][rect  [ x 485 , y 18, width 8, height 8][]]    
        , g[class [ "middleeastandnorthafrica" ]][rect  [ x 595 , y 3, width 8, height 8][]]    
        , g[class [ "commonwealthofindependentstates" ]][rect  [ x 595 , y 18, width 8, height 8][]]    
        , g 
            [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xValues
             , text_
                [ x 300, y 30]
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
         , g 
             [transform [ Translate padding padding ] ]
                (List.map3 (point_hover xScaleLocal yScaleLocal) descriptions regions xyPoints)      
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
                ]
                 
point_hover : ContinuousScale Float -> ContinuousScale Float -> String -> String -> (Float, Float)  -> Svg msg
point_hover scaleX scaleY description region_id xyPoint   =
        g [ class [ "ontop" ], fontSize <| Px 12.0, fontFamily [ "Helvetica", "sans-serif" ], fontWeight FontWeightBold ]       
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
    .ontop circle { fill: rgba(0, 0, 0, 0);}
    .ontop text { display: none; }
    .ontop:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(256, 256, 256, 0.5); }
    .ontop:hover text { display: inline; font-family:  sans-serif;}

    .westerneurope circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(21, 111, 187, 1); }
    .westerneurope rect { stroke: rgba(0, 0, 0,0.4); fill: rgba(21, 111, 187, 1); }
    .westerneurope text { display: none; }

    .northamericaandanz circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 153, 51); }
    .northamericaandanz rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 153, 51); }
    .northamericaandanz text { display: none; }

    .middleeastandnorthafrica circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 153, 0); }
    .middleeastandnorthafrica rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 153, 0); }
    .middleeastandnorthafrica text { display: none; }

    .latinamericaandcaribbean circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(204, 0, 0); }
    .latinamericaandcaribbean rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(204, 0, 0); }
    .latinamericaandcaribbean text { display: none; }
    
    .centralandeasterneurope circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 204, 255); }
    .centralandeasterneurope rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 204, 255); }
    .centralandeasterneurope text { display: none; }

    .eastasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(153, 51, 255); }
    .eastasia rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(153, 51, 255); }
    .eastasia text { display: none; }

    .southeastasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(230, 230, 230); }
    .southeastasia rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(230, 230, 230); }
    .southeastasia text { display: none; }

    .commonwealthofindependentstates circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 255, 102); }
    .commonwealthofindependentstates rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 255, 102); }
    .commonwealthofindependentstates text { display: none; }

    .sub-saharanafrica circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 102, 204); }
    .sub-saharanafrica rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 102, 204); }
    .sub-saharanafrica text { display: none; }

    .southasia circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(51, 51, 204); }
    .southasia rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(51, 51, 204); }
    .southasia text { display: none; }

    """