module Timeseries exposing (..)
import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape

import TypedSvg exposing (g, svg, text_, style,rect )
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth, y,x, fontSize, height,width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import My_types exposing (..)


type alias Ts_scale =
    { ladder : ContinuousScale Float
    , lg_gdp : ContinuousScale Float
    , social : ContinuousScale Float
    , life : ContinuousScale Float
    , freedom : ContinuousScale Float
    , generosity : ContinuousScale Float
    , corruption : ContinuousScale Float
    , positive : ContinuousScale Float
    , negative : ContinuousScale Float
    }

get_scale : String -> Ts_scale -> ContinuousScale Float
get_scale selecter cntry = 
    case selecter of
        "Happiness Score" ->  .ladder cntry
        "Log GDP per capita" ->  .lg_gdp cntry
        "Social Support" ->  .social cntry
        "Life expectancy" ->  .life cntry
        "Freedom to make life choices" ->  .freedom cntry
        "Generosity" ->  .generosity cntry
        "Perceived Corruption" ->  .corruption cntry
        "Positive Affect" ->  .positive cntry
        "Negative Affect" ->  .negative cntry
        _ -> .ladder cntry
ts_scalesListed: Ts_scale
ts_scalesListed = {
    ladder =  Scale.linear  ( ht - 2 * paddingt, 0 ) ( 0, 9),  --ladder
    lg_gdp = Scale.linear  ( ht - 2 * paddingt, 0 ) ( 0, 12),
    social = Scale.linear ( ht - 2 * paddingt, 0 ) ( 0, 1), -- Social support
    life = Scale.linear ( ht - 2 * paddingt, 0 ) ( 30, 80), -- Life expectancy
    freedom = Scale.linear ( ht - 2 * paddingt, 0 ) ( 0, 1), -- Freedom
    generosity = Scale.linear ( ht - 2 * paddingt, 0 ) ( -1, 1), -- generosity
    corruption = Scale.linear ( ht - 2 * paddingt, 0 ) ( 0, 1), -- perceived corruption
    positive  = Scale.linear ( ht - 2 * paddingt, 0 ) ( 0.3, 1),  -- positive affect
    negative = Scale.linear ( ht - 2 * paddingt, 0 ) ( 0, 0.8)   -- negative affect    
    }

wt : Float
wt =
    900


ht : Float
ht =
    300


paddingt : Float
paddingt =
    40


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, wt - 2 * paddingt ) ( 2006, 2020 ) 


yScale : ContinuousScale Float
yScale =
    Scale.linear ( ht - 2 * paddingt, 0 ) ( 0, 5 )


xAxis : List ( Float, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


get_yaxis : String -> Svg msg
get_yaxis selecter = 
    Axis.left [ Axis.tickCount 5 ] (get_scale selecter ts_scalesListed)

transformToLineData :ContinuousScale Float ->   ( Float, Float )  -> Maybe ( Float, Float )
transformToLineData yscale ( x, y )  =
     Just ( Scale.convert xScale x, Scale.convert yscale y )



line : List ( Float, Float ) -> ContinuousScale Float -> Path
line data yscale =
    List.map (transformToLineData yscale) data 
        |>Shape.line  Shape.linearCurve 


ts_plot : List ( Float, Float ) -> List ( Float, Float ) -> String -> String -> String -> Svg msg
ts_plot line_1 line_2 ctry_1 ctry_2 scale_str=
    svg [ viewBox 0 0 wt ht ]
        [style [] [ text ".label {font-family:  sans-serif;}    .redline rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(256, 0, 0); } .blueline rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 0, 256); }" ]
        ,g [ class [ "label" ], transform [ Translate 10 10 ] ]
            [ text_ [ fontSize 10, y 5 , x 185] [ text ctry_1]
            , text_ [ fontSize 10, y 16 , x 185 ] [ text ctry_2]
            , text_ [ fontSize 10, y 15 ] [ text scale_str]
            , text_ [ fontSize 10, y 280, x 300 ] [ text "Year"]]
        , g[class [ "redline" ]][rect  [ x 185 , y 7, width 8, height 8][]]
        , g[class [ "blueline" ]][rect  [ x 185 , y 19, width 8, height 8][]]  
        ,g [ transform [ Translate (paddingt - 1) (ht - paddingt) ] ]
            [ xAxis line_1 ]
        , g [ transform [ Translate (paddingt - 1) paddingt ] ]
            [ get_yaxis scale_str]
        , g [ transform [ Translate paddingt paddingt ], class [ "series" ] ]
            [ Path.element (line line_1 (get_scale scale_str ts_scalesListed)) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone],
              Path.element (line line_2 (get_scale scale_str ts_scalesListed)) [ stroke <| Paint <| Color.rgb 0 0 1, strokeWidth 3, fill PaintNone]
            ]
        ]


ts_list: List String
ts_list = 
    ["Afghanistan", "Albania", "Algeria", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahrain", "Bangladesh", "Belarus", "Belgium", "Benin", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Chad", "Chile", "Colombia", "Comoros", "Congo (Brazzaville)", "Costa Rica", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia", "Ethiopia", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Guatemala", "Guinea", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Pakistan", "Palestinian Territories", "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Romania", "Russia", "Rwanda", "Saudi Arabia", "Senegal", "Serbia", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "South Africa", "South Korea", "Spain", "Sri Lanka", "Swaziland", "Sweden", "Switzerland", "Taiwan Province of China", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tunisia", "Turkey", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"]

