
module Polarplot exposing (..)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, ellipse, polygon, g, line, style, svg, text_, rect)
import TypedSvg.Attributes exposing (class, dy, textAnchor, transform, viewBox, points)
import TypedSvg.Attributes.InPx exposing (fontSize, r, x,x1, x2,y1,y2, y, cx, cy, rx,ry)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), em)
import My_types exposing (..)

wp : Float
wp =
    900


hp : Float
hp =
    450


padding : Float
padding =
    70


mainRadius : Float
mainRadius =
    Basics.min wp hp / 2 - padding



spoke : String -> Float -> Svg msg
spoke descript angle =
    let
        adjustedangle = angle-70
    in
     g [ transform [ Rotate (adjustedangle) 0 0 ] ]
        [ line [ x2 mainRadius ] []
        , text_
            [ x (mainRadius + 6)
            , dy (em 0.35)
            , textAnchor
                (if adjustedangle < 270 && adjustedangle > 90 then
                    AnchorEnd

                 else
                    AnchorInherit
                )
            , transform [ Rotate -adjustedangle (mainRadius + 8) 0 ]
            ]
            [ text descript]
        ]


drawPlot : Float -> Svg msg
drawPlot radius = -- actually the tick values
    g []
        [ellipse [rx <| Scale.convert radiusScale radius, ry <| Scale.convert radiusScale radius ][]
        ] 

drawPointandTicks : ContinuousScale Float-> Float -> Float -> Svg msg
drawPointandTicks radiusscale value angle  =  
    g [ transform [ Translate (wp / 2 + mainRadius) (hp / 2) ] ]
            -- Drawing the ticks for the individual scale
            [Scale.ticks radiusscale 4 -- set number of ticks
                |> List.drop 1
                |> List.map (drawTicksforAxis radiusscale angle)
                |> g [ class [ "r", "axis" ] ],
                
             -- Drawing the Point with hover value   
             g [transform [ Rotate (angle - 180) 0 0 ], class [ "r", "point" ]  ]
                [circle [cy ( Scale.convert radiusscale value), r  7][]
                , text_ [ y <| ( Scale.convert radiusscale value) - 9 , transform [Rotate -(angle - 180) 0 ( Scale.convert radiusscale value)]]
                [text (String.fromFloat value)]
                ]
            ]

drawTicksforAxis : ContinuousScale Float-> Float -> Float -> Svg msg
drawTicksforAxis scale angle radius = -- actually the tick values
    g []
        [text_ [ y <| Scale.convert scale radius* -1 + 10, transform [ Rotate angle 0 0 ], textAnchor AnchorMiddle ]
            [ radius |> Scale.tickFormat scale 10 |> text ] -- scale sets the decimal accuracy     
        ]


drawpoint : ContinuousScale Float -> Float -> Float -> Svg msg
drawpoint radiusscale value angle = -- actually the tick values
    g [transform [ Rotate angle 0 0 ] ]
        [circle [cy ( Scale.convert radiusscale value), r  7][]
        , text_ [ y <| ( Scale.convert radiusscale value) - 9 , transform [Rotate -angle 0 ( Scale.convert radiusscale value)]]
        [text (String.fromFloat value)]
        ]


drawPolarplot: String -> Float -> List Float -> Svg msg
drawPolarplot countryname ladderscore listofvalues=
    svg [ viewBox 0 0 wp hp ]
        [ style [] [ text css ]
        , g [ class [ "label" ], transform [ Translate (padding * 2) (hp / 2) ] ]
            [ text_ [ fontSize 20 ] [ text countryname]
            , text_ [ fontSize 12, y 20 ] [ text ("Happiness Score: "++(String.fromFloat ladderscore))]
            ]   
            --- drawing the Plot and descriptions
        , g [ transform [ Translate (wp / 2 + mainRadius) (hp / 2) ] ]
            [ List.map drawPlot [20,40,60,80,100] |> g [ class [ "r", "axis" ] ]
            , Statistics.range 0 360 60
                |> List.map2 spoke ["Log GDP per Capita","Social Support", 
                                    "Life expectancy","Freedom","Generosity","Perceived corruption"]
                |> g [ class [ "a", "axis" ] ]
            ]
            --- calculating the ticks and drawing them onto the plot
        , List.map3 drawPointandTicks scalesListed listofvalues anglelist |>  g[]
           -- drawing the points
        ]
        
main : Svg msg
main =
    drawPolarplot "Germany" 7.5 valuelist
        
radiusScale : ContinuousScale Float
radiusScale =
    Scale.linear ( 0, mainRadius ) ( 0, 100)

radiusScale2 : ContinuousScale Float
radiusScale2 =
    Scale.linear ( 0, mainRadius ) ( 0, 20)

scalesListed: List (ContinuousScale Float)
scalesListed = [
    Scale.linear ( 0, mainRadius ) ( 0, 12),  --Logged Gdp
    Scale.linear ( 0, mainRadius ) ( 0, 1), -- Social support
    Scale.linear ( 0, mainRadius ) ( 0, 80), -- Life expectancy
    Scale.linear ( 0, mainRadius ) ( 0, 1), -- Freedom
    Scale.linear ( 0, mainRadius ) ( -5, 5), -- generosity
    Scale.linear ( 0, mainRadius ) ( 1, 0) -- perceived corruption
    ]

valuelist: List Float
valuelist= [15, 70, 120, 35, 250, 400]

anglelist: List Float
anglelist= [20, 80, 140, 200, 260, 320]

css : String
css =
    """
    .frame {
      fill: none;
      stroke: #000;
    }
    .axis text {
      font: 10px sans-serif;
    }

    .axis line{
      stroke: #777;
      stroke-dasharray: 1,2;
    }
    
    .axis circle {
      fill: rgba(58, 139, 211, 1);
    }
    .point circle {
      fill: rgba(58, 139, 211, 1);
      
    }
    .point:hover circle {
      fill: rgba(58, 200, 211, 1);
      stroke: black;
    }
    .point text { display: none;}
    .point:hover text { 
        display: inline;
        font: 12px sans-serif;
        }


    .axis ellipse {
      fill: none;
      stroke: black;
      stroke-dasharray: 1,2;
      stroke-width: 1px;
    }
    
    .axis :last-of-type ellipse {
      fill: none;
      stroke: black;
      stroke-dasharray: none;
      stroke-width: 2px;
    }

    .axis :last-of-type circle {
      stroke: #333;
      stroke-dasharray: none;
    }

    .line {
      fill: none;
      stroke: red;
      stroke-width: 1px;
    }

    .label {
      font-family:  sans-serif;
    }
  """

polarlist: List String
polarlist = ["Afghanistan", "Albania", "Algeria", "Argentina", "Armenia",
       "Australia", "Austria", "Azerbaijan", "Bahrain", "Bangladesh",
       "Belarus", "Belgium", "Benin", "Bolivia", "Bosnia and Herzegovina",
       "Botswana", "Brazil", "Bulgaria", "Burkina Faso", "Burundi",
       "Cambodia", "Cameroon", "Canada", "Chad", "Chile", "China",
       "Colombia", "Comoros", "Congo (Brazzaville)", "Costa Rica",
       "Croatia", "Cyprus", "Czech Republic", "Denmark",
       "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia",
       "Ethiopia", "Finland", "France", "Gabon", "Gambia", "Georgia",
       "Germany", "Ghana", "Greece", "Guatemala", "Guinea", "Haiti",
       "Honduras", "Hong Kong S.A.R. of China", "Hungary", "Iceland",
       "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy",
       "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya",
       "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon",
       "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg",
       "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta",
       "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia",
       "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia",
       "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger",
       "Nigeria", "North Cyprus", "North Macedonia", "Norway", "Pakistan",
       "Palestinian Territories", "Panama", "Paraguay", "Peru",
       "Philippines", "Poland", "Portugal", "Romania", "Russia", "Rwanda",
       "Saudi Arabia", "Senegal", "Serbia", "Sierra Leone", "Singapore",
       "Slovakia", "Slovenia", "South Africa", "South Korea", "Spain",
       "Sri Lanka", "Swaziland", "Sweden", "Switzerland",
       "Taiwan Province of China", "Tajikistan", "Tanzania", "Thailand",
       "Togo", "Tunisia", "Turkey", "Turkmenistan", "Uganda", "Ukraine",
       "United Arab Emirates", "United Kingdom", "United States",
       "Uruguay", "Uzbekistan", "Venezuela", "Vietnam", "Yemen", "Zambia",
       "Zimbabwe"]


