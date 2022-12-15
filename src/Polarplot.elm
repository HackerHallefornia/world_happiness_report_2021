
module Polarplot exposing (..)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, ellipse, polygon, g, line, style, svg, text_)
import TypedSvg.Attributes exposing (class, dy, textAnchor, transform, viewBox, points)
import TypedSvg.Attributes.InPx exposing (fontSize, r, x,x1, x2,y1,y2, y, cx, cy, rx,ry)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), em)

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
            , text_ [ fontSize 12, y 20 ] [ text ("Ladder rank: "++(String.fromFloat ladderscore))]
            ]
            
            --- drawing the Plot and descriptions
        , g [ transform [ Translate (wp / 2 + mainRadius) (hp / 2) ] ]
            [ List.map drawPlot [20,40,60,80,100] |> g [ class [ "r", "axis" ] ]
            , Statistics.range 0 360 60
                |> List.map2 spoke ["Log GDP per Capita","Social Support", 
                                    "Life expectancy","Freedom","Generosity","perceived corruption"]
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
    Scale.linear ( 0, mainRadius ) ( 0, 20),  --Logged Gdp
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


idToCountryPolar : Int -> String
idToCountryPolar id = 
      case id of
        1 -> "Afghanistan"
        2 -> "Albania"
        3 -> "Algeria"
        4 -> "Argentina"
        5 -> "Armenia"
        6 -> "Australia"
        7 -> "Austria"
        8 -> "Azerbaijan"
        9 -> "Bahrain"
        10 -> "Bangladesh"
        11 -> "Belarus"
        12 -> "Belgium"
        13 -> "Benin"
        14 -> "Bolivia"
        15 -> "Bosnia and Herzegovina"
        16 -> "Botswana"
        17 -> "Brazil"
        18 -> "Bulgaria"
        19 -> "Burkina Faso"
        20 -> "Burundi"
        21 -> "Cambodia"
        22 -> "Cameroon"
        23 -> "Canada"
        24 -> "Chad"
        25 -> "Chile"
        26 -> "China"
        27 -> "Colombia"
        28 -> "Comoros"
        29 -> "Congo (Brazzaville)"
        30 -> "Costa Rica"
        31 -> "Croatia"
        32 -> "Cyprus"
        33 -> "Czech Republic"
        34 -> "Denmark"
        35 -> "Dominican Republic"
        36 -> "Ecuador"
        37 -> "Egypt"
        38 -> "El Salvador"
        39 -> "Estonia"
        40 -> "Ethiopia"
        41 -> "Finland"
        42 -> "France"
        43 -> "Gabon"
        44 -> "Gambia"
        45 -> "Georgia"
        46 -> "Germany"
        47 -> "Ghana"
        48 -> "Greece"
        49 -> "Guatemala"
        50 -> "Guinea"
        51 -> "Haiti"
        52 -> "Honduras"
        53 -> "Hong Kong S.A.R. of China"
        54 -> "Hungary"
        55 -> "Iceland"
        56 -> "India"
        57 -> "Indonesia"
        58 -> "Iran"
        59 -> "Iraq"
        60 -> "Ireland"
        61 -> "Israel"
        62 -> "Italy"
        63 -> "Ivory Coast"
        64 -> "Jamaica"
        65 -> "Japan"
        66 -> "Jordan"
        67 -> "Kazakhstan"
        68 -> "Kenya"
        69 -> "Kosovo"
        70 -> "Kuwait"
        71 -> "Kyrgyzstan"
        72 -> "Laos"
        73 -> "Latvia"
        74 -> "Lebanon"
        75 -> "Lesotho"
        76 -> "Liberia"
        77 -> "Libya"
        78 -> "Lithuania"
        79 -> "Luxembourg"
        80 -> "Madagascar"
        81 -> "Malawi"
        82 -> "Malaysia"
        83 -> "Maldives"
        84 -> "Mali"
        85 -> "Malta"
        86 -> "Mauritania"
        87 -> "Mauritius"
        88 -> "Mexico"
        89 -> "Moldova"
        90 -> "Mongolia"
        91 -> "Montenegro"
        92 -> "Morocco"
        93 -> "Mozambique"
        94 -> "Myanmar"
        95 -> "Namibia"
        96 -> "Nepal"
        97 -> "Netherlands"
        98 -> "New Zealand"
        99 -> "Nicaragua"
        100 -> "Niger"
        101 -> "Nigeria"
        102 -> "North Cyprus"
        103 -> "North Macedonia"
        104 -> "Norway"
        105 -> "Pakistan"
        106 -> "Palestinian Territories"
        107 -> "Panama"
        108 -> "Paraguay"
        109 -> "Peru"
        110 -> "Philippines"
        111 -> "Poland"
        112 -> "Portugal"
        113 -> "Romania"
        114 -> "Russia"
        115 -> "Rwanda"
        116 -> "Saudi Arabia"
        117 -> "Senegal"
        118 -> "Serbia"
        119 -> "Sierra Leone"
        120 -> "Singapore"
        121 -> "Slovakia"
        122 -> "Slovenia"
        123 -> "South Africa"
        124 -> "South Korea"
        125 -> "Spain"
        126 -> "Sri Lanka"
        127 -> "Swaziland"
        128 -> "Sweden"
        129 -> "Switzerland"
        130 -> "Taiwan Province of China"
        131 -> "Tajikistan"
        132 -> "Tanzania"
        133 -> "Thailand"
        134 -> "Togo"
        135 -> "Tunisia"
        136 -> "Turkey"
        137 -> "Turkmenistan"
        138 -> "Uganda"
        139 -> "Ukraine"
        140 -> "United Arab Emirates"
        141 -> "United Kingdom"
        142 -> "United States"
        143 -> "Uruguay"
        144 -> "Uzbekistan"
        145 -> "Venezuela"
        146 -> "Vietnam"
        147 -> "Yemen"
        148 -> "Zambia"
        149 -> "Zimbabwe"
        _  -> " "

countryPolarToid : String -> String
countryPolarToid name = 
    case name of
        "Afghanistan" -> "1"
        "Albania" -> "2"
        "Algeria" -> "3"
        "Argentina" -> "4"
        "Armenia" -> "5"
        "Australia" -> "6"
        "Austria" -> "7"
        "Azerbaijan" -> "8"
        "Bahrain" -> "9"
        "Bangladesh" -> "10"
        "Belarus" -> "11"
        "Belgium" -> "12"
        "Benin" -> "13"
        "Bolivia" -> "14"
        "Bosnia and Herzegovina" -> "15"
        "Botswana" -> "16"
        "Brazil" -> "17"
        "Bulgaria" -> "18"
        "Burkina Faso" -> "19"
        "Burundi" -> "20"
        "Cambodia" -> "21"
        "Cameroon" -> "22"
        "Canada" -> "23"
        "Chad" -> "24"
        "Chile" -> "25"
        "China" -> "26"
        "Colombia" -> "27"
        "Comoros" -> "28"
        "Congo (Brazzaville)" -> "29"
        "Costa Rica" -> "30"
        "Croatia" -> "31"
        "Cyprus" -> "32"
        "Czech Republic" -> "33"
        "Denmark" -> "34"
        "Dominican Republic" -> "35"
        "Ecuador" -> "36"
        "Egypt" -> "37"
        "El Salvador" -> "38"
        "Estonia" -> "39"
        "Ethiopia" -> "40"
        "Finland" -> "41"
        "France" -> "42"
        "Gabon" -> "43"
        "Gambia" -> "44"
        "Georgia" -> "45"
        "Germany" -> "46"
        "Ghana" -> "47"
        "Greece" -> "48"
        "Guatemala" -> "49"
        "Guinea" -> "50"
        "Haiti" -> "51"
        "Honduras" -> "52"
        "Hong Kong S.A.R. of China" -> "53"
        "Hungary" -> "54"
        "Iceland" -> "55"
        "India" -> "56"
        "Indonesia" -> "57"
        "Iran" -> "58"
        "Iraq" -> "59"
        "Ireland" -> "60"
        "Israel" -> "61"
        "Italy" -> "62"
        "Ivory Coast" -> "63"
        "Jamaica" -> "64"
        "Japan" -> "65"
        "Jordan" -> "66"
        "Kazakhstan" -> "67"
        "Kenya" -> "68"
        "Kosovo" -> "69"
        "Kuwait" -> "70"
        "Kyrgyzstan" -> "71"
        "Laos" -> "72"
        "Latvia" -> "73"
        "Lebanon" -> "74"
        "Lesotho" -> "75"
        "Liberia" -> "76"
        "Libya" -> "77"
        "Lithuania" -> "78"
        "Luxembourg" -> "79"
        "Madagascar" -> "80"
        "Malawi" -> "81"
        "Malaysia" -> "82"
        "Maldives" -> "83"
        "Mali" -> "84"
        "Malta" -> "85"
        "Mauritania" -> "86"
        "Mauritius" -> "87"
        "Mexico" -> "88"
        "Moldova" -> "89"
        "Mongolia" -> "90"
        "Montenegro" -> "91"
        "Morocco" -> "92"
        "Mozambique" -> "93"
        "Myanmar" -> "94"
        "Namibia" -> "95"
        "Nepal" -> "96"
        "Netherlands" -> "97"
        "New Zealand" -> "98"
        "Nicaragua" -> "99"
        "Niger" -> "100"
        "Nigeria" -> "101"
        "North Cyprus" -> "102"
        "North Macedonia" -> "103"
        "Norway" -> "104"
        "Pakistan" -> "105"
        "Palestinian Territories" -> "106"
        "Panama" -> "107"
        "Paraguay" -> "108"
        "Peru" -> "109"
        "Philippines" -> "110"
        "Poland" -> "111"
        "Portugal" -> "112"
        "Romania" -> "113"
        "Russia" -> "114"
        "Rwanda" -> "115"
        "Saudi Arabia" -> "116"
        "Senegal" -> "117"
        "Serbia" -> "118"
        "Sierra Leone" -> "119"
        "Singapore" -> "120"
        "Slovakia" -> "121"
        "Slovenia" -> "122"
        "South Africa" -> "123"
        "South Korea" -> "124"
        "Spain" -> "125"
        "Sri Lanka" -> "126"
        "Swaziland" -> "127"
        "Sweden" -> "128"
        "Switzerland" -> "129"
        "Taiwan Province of China" -> "130"
        "Tajikistan" -> "131"
        "Tanzania" -> "132"
        "Thailand" -> "133"
        "Togo" -> "134"
        "Tunisia" -> "135"
        "Turkey" -> "136"
        "Turkmenistan" -> "137"
        "Uganda" -> "138"
        "Ukraine" -> "139"
        "United Arab Emirates" -> "140"
        "United Kingdom" -> "141"
        "United States" -> "142"
        "Uruguay" -> "143"
        "Uzbekistan" -> "144"
        "Venezuela" -> "145"
        "Vietnam" -> "146"
        "Yemen" -> "147"
        "Zambia" -> "148"
        "Zimbabwe" -> "149"
        _ -> " "
