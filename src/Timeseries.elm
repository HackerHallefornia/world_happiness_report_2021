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

id_to_ctry_ts : Int -> String
id_to_ctry_ts id = 
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
        26 -> "Colombia"
        27 -> "Comoros"
        28 -> "Congo (Brazzaville)"
        29 -> "Costa Rica"
        30 -> "Croatia"
        31 -> "Cyprus"
        32 -> "Czech Republic"
        33 -> "Denmark"
        34 -> "Dominican Republic"
        35 -> "Ecuador"
        36 -> "Egypt"
        37 -> "El Salvador"
        38 -> "Estonia"
        39 -> "Ethiopia"
        40 -> "Finland"
        41 -> "France"
        42 -> "Gabon"
        43 -> "Gambia"
        44 -> "Georgia"
        45 -> "Germany"
        46 -> "Ghana"
        47 -> "Greece"
        48 -> "Guatemala"
        49 -> "Guinea"
        50 -> "Haiti"
        51 -> "Honduras"
        52 -> "Hungary"
        53 -> "Iceland"
        54 -> "India"
        55 -> "Indonesia"
        56 -> "Iran"
        57 -> "Iraq"
        58 -> "Ireland"
        59 -> "Israel"
        60 -> "Italy"
        61 -> "Ivory Coast"
        62 -> "Jamaica"
        63 -> "Japan"
        64 -> "Jordan"
        65 -> "Kazakhstan"
        66 -> "Kenya"
        67 -> "Kuwait"
        68 -> "Kyrgyzstan"
        69 -> "Laos"
        70 -> "Latvia"
        71 -> "Lebanon"
        72 -> "Lesotho"
        73 -> "Liberia"
        74 -> "Libya"
        75 -> "Lithuania"
        76 -> "Luxembourg"
        77 -> "Madagascar"
        78 -> "Malawi"
        79 -> "Malaysia"
        80 -> "Mali"
        81 -> "Malta"
        82 -> "Mauritania"
        83 -> "Mauritius"
        84 -> "Mexico"
        85 -> "Moldova"
        86 -> "Mongolia"
        87 -> "Montenegro"
        88 -> "Morocco"
        89 -> "Mozambique"
        90 -> "Myanmar"
        91 -> "Namibia"
        92 -> "Nepal"
        93 -> "Netherlands"
        94 -> "New Zealand"
        95 -> "Nicaragua"
        96 -> "Niger"
        97 -> "Nigeria"
        98 -> "North Macedonia"
        99 -> "Norway"
        100 -> "Pakistan"
        101 -> "Palestinian Territories"
        102 -> "Panama"
        103 -> "Paraguay"
        104 -> "Peru"
        105 -> "Philippines"
        106 -> "Poland"
        107 -> "Portugal"
        108 -> "Romania"
        109 -> "Russia"
        110 -> "Rwanda"
        111 -> "Saudi Arabia"
        112 -> "Senegal"
        113 -> "Serbia"
        114 -> "Sierra Leone"
        115 -> "Singapore"
        116 -> "Slovakia"
        117 -> "Slovenia"
        118 -> "South Africa"
        119 -> "South Korea"
        120 -> "Spain"
        121 -> "Sri Lanka"
        122 -> "Swaziland"
        123 -> "Sweden"
        124 -> "Switzerland"
        125 -> "Taiwan Province of China"
        126 -> "Tajikistan"
        127 -> "Tanzania"
        128 -> "Thailand"
        129 -> "Togo"
        130 -> "Tunisia"
        131 -> "Turkey"
        132 -> "Uganda"
        133 -> "Ukraine"
        134 -> "United Arab Emirates"
        135 -> "United Kingdom"
        136 -> "United States"
        137 -> "Uruguay"
        138 -> "Uzbekistan"
        139 -> "Venezuela"
        140 -> "Vietnam"
        141 -> "Yemen"
        142 -> "Zambia"
        143 -> "Zimbabwe"
        _ -> ""
    
countryTsToid : String -> String
countryTsToid name = 
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
        "Colombia" -> "26"
        "Comoros" -> "27"
        "Congo (Brazzaville)" -> "28"
        "Costa Rica" -> "29"
        "Croatia" -> "30"
        "Cyprus" -> "31"
        "Czech Republic" -> "32"
        "Denmark" -> "33"
        "Dominican Republic" -> "34"
        "Ecuador" -> "35"
        "Egypt" -> "36"
        "El Salvador" -> "37"
        "Estonia" -> "38"
        "Ethiopia" -> "39"
        "Finland" -> "40"
        "France" -> "41"
        "Gabon" -> "42"
        "Gambia" -> "43"
        "Georgia" -> "44"
        "Germany" -> "45"
        "Ghana" -> "46"
        "Greece" -> "47"
        "Guatemala" -> "48"
        "Guinea" -> "49"
        "Haiti" -> "50"
        "Honduras" -> "51"
        "Hungary" -> "52"
        "Iceland" -> "53"
        "India" -> "54"
        "Indonesia" -> "55"
        "Iran" -> "56"
        "Iraq" -> "57"
        "Ireland" -> "58"
        "Israel" -> "59"
        "Italy" -> "60"
        "Ivory Coast" -> "61"
        "Jamaica" -> "62"
        "Japan" -> "63"
        "Jordan" -> "64"
        "Kazakhstan" -> "65"
        "Kenya" -> "66"
        "Kuwait" -> "67"
        "Kyrgyzstan" -> "68"
        "Laos" -> "69"
        "Latvia" -> "70"
        "Lebanon" -> "71"
        "Lesotho" -> "72"
        "Liberia" -> "73"
        "Libya" -> "74"
        "Lithuania" -> "75"
        "Luxembourg" -> "76"
        "Madagascar" -> "77"
        "Malawi" -> "78"
        "Malaysia" -> "79"
        "Mali" -> "80"
        "Malta" -> "81"
        "Mauritania" -> "82"
        "Mauritius" -> "83"
        "Mexico" -> "84"
        "Moldova" -> "85"
        "Mongolia" -> "86"
        "Montenegro" -> "87"
        "Morocco" -> "88"
        "Mozambique" -> "89"
        "Myanmar" -> "90"
        "Namibia" -> "91"
        "Nepal" -> "92"
        "Netherlands" -> "93"
        "New Zealand" -> "94"
        "Nicaragua" -> "95"
        "Niger" -> "96"
        "Nigeria" -> "97"
        "North Macedonia" -> "98"
        "Norway" -> "99"
        "Pakistan" -> "100"
        "Palestinian Territories" -> "101"
        "Panama" -> "102"
        "Paraguay" -> "103"
        "Peru" -> "104"
        "Philippines" -> "105"
        "Poland" -> "106"
        "Portugal" -> "107"
        "Romania" -> "108"
        "Russia" -> "109"
        "Rwanda" -> "110"
        "Saudi Arabia" -> "111"
        "Senegal" -> "112"
        "Serbia" -> "113"
        "Sierra Leone" -> "114"
        "Singapore" -> "115"
        "Slovakia" -> "116"
        "Slovenia" -> "117"
        "South Africa" -> "118"
        "South Korea" -> "119"
        "Spain" -> "120"
        "Sri Lanka" -> "121"
        "Swaziland" -> "122"
        "Sweden" -> "123"
        "Switzerland" -> "124"
        "Taiwan Province of China" -> "125"
        "Tajikistan" -> "126"
        "Tanzania" -> "127"
        "Thailand" -> "128"
        "Togo" -> "129"
        "Tunisia" -> "130"
        "Turkey" -> "131"
        "Uganda" -> "132"
        "Ukraine" -> "133"
        "United Arab Emirates" -> "134"
        "United Kingdom" -> "135"
        "United States" -> "136"
        "Uruguay" -> "137"
        "Uzbekistan" -> "138"
        "Venezuela" -> "139"
        "Vietnam" -> "140"
        "Yemen" -> "141"
        "Zambia" -> "142"
        "Zimbabwe" -> "143"
        _ -> ""