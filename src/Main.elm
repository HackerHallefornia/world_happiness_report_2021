module Main exposing (..)
import Displaydata exposing(..)
import Polarplot exposing(..)
import Timeseries exposing (..)
import Browser
import Bulma.CDN exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Layout exposing (..)
import Http
import Html exposing ( Html, Attribute, main_,  span, a, p, img ,br, text, strong, option, small, input, i , select, label)
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class , value)
import Html.Events exposing (onClick, on)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
-- VIEW

type alias WorldHappData = 
    { data : List Country_2021,
      ts_data : List Ts_data,
      y_axis : String,
      x_axis : String,
      polar_country : String,
      line_1 : String,
      line_2 : String,
      ts_cat : String
    }



view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      main_ []
      [ stylesheet
        , fontAwesomeCDN
        , exampleNavbar
        , headerText
        , dropDown_x
        , dropDown_y
        , (viewHappiness fullText)
        , myfooter
        ]
        
viewHappiness : WorldHappData -> Html Msg
viewHappiness ls = 
  let
    df = ls.data
    x_values : List Float
    x_values = List.map (get_float_att ls.x_axis) df
    y_values : List Float
    y_values = List.map (get_float_att ls.y_axis) df
    scat_desc : List String
    scat_desc = List.map (get_str_att "country_name") df
    country_category : List String
    country_category = List.map (get_str_att "regional_indicator") df

    p_cntry : Country_2021
    p_cntry = getcountry_by_name ls.polar_country df

    ts_data_ctry1 : List (Float, Float)
    ts_data_ctry1 = list_ts_data_to_list_float ls.ts_cat <| get_ts_data_by_countryname ls.line_1 ls.ts_data
    
    ts_data_ctry2 : List (Float, Float)
    ts_data_ctry2 = list_ts_data_to_list_float ls.ts_cat <| get_ts_data_by_countryname ls.line_2 ls.ts_data

  in     
    container []
        [ br[][]
          ,scatterplot scat_desc country_category x_values y_values ls.x_axis ls.y_axis ,
          textPolar,
          dropDown_polar,
          drawPolarplot  p_cntry.country_name p_cntry.ladder_score [p_cntry.lg_gdp_pc, 
                p_cntry.social_support,p_cntry.life_expectancy,p_cntry.freedom_lc,
                p_cntry.generosity,p_cntry.pc_corruption],
          dropDown_ts1,
          dropDown_ts2,
          dropDown_ts_cat,
          ts_plot ts_data_ctry1 ts_data_ctry2 ls.line_1 ls.line_2 ls.ts_cat
          ]


main : Program () Model Msg
main
  = Browser.element
    { init = init
    , view = view
    , subscriptions = \_ -> Sub.none
    , update = update
    }

    


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading , fetchData)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok data ->
          (Success <| {data = (csvString_to_data data), ts_data = [emptyts], 
            y_axis = "Happiness Score", x_axis ="Life expectancy", polar_country = "Germany",
            line_1 = "Germany", line_2 = "Chad", ts_cat = "Freedom to make life choices"}, fetchTsData)
          
        Err _ ->
          (Failure, Cmd.none) 

    GotTsdata result ->
      case result of
        Ok tsdata ->
          case model of
            Success d -> 
              (Success <| { d | ts_data = csvString_to_data_ts tsdata}, Cmd.none)
            _ -> 
              (model, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)      
    -- Changetext ->
    --     (Success <| { data = "Here is some text"}, Cmd.none)   
    Scatterplot_yaxis id ->
      case model of
        Success d -> 
            (Success <| { d | y_axis = idToAxis id}, Cmd.none)
        _ -> 
            (model, Cmd.none)
    Scatterplot_xaxis id ->
      case model of
        Success d -> 
            (Success <| { d | x_axis = idToAxis id}, Cmd.none)
        _ -> 
            (model, Cmd.none)
    Polarplot_country id ->
      case model of
        Success d -> 
            (Success <| { d | polar_country = idToCountryPolar id}, Cmd.none)
        _ -> 
            (model, Cmd.none)
    Timeseries_1 id ->
      case model of
        Success d -> 
            (Success <| { d | line_1 = id_to_ctry_ts id}, Cmd.none)
        _ -> 
            (model, Cmd.none)
    Timeseries_2 id ->
      case model of
        Success d -> 
            (Success <| { d | line_2 = id_to_ctry_ts id}, Cmd.none)
        _ -> 
            (model, Cmd.none)
    Timeseries_cat id ->
      case model of
        Success d -> 
            (Success <| { d | ts_cat = id_to_ts_category id}, Cmd.none)
        _ -> 
            (model, Cmd.none)   




type Model 
  =  Failure
  | Loading
  | Success WorldHappData

type Msg
  = GotText (Result Http.Error String)
  | GotTsdata (Result Http.Error String)
  | Scatterplot_yaxis Int
  | Scatterplot_xaxis Int
  | Polarplot_country Int
  | Timeseries_1 Int
  | Timeseries_2 Int
  | Timeseries_cat Int
fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report-2021_simple.csv"
        , expect = Http.expectString <| GotText
        }
-- 
fetchTsData : Cmd Msg
fetchTsData =
    Http.get
        { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report_ts.csv"
        , expect = Http.expectString <| GotTsdata
        }
--- dropdown function
dropDown_y : Html Msg
dropDown_y =
  container []
    [ text "Y Axis   "
     ,select
        [ on "change" (Json.map Scatterplot_yaxis targetValueIntParse)
        ]
         (List.map(\axisname -> option[value (axisname_to_id axisname)][ Html.text axisname]) axislist)
      , br[] [ ]
    ]
dropDown_x : Html Msg
dropDown_x =
  container []
    [ text "X Axis   "
      ,select
        [ on "change" (Json.map Scatterplot_xaxis targetValueIntParse)
        ]
         (List.map(\axisname -> option[value (axisname_to_id axisname)][ Html.text axisname]) axislist)

    ]
-- , selected (-- == )
dropDown_polar : Html Msg
dropDown_polar =
  container []
    [ select
        [ on "change" (Json.map Polarplot_country targetValueIntParse)
        ]
         (List.map(\ctry -> option[value (countryPolarToid ctry)][ Html.text ctry]) polarlist)
    ]

dropDown_ts1 : Html Msg
dropDown_ts1 =
  container []
    [ select
        [ on "change" (Json.map Timeseries_1 targetValueIntParse)
        ]
         (List.map(\ctry -> option[value (countryTsToid ctry)][ Html.text ctry]) ts_list)
    ]
dropDown_ts2 : Html Msg
dropDown_ts2 =
  container []
    [ select
        [ on "change" (Json.map Timeseries_2 targetValueIntParse)
        ]
         (List.map(\ctry -> option[value (countryTsToid ctry)][ Html.text ctry]) ts_list)
    ]

dropDown_ts_cat : Html Msg
dropDown_ts_cat =
  container []
    [ select
        [ on "change" (Json.map Timeseries_cat targetValueIntParse)
        ]
         (List.map(\ctry -> option[value (category_ts_to_id ctry)][ Html.text ctry]) ts_category)
    ]

list_ts_data_to_list_float : String -> List Ts_data  -> List (Float, Float)
list_ts_data_to_list_float select_string tsd_list = 
    List.map (generate_ts_floats select_string) tsd_list
generate_ts_floats: String -> Ts_data -> (Float, Float)
generate_ts_floats select_string tsd =
    (tsd.year, get_tsfloat_att select_string tsd)
get_ts_data_by_countryname : String -> List Ts_data -> List Ts_data
get_ts_data_by_countryname countrystring countrylist = 
    List.filter (matches_countryname_ts countrystring) countrylist

matches_countryname_ts : String -> Ts_data -> Bool
matches_countryname_ts countrystring country =
    country.country_name == countrystring


get_tsfloat_att : String -> Ts_data -> Float
get_tsfloat_att float cntry = 
    case float of
        "Ladder Score" ->  .ladder_score cntry
        "Log GDP per capita" ->  .lg_gdp_pc cntry
        "Social Support" ->  .social_support cntry
        "Life expectancy" ->  .life_expectancy cntry
        "Freedom to make life choices" ->  .freedom_lc cntry
        "Generosity" ->  .generosity cntry
        "Perceived Corruption" ->  .pc_corruption cntry
        "Positive Affect" ->  .positive_affect cntry
        "Negative Affect" ->  .negative_affect cntry
        _ -> 0

ts_category: List String
ts_category = [
        "Ladder Score",
        "Log GDP per capita",
        "Social Support",
        "Life expectancy" ,
        "Freedom to make life choices",
        "Generosity" ,
        "Perceived Corruption",
        "Positive Affect" ,
        "Negative Affect"]

id_to_ts_category: Int -> String
id_to_ts_category id = 
    case id of 
      1 -> "Ladder Score"
      2 -> "Log GDP per capita"
      3 -> "Social Support"
      4 -> "Life expectancy" 
      5 -> "Freedom to make life choices"
      6 -> "Generosity" 
      7 -> "Perceived Corruption"
      8 -> "Positive Affect" 
      9 ->  "Negative Affect"
      _ -> ""
 
category_ts_to_id : String -> String
category_ts_to_id cat = 
    case cat of 
       "Ladder Score" -> "1"
       "Log GDP per capita" -> "2"
       "Social Support" -> "3"
       "Life expectancy" -> "4"
       "Freedom to make life choices" -> "5"
       "Generosity" -> "6"
       "Perceived Corruption" -> "7"
       "Positive Affect" -> "8"
       "Negative Affect" -> "9"
       _ -> ""       


axislist: List String
axislist = [
        "Happiness Score",
        "Logged GDP per capita",
        "Social support" ,
        "Life expectancy",
        "Freedom to make life choices" ,
        "Generosity",
        "Perceived corruption"]


axisname_to_id : String -> String
axisname_to_id name = 
    case name of
        "Happiness Score" ->  "1"
        "Logged GDP per capita" -> "5"
        "Social support" -> "6" 
        "Life expectancy" -> "7" 
        "Freedom to make life choices" -> "8"
        "Generosity" -> "9"
        "Perceived corruption" -> "10" 
        _ -> " "


idToAxis : Int -> String
idToAxis id = 
      case id of
       1 -> "Happiness Score"
       5 -> "Logged GDP per capita"
       6 -> "Social support" 
       7 -> "Life expectancy"
       8 -> "Freedom to make life choices" 
       9 -> "Generosity"
       10 -> "Perceived corruption" 
       _  -> " "

fontAwesomeCDN
  = Html.node "link"
    [ rel "stylesheet"
    , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ]
    []
        
exampleNavbar : Html Msg
exampleNavbar
  = navbar navbarModifiers []
    [ navbarBrand []
      ( navbarBurger False []
        [ span [] []
        , span [] []
        , span [] []
        ]
      )
      [ navbarItem False []
        [ img [ src "https://fondazionernestoilly.org/wp-content/uploads/2021/05/world-happiness-report-2-1024x1024.png" ] []
        ]
      ]
    , navbarMenu False []
      [ navbarStart [] 
        [ navbarItemLink False [] [ text " Project for Information Retrieval and Visualization 2022"  ]
        ]
      ]
    ]


myFluidContainer : Html msg
myFluidContainer
  = container []
    [ p [] [ text "This container fills the screen-width..." ]
    , p [] [ text "...until it hits the widescreen breakpoint." ]
    ]

myBox : Html msg
myBox 
  = box []
    [ p [] 
      [ text "I'm the box ghost!" ]
    ]

headerText : Html Msg
headerText
  = hero { heroModifiers | color = White, size = Small } []
    [ heroBody []
      [ container []
        [ title H2 [] [ text "World Happiness 2021" ]
        , span [] []
        , text topText]
        -- , Html.button [ onClick Changetext ] [ text "Click me" ]]
      ]
    ]

topText: String
topText= """
Summarising findings on World Happiness with the data from World Happiness Report 2021.
The data are until 2020, all non timeseries plots are a mean of the years 2018-2020.
You can interact with the plots via the dropdowns, once you click on them they are also searchable.
This first graph below, the scatterplot compares two selected attributes for all Countries. 
They are grouped into region by color. You can find out which country a point represents by hovering your mouse above it.

"""

textPolar : Html Msg
textPolar
  = hero { heroModifiers | color = White, size = Small } []
    [ heroBody []
      [ container []
        [ text " This following Visualization shows you all attributes for a single country on a circular plot. Each atttribute has its own axis at a different angle with its own scaling. "]
        -- , Html.button [ onClick Changetext ] [ text "Click me" ]]
      ]
    ]


myfooter : Html Msg
myfooter
  = footer []
    [ container []
      [ content Standard [ textCentered ]
        [ p []
          [ strong [] [ text "Johannes Boldt" ]
          ]
        ]
      ]
    ]
      