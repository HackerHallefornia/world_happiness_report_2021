module Main exposing (..)
import Displaydata exposing(..)
import Polarplot exposing(..)
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
import Html exposing ( Html, Attribute, main_,  span, a, p, img ,br, text, strong, option, small, input, i , select)
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class , value)
import Html.Events exposing (onClick, on)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
-- VIEW
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
        , exampleHero
        , dropDown_x
        , dropDown_y
        , (viewHappiness fullText)
        , myfooter
        ]
        
viewHappiness : WorldHappData -> Html Msg
viewHappiness ls = 
  let
    df = ls.data
    lengt = String.fromInt (List.length ls.data)
    frst_elment = Maybe.withDefault emptyCountry (List.head ls.data)
    x_values : List Float
    x_values = List.map (get_float_att ls.x_axis) df
    y_values : List Float
    y_values = List.map (get_float_att ls.y_axis) df
    scat_desc : List String
    scat_desc = List.map (get_str_att "country_name") df

    p_cntry : Country_2021
    p_cntry = getcountry_by_name "Germany" df
  in     
    container []
        [ --p [] [text lengt], 
          --p [] [text (get_str_att "country_name" frst_elment)], 
          scatterplot scat_desc x_values y_values ls.x_axis ls.y_axis ,
          drawPolarplot  p_cntry.country_name p_cntry.ladder_score [p_cntry.lg_gdp_pc, 
                p_cntry.social_support,p_cntry.life_expectancy,p_cntry.freedom_lc,
                p_cntry.generosity,p_cntry.pc_corruption]]


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
          (Success <| {data = (csvString_to_data data), y_axis = "ladder_score", x_axis ="life_expectancy"}, Cmd.none)

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

type alias WorldHappData = 
    { data : List Country_2021,
      y_axis : String,
      x_axis : String
    }

type Model 
  =  Failure
  | Loading
  | Success WorldHappData

type Msg
  = GotText (Result Http.Error String)
  | Scatterplot_yaxis Int
  | Scatterplot_xaxis Int

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report-2021_simple.csv"
        , expect = Http.expectString <| GotText
        }
-- 

--- dropdown function
dropDown_y : Html Msg
dropDown_y =
  container []
    [ select
        [ on "change" (Json.map Scatterplot_yaxis targetValueIntParse)
        ]
         (List.map(\axisname -> option[value (axisname_to_id axisname)][ Html.text axisname]) axislist)
    ]
dropDown_x : Html Msg
dropDown_x =
  container []
    [ select
        [ on "change" (Json.map Scatterplot_xaxis targetValueIntParse)
        ]
         (List.map(\axisname -> option[value (axisname_to_id axisname)][ Html.text axisname]) axislist)
    ]
-- , selected (-- == )

axislist: List String
axislist = [
        "ladder_score",
        "se_ladder",
        "u_whisker",
        "l_whisker" ,
        "lg_gdp_pc",
        "social_support" ,
        "life_expectancy",
        "freedom_lc" ,
        "generosity",
        "pc_corruption"]

idToAxis : Int -> String
idToAxis id = 
      case id of
       1 -> "ladder_score"
       2-> "se_ladder"
       3 -> "u_whisker"
       4 -> "l_whisker" 
       5 -> "lg_gdp_pc"
       6 -> "social_support" 
       7 -> "life_expectancy"
       8 -> "freedom_lc" 
       9 -> "generosity"
       10 -> "pc_corruption" 
       _  -> " "

axisname_to_id : String -> String
axisname_to_id name = 
    case name of
        "ladder_score" ->  "1"
        "se_ladder" -> "2"
        "u_whisker" -> "3"
        "l_whisker" -> "4"
        "lg_gdp_pc" -> "5"
        "social_support" -> "6" 
        "life_expectancy" -> "7" 
        "freedom_lc" -> "8"
        "generosity" -> "9"
        "pc_corruption" -> "10" 
        _ -> " "


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
        [ navbarItemLink False [] [ text "Elm Visualisierung World Happiness 2021 "  ]
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

exampleHero : Html Msg
exampleHero
  = hero { heroModifiers | color = White, size = Small } []
    [ heroBody []
      [ container []
        [ title H2 [] [ text "World Happiness 2021" ]
        , text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean efficitur sit amet massa fringilla egestas. Nullam condimentum luctus turpis."]
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
      