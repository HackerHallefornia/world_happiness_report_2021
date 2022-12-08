module Main exposing (..)
import Displaydata exposing(..)
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
import Html exposing ( Html, Attribute, main_,  span, a, p, img ,br, text, strong, option, small, input, i )
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class )
import Html.Events exposing (onClick)
import Json.decode
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
        -- , myFluidContainer
        , exampleHero
        -- , dropDown
        , (viewHappiness fullText)
        -- , exampleTiles
        -- , exampleLevel
        --, exampleElementsAndComponents
        , myfooter
        ]
        
viewHappiness : WorldHappData -> Html Msg
viewHappiness ls = 
  let
    df = ls.data
    lengt = String.fromInt (List.length ls.data)
    frst_elment = Maybe.withDefault emptyCountry (List.head ls.data)
    x_values : List Float
    x_values = List.map (get_float_att "ladder_score") df
    y_values : List Float
    y_values = List.map (get_float_att "life_expectancy") df

    scat_desc : List String
    scat_desc = List.map (get_str_att "country_name") df
  in     
    container []
        [ --p [] [text lengt], 
          --p [] [text (get_str_att "country_name" frst_elment)], 
          scatterplot scat_desc x_values y_values "Ladder Score" "Life expectancy"]


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
          (Success <| {data = (csvString_to_data data)}, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)
    -- Changetext ->
    --     (Success <| { data = "Here is some text"}, Cmd.none)   

type alias WorldHappData = 
    { data : List Country_2021
    -- , configscatter
    }

type Model 
  =  Failure
  | Loading
  | Success WorldHappData

type Msg
  = GotText (Result Http.Error String)
  -- | Changetext 

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/HackerHallefornia/world_happiness_report_2021/main/data/world-happiness-report-2021_simple.csv"
        , expect = Http.expectString <| GotText
        }

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

exampleColumns : Html msg
exampleColumns 
  = section NotSpaced []
    [ container []
      [ columns columnsModifiers []
        [ column columnModifiers [] [ text "First Column" ]
        , column columnModifiers [] [ text "Second Column" ]
        , column columnModifiers [] [ text "Third Column" ]
        ]
      ]
    ]

dropDown : Html Msg
dropDown =  section NotSpaced []
    [ container []
          [ dropdown True dropdownModifiers []
            [ dropdownTrigger []
              [ button buttonModifiers 
                [ attribute "aria-haspopup" "true"
                , attribute "aria-controls" "dropdown-menu"
                ]
                [ text "Dropdown button"
                ]
              ]
            , dropdownMenu [] []
              [ dropdownItem     False [] [ text "Dropdown item"        ]
              , dropdownItemLink False [] [ text "Other dropdown item"  ]
              , dropdownItemLink True  [] [ text "Active dropdown item" ]
              , dropdownItemLink False [] [ text "Other item"           ]
              , dropdownDivider        [] [                             ]
              , dropdownItemLink False [] [ text "With a divider"       ]
              ]
            ]
          ] 
          ]

myColumnModifiers : Width -> Maybe Width -> ColumnModifiers
myColumnModifiers offset width
  = let widths : Devices (Maybe Width)
        widths = columnModifiers.widths
    in { columnModifiers
         | offset
           = offset
         , widths
           = { widths
               | tablet     = width
               , desktop    = width
               , widescreen = width
               , fullHD     = width
             }
       }


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
      