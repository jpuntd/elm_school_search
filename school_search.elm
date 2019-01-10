module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Fuzzy

import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Badge as Badge


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { search : String
    , city : String
    , schools : List School
    , filteredSchools: List School
    , scoredSchools : List (Fuzzy.Result, School)
    }


type alias School =
    { name : String
    , street : String
    , pageid : String
    , city: String
    , zip: Int
    }


init : List School -> ( Model, Cmd Msg )
init flags =
    ( Model "" "BRUSSEL-STAD" flags [] []
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeSearch String
    | SchoolsFetched (Result Http.Error (List School))
    | SearchNow
    | ChangeCity String
    | Filter
   


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearch s ->
            ( {model | search = s} , Cmd.none )
        
        ChangeCity s ->
              ( {model | city = s }, Cmd.none )
                      
        Filter ->
            ( {model | filteredSchools = List.filter (\e -> String.toLower e.city == String.toLower model.city ) model.schools} , Cmd.none)     -- No filter on zipcodes
           -- ( {model | filteredSchools = model.schools}, Cmd.none)

        SchoolsFetched (Ok schools) ->
            ( {model | schools = schools} , Cmd.none )

        SchoolsFetched (Err _) ->
            ( model, Cmd.none )
        
        SearchNow ->
          
           ( {model | scoredSchools = List.sortBy (Tuple.first >> .score) (List.map (scoredSchool model.search) model.filteredSchools)} , Cmd.none )
            

scoredSchool: String -> School -> (Fuzzy.Result, School)
scoredSchool needle school = 
   (Fuzzy.match [] [" "] (String.toLower needle) (String.toLower (school.name ++ " "++ school.street)), school)  

-- VIEW
  -- 

view : Model -> Html Msg
view model =
    
    div [style [("padding","2px")]]
        [ h2 [] [ text "Zoek school" ]
        , input [ type_ "text", onInput ChangeCity ] [ ]
        , input [ type_ "text", onInput ChangeSearch, onBlur Filter ] [ ]
  --      , input [ type_ "text", onInput ]
       
        , button [onClick SearchNow] [text "Zoek"]
        , div [] (List.map viewSchool model.scoredSchools )
    
  
     
        
        ]

viewSchool : (Fuzzy.Result, School) -> Html Msg
viewSchool (result, school) = 
    div [style [("padding","10px")]]
        [ a [href ("http://www.cultuurkuur.be/agenda/g/school/" ++ school.pageid)] [text school.name]
        , Badge.badge [class "pull-right"] [text (toString result.score)]
        , div [class "d-block"] [text school.street ]
        , div [class "d-block"] [text (toString school.zip ++ " " ++ school.city) ]
        ]
      

    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getSchools :  Cmd Msg
getSchools =
    let
        url = "./schools.json"
            
    in
        Http.send SchoolsFetched (Http.get url (Decode.list decodeSchool))

decodeSchool : Decode.Decoder School
decodeSchool =
    Decode.map5 School
        (Decode.field "Naam" Decode.string)
        (Decode.field "Straat" Decode.string)
        (Decode.field "pid" Decode.string)
        (Decode.field "Gemeente" Decode.string)
        (Decode.field "Postcode" Decode.int)

