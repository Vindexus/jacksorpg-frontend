import Array exposing (..)
import String

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)
import Task
import Json.Decode as Json exposing (..)

type alias Model = 
  { hand : Hand
  , id : Int
  , held: Held
  , error : String
  , deals: (List Hand)
  }

type alias Suit = String

type alias CardValue = String

type alias Card = 
  { value: CardValue
  , suit: Suit 
  }

type alias Hand = List Card

type alias Held = Array Bool

type Msg
  = MorePlease
  | FetchSucceed String
  | FetchHandSucceed Hand
  | FetchDealsSucceed (List Hand)
  | FetchFail Http.Error
  | HoldCard Int
  | Deal

initModel : (Model, Cmd Msg)
initModel = 
  ({ hand = initHand
  , id = 0
  , error = ""
  , held = initHeld
  , deals = []
  }, getHand)

initHand : Hand
initHand = 
  [ { value = "3", suit = "diamond"}
  , { value = "J", suit = "spade" }
  , { value = "K", suit = "spade" }
  , { value = "7", suit = "heart" }
  , { value = "J", suit = "diamond" }
  ]

initHeld : Held
initHeld = 
  Array.repeat 5 False

heldToButtons : Held -> List (Html Msg)
heldToButtons held =
  let
    x = 1
  in
    Array.toList (Array.indexedMap (\index h ->
      --index = increment index
      button [ onClick (HoldCard index) ] [ (if h then (text "Unhold") else (text "Hold")) ]
    ) held)

handToHtml : Hand -> List (Html Msg)
handToHtml cards = 
  List.map card cards

card : Card -> Html Msg
card card = 
  div [ class ("card " ++ card.value ++ " " ++ card.suit), style [("padding", "0px 0px 1px 0")] ]
      [ div [ class "corder"]
            [ text (card.value ++ card.suit) ]
      ]

dealsToHtml : (List Hand) -> List (Html Msg)
dealsToHtml deals =
  List.map (\d ->
    div [ class "hand", style [("padding", "0px 0px 10px 0")] ]
      (handToHtml d)
    ) deals

cardToString : Card -> String
cardToString card =
  card.suit ++ "." ++ card.value

handToUrlString : Hand -> String
handToUrlString hand =
  String.join "," (List.map cardToString hand)

heldToUrlString : Held -> String
heldToUrlString held =
  String.join "," (List.map (\h ->
      if h then "1" else "0"
    ) (Array.toList held))

getHand : Cmd Msg
getHand = 
  let
    url =
      "http://localhost:3010/newhand"
  in
    Task.perform FetchFail FetchHandSucceed (Http.get (Json.at ["hand"] decodeHand) url)

getDeals : Model -> Cmd Msg
getDeals model =
  let
    --TODO: Add hand and held to the URL
    url =
      "http://localhost:3010/getdeals?hand=" ++ (handToUrlString model.hand) ++ "&held=" ++ (heldToUrlString model.held)
    in
      Task.perform FetchFail FetchDealsSucceed (Http.get decodeDeals url)

decodeCard : Json.Decoder Card
decodeCard =
  Json.object2 (\suit value ->
    { suit = suit
    , value = value }
  )
  ("suit" := Json.string)
  ("value" := Json.string)

decodeHand : Json.Decoder (List Card)
decodeHand =
  Json.list decodeCard

decodeDeals : Json.Decoder (List Hand)
decodeDeals =
  Json.at ["deals"] (Json.list decodeHand)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

toggleHoldCard : Held -> Int -> Held
toggleHoldCard held index =
  Array.set index (if Array.get index held == Just True then False else True) held 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, Cmd.none)
    FetchSucceed json ->
      ({model | error = (toString json)}, Cmd.none) --TODO: Remove this I think
    FetchHandSucceed hand ->
      ({model | hand = hand }, Cmd.none)   
    FetchDealsSucceed deals ->
      ({model | deals = deals}, Cmd.none)
    FetchFail e ->
      ({ model | error = (toString e)}, Cmd.none)
    HoldCard index ->
      ({ model | held = (toggleHoldCard model.held index)}, Cmd.none)
    Deal ->
      (model, getDeals model)


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Hi" ]
    , div [ class "error" ] [ text model.error ]
    , div [ class "hand" ]
      (handToHtml model.hand )
    , div [ class "held" ]
      (heldToButtons model.held)
    , div [ class "footer" ] [ button [ onClick Deal ] [ text "Play" ] ]
    , div [ class "deals" ] (dealsToHtml model.deals)
    ]


main =
  App.program
    { init = initModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }