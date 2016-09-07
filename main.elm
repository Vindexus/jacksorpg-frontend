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
  | FetchFail Http.Error
  | HoldCard Int
  | Deal

initModel : (Model, Cmd Msg)
initModel = 
  ({ hand = initHand
  , id = 0
  , error = ""
  , held = initHeld
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

handToText : Hand -> Html Msg
handToText cards = 
  text "Cards to Hand"

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
  div [ class ("card " ++ card.value ++ " " ++ card.suit)]
      [ div [ class "corder"]
            [ text (card.value ++ card.suit) ]
      ]

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
      "http://localhost:3000/newhand"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeHand url)

getDeals : Model -> Cmd Msg
getDeals model =
  let
    --TODO: Add hand and held to the URL
    url =
      "http://localhost:3000/getdeals?hand=" ++ (handToUrlString model.hand) ++ "&held=" ++ (heldToUrlString model.held)
    in
      Task.perform FetchFail FetchSucceed (Http.get decodeDeals url)

decodeHand : Json.Decoder String
decodeHand =
  Json.at ["hand", "0","suit"] Json.string

decodeDeals : Json.Decoder String
decodeDeals =
  Json.at ["deals"] Json.string

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
      ({model | error = (toString json)}, Cmd.none)
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
    ]


main =
  App.program
    { init = initModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }