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
  , plays: (List Hand)
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
  = FetchSucceed String
  | FetchHandSucceed Hand
  | FetchPlaysSucceed (List Hand)
  | FetchFail Http.Error
  | HoldCard Int
  | Deal
  | Play

initModel : (Model, Cmd Msg)
initModel = 
  ({ hand = initHand
  , id = 0
  , error = ""
  , held = initHeld
  , plays = []
  }, getHand)

initHand : Hand
initHand = 
  []

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
      button [ onClick (HoldCard index) ] [ (if h then (text "Don't Hold") else (text "Hold")) ]
    ) held)

cardsToHtml : (List Card) -> List (Html Msg)
cardsToHtml cards = 
  List.map card cards

handToHtml : Hand -> Held -> List (Html Msg)
handToHtml hand held = 
  Array.toList (Array.indexedMap (\index c ->
    div [ class "hand-card-container" ] 
        [ card c 
        , button [ onClick (HoldCard index) ] [ (if (Array.get index held) then (text "Don't Hold") else (text "Hold")) ]
        ]
  ) (Array.fromList hand))

card : Card -> Html Msg
card card = 
  div [ class ("card " ++ card.value ++ " " ++ card.suit), style [("padding", "0px 0px 1px 0")] ]
      [ div [ class "corder"]
            [ text (card.value ++ card.suit) ]
      ]

playsToHtml : (List Hand) -> List (Html Msg)
playsToHtml plays =
  List.map (\d ->
    div [ class "hand", style [("padding", "0px 0px 10px 0")] ]
      (cardsToHtml d)
    ) plays

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

getPlays : Model -> Cmd Msg
getPlays model =
  let
    --TODO: Add hand and held to the URL
    url =
      "http://localhost:3010/getplays?hand=" ++ (handToUrlString model.hand) ++ "&held=" ++ (heldToUrlString model.held)
    in
      Task.perform FetchFail FetchPlaysSucceed (Http.get decodePlays url)

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

decodePlays : Json.Decoder (List Hand)
decodePlays =
  Json.at ["plays"] (Json.list decodeHand)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

toggleHoldCard : Held -> Int -> Held
toggleHoldCard held index =
  Array.set index (if Array.get index held == Just True then False else True) held 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed json ->
      ({model | error = (toString json)}, Cmd.none) --TODO: Remove this I think
    FetchHandSucceed hand ->
      ({model | hand = hand }, Cmd.none)   
    FetchPlaysSucceed plays ->
      ({model | plays = plays}, Cmd.none)
    FetchFail e ->
      ({ model | error = (toString e)}, Cmd.none)
    HoldCard index ->
      ({ model | held = (toggleHoldCard model.held index)}, Cmd.none)
    Play ->
      (model, getPlays model)
    Deal ->
      ({model | held = initHeld, hand = initHand, plays = []}, getHand)


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Hi" ]
    , div [ class "error" ] [ text model.error ]
    , div [ class "hand" ]
      (handToHtml model.hand model.held )
    , div [ class "held" ]
      (heldToButtons model.held)
    , div [ class "footer" ] [ button [ onClick Deal ] [ text "Deal" ] ]
    , div [ class "footer" ] [ button [ onClick Play ] [ text "Play" ] ]
    , div [ class "plays" ] (playsToHtml model.plays)
    ]


main =
  App.program
    { init = initModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }