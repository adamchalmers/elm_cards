module Game where

import Drawing
import Cards exposing (..)
import Rules

import Effects exposing (Effects)
import Graphics.Element exposing (Element, show, flow, down, leftAligned)
import Graphics.Collage exposing (collage)
import Html exposing (..)
import Html.Attributes exposing (style, hidden)
import Html.Events exposing (onClick)
import Maybe
import Random
import StartApp exposing (start)
import Text
import Time exposing (Time)

-- MODEL 

(tableWidth, tableHeight, seed) = (400, 100, 345738)

type alias Model = {deck: Deck, player: Deck, dealer: Deck, state: State}
type State = PlayerTurn | DealerTurn | PlayerWin | DealerWin

init : (Model, Effects Action)
init = 
    let
        (deck, s1) = Cards.genDeck (Random.initialSeed seed)
    in
        ({
        deck = deck,
        player = [], 
        dealer = [],
        state = PlayerTurn}, Effects.none)

-- UPDATE

type Action = PlayerDraw | DealerDraw | PlayerPass | Noop

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        PlayerDraw ->
            ({ model
            | deck = Maybe.withDefault [] (List.tail model.deck)
            , player = model.player ++ (headOf model.deck)
            , state = 
                if Rules.score (model.player ++ (headOf model.deck)) < 21
                then PlayerTurn
                else if Rules.score (model.player ++ (headOf model.deck)) == 21
                then PlayerWin
                else DealerWin
            }, Effects.none)
        DealerDraw ->
            ({ model
            | deck = Maybe.withDefault [] (List.tail model.deck)
            , dealer = model.dealer ++ (headOf model.deck)
            , state = 
                if Rules.score (model.dealer ++ (headOf model.deck)) > 21
                then PlayerWin
                else if Rules.score (model.dealer ++ (headOf model.deck)) <= 21 && 
                    Rules.score (model.dealer ++ (headOf model.deck)) < Rules.score model.player
                then DealerTurn
                else DealerWin
            }, Effects.none)
        PlayerPass ->
            ({ model | state = DealerTurn}, Effects.none)
        Noop ->
            (model, Effects.none)
            
-- VIEW
view : Signal.Address Action -> Model -> Html
view address model = 
  div [ boxStyle]
    [ h1 [ h1Style ] [ text "Elm Blackjack"]
    , button [ onClick address PlayerDraw, hidden (model.state /= PlayerTurn) ] [ text "Draw for player" ]
    , button [ onClick address PlayerPass, hidden (model.state /= PlayerTurn) ] [ text "Pass" ]
    , button [ onClick address DealerDraw, hidden (model.state /= DealerTurn) ] [ text "Draw for dealer" ]
    , fromElement (gui model)
    , h1 [ h1Style ] [ text (statusText model.state) ]
    ]
    
gui : Model -> Element
gui model =
    flow down 
        [ showScore model.player "Player"
        , collage tableWidth tableHeight [Drawing.row model.player]
        , showScore model.dealer "Dealer"
        , collage tableWidth tableHeight [Drawing.row model.dealer]
        ]
        
showScore : Cards.Deck -> String -> Element
showScore deck name = 
    Text.fromString (name ++ " (" ++ (Rules.score deck |> toString) ++ ")") |> leftAligned
    
statusText : State -> String
statusText state =
    case state of
        PlayerTurn -> "It's your turn"
        DealerTurn -> "It's the dealer's turn"
        PlayerWin -> "You won!"
        DealerWin -> "You lose!"
        
boxStyle : Attribute
boxStyle = 
    style 
        [ ("border", "1px solid black")
        , ("width", "600px")
        , ("margin", "0 auto")
        ]
        
h1Style : Attribute
h1Style = 
    style 
        [ ("text-align", "center")
        ]
        
-- UTIL
headOf : List a -> List a
headOf l = 
    case List.head l of
        Just element -> [element]
        Nothing -> []
