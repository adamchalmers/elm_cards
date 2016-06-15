module Game exposing (..)

import Drawing
import Cards exposing (..)
import Rules

import Platform.Cmd as Cmd
import Element exposing (Element, show, flow, down, right, leftAligned, toHtml)
import Collage exposing (collage)
import Html exposing (..)
import Html.Attributes exposing (style, hidden)
import Html.Events exposing (onClick)
import Maybe
import Platform.Cmd as Cmd
import Random
import Text

-- CONSTANTS

(canvasWidth, canvasHeight, seed) = (400, 100, 345738)


-- MODEL

type alias Model = {deck: Deck, player: Deck, dealer: Deck, state: State}
type State = PlayerTurn | DealerTurn | PlayerWin | DealerWin

init : (Model, Cmd Msg)
init =
    let
        (deck, s1) = Cards.genDeck (Random.initialSeed seed)
        (facedown, deck') = (List.take 2 deck, List.drop 2 deck)
    in
        ({
        deck = deck',
        player = [],
        dealer = facedown,
        state = PlayerTurn}, Cmd.none)


-- UPDATE

type Msg = PlayerDraw | DealerDraw | PlayerPass | Restart | Noop

update : Msg -> Model -> (Model, Cmd Msg)
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
            }, Cmd.none)
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
            }, Cmd.none)
        PlayerPass ->
            ({ model | state = DealerTurn}, Cmd.none)
        Restart ->
            init
        Noop ->
            (model, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [ h1Style ] [ text "Elm Blackjack"]
    , div [ boxStyle ]
        [ toHtml (gui model)
        , p [] [ text (statusText model.state) ]
        , button [ onClick PlayerDraw, hidden (model.state /= PlayerTurn), btnStyle ] [ text "Draw" ]
        , button [ onClick PlayerPass, hidden (model.state /= PlayerTurn), btnStyle ] [ text "Pass" ]
        , button [ onClick DealerDraw, hidden (model.state /= DealerTurn), btnStyle ] [ text "Draw for dealer" ]
        , button [ onClick Restart, hidden (model.state /= PlayerWin && model.state /= DealerWin), btnStyle ] [ text "Restart" ]
        ]
    ]

gui : Model -> Element
gui model =
    let
        (numHidden, dealerScore) =
            if model.state == PlayerTurn
            then (2, Text.fromString "Dealer" |> leftAligned)
            else (0, showScore model.dealer "Dealer")
    in
        flow down
            [ flow right [dealerScore, collage canvasWidth canvasHeight [Drawing.row model.dealer numHidden]]
            , flow right [showScore model.player "Player", collage canvasWidth canvasHeight [Drawing.row model.player 0]]
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

boxStyle : Attribute msg
boxStyle = style
    [ ("border", "1px dashed gray")
    , ("width", "600px")
    , ("margin", "0 auto")
    ]

h1Style : Attribute msg
h1Style = style
    [ ("text-align", "center")
    ]

btnStyle : Attribute msg
btnStyle = style
    [ ("font-size", "1em")
    ]

-- UTIL
headOf : List a -> List a
headOf l =
    case List.head l of
        Just element -> [element]
        Nothing -> []
