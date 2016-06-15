module Game exposing (..)

import Drawing
import Cards exposing (..)
import Rules

import Platform.Cmd as Cmd
import Element exposing (Element, show, flow, down, right, leftAligned, toHtml)
import Html exposing (..)
import Html.Attributes exposing (style, hidden)
import Html.Events exposing (onClick)
import Maybe
import Platform.Cmd as Cmd
import Random
import Text
import Time

-- CONSTANTS

(canvasWidth, canvasHeight, seed) = (400, 100, 345738)


-- MODEL

type alias Model =
    { deck: Deck -- Deck of cards that both player and dealer will draw from
    , player: Deck -- Cards belonging to player
    , dealer: Deck -- Cards belonging to dealer
    , state: State -- Whose turn it is, or whether someone won/lost
    , time: Time.Time -- Current time, to be used as a random seed
    , winsLosses: (Int, Int) -- How many times the player has won or lost
    }

type State = PlayerTurn | DealerTurn | PlayerWin | DealerWin

init : Time.Time -> (Int, Int) -> (Model, Cmd Msg)
init time winsLosses =
    let
        (deck, seed) = Cards.genDeck (Random.initialSeed <| round time)
        (facedown, deck') = (List.take 2 deck, List.drop 2 deck)
        model =
            { deck = deck'
            , player = []
            , dealer = facedown
            , state = PlayerTurn
            , time = time
            , winsLosses = winsLosses
            }
    in
        (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every Time.second Tick
    , Time.every Time.second DealerDraw
    ]


-- UPDATE

type Msg = PlayerDraw | DealerDraw Time.Time | PlayerPass | Restart | Tick Time.Time | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        PlayerDraw ->
            let m =
                { model
                | deck = Maybe.withDefault [] (List.tail model.deck)
                , player = model.player ++ (headOf model.deck)
                }
            in
                ({ m | state = statePlayerTurn m}, Cmd.none)

        DealerDraw _ ->
            if model.state == DealerTurn
            then
                let m =
                    { model
                    | deck = Maybe.withDefault [] (List.tail model.deck)
                    , dealer = model.dealer ++ (headOf model.deck)
                    }
                in
                    ({ m | state = stateDealerTurn m}, Cmd.none)
            else (model, Cmd.none)

        PlayerPass ->
            ({ model | state = stateDealerTurn model}, Cmd.none)

        Restart ->
            init model.time (model.winsLosses `pairAdd` (if model.state == PlayerWin then (1, 0) else (0, 1)))

        Tick t ->
            ({ model | time = t}, Cmd.none)

        Noop ->
            (model, Cmd.none)

pScore : Model -> Int
pScore m = Rules.score m.player

dScore : Model -> Int
dScore m = Rules.score m.dealer

pairAdd : ( number, number' ) -> ( number, number' ) -> ( number, number' )
pairAdd (a,b) (c,d) = (a+c, b+d)

stateDealerTurn : Model -> State
stateDealerTurn m =
    if dScore m > 21
    then PlayerWin
    else if dScore m == 21
    then DealerWin
    else if dScore m < 21 && dScore m < pScore m
    then DealerTurn
    else DealerWin

statePlayerTurn : Model -> State
statePlayerTurn m =
    if pScore m > 21
    then DealerWin
    else if pScore m == 21
    then PlayerWin
    else PlayerTurn

-- VIEW
view : Model -> Html Msg
view model =
  div [bodyStyle]
    [ h1 [ h1Style ] [ text "Elm Blackjack"]
    , div [ boxStyle ]
        [ gui model
        , p [] [ text (statusText model) ]
        , button [ onClick PlayerDraw, hidden (model.state /= PlayerTurn), btnStyle ] [ text "Draw" ]
        , button [ onClick PlayerPass, hidden (model.state /= PlayerTurn), btnStyle ] [ text "Pass" ]
        , button [ onClick Restart, hidden (model.state /= PlayerWin && model.state /= DealerWin), btnStyle ] [ text "Restart" ]
        ]
    ]

gui model =
    let
        (numHidden, dealerScore) =
            if model.state == PlayerTurn
            then (2, Text.fromString "Dealer" |> leftAligned)
            else (0, showScore model.dealer "Dealer")
    in
        Html.table []
            [ Html.tr []
              [ Html.td [] [toHtml dealerScore]
              , Html.td [] [toHtml (Drawing.elemRow numHidden model.dealer |> flow right)]
              ]
            , Html.tr []
              [ Html.td [] [toHtml <| showScore model.player "Player" ]
              , Html.td [] [toHtml (Drawing.elemRow 0 model.player |> flow right)]
              ]
            ]
        -- flow down
        --     [ dealerScore
        --     , flow right <| Drawing.elemRow numHidden model.dealer
        --     , showScore model.player "Player"
        --     , flow right <| Drawing.elemRow 0 model.player
        --     ]

showScore : Cards.Deck -> String -> Element
showScore deck name =
    Text.fromString (name ++ " (" ++ (Rules.score deck |> toString) ++ ")") |> leftAligned

statusText : Model -> String
statusText model  =
    let
        statusText = case model.state of
            PlayerTurn -> "It's your turn."
            DealerTurn -> "It's the dealer's turn."
            PlayerWin -> "You won!"
            DealerWin -> "You lose!"
        (w, l) = model.winsLosses
    in
        statusText ++ " " ++ "You've won " ++ toString w ++ " games and lost " ++ toString l ++ "."

boxStyle : Attribute msg
boxStyle = style
    [ ("border", "1px dashed gray")
    , ("width", "600px")
    , ("margin", "0 auto")
    ]

bodyStyle : Attribute msg
bodyStyle = style
    [ ("font-family", "sans-serif")
    ]

tableStyle : Attribute msg
tableStyle = style
    [ ("border-spacing", "10px 5px")
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
