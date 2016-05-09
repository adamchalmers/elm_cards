module Drawing (row, grid, drawCard) where

import Cards
import Color
import Graphics.Collage as C
import Text

-- Draws a deck of cards in a row
row : Cards.Deck -> C.Form
row deck = 
    let
        nudge = \i card -> C.move (toFloat ((width+gap)*i), 0) (drawCard card)
    in
        List.indexedMap nudge deck |> C.group

-- Draws a deck of cards in a grid, with a maximum of \cols columns
grid : Cards.Deck -> Int -> C.Form
grid deck cols =
    let
        fn = \i card -> C.move (layoutGrid i cols gap) (drawCard card)
    in
        (List.indexedMap fn deck) |> C.group

layoutGrid : Int -> Int -> Int -> (Float, Float)
layoutGrid i cols gap =
    let 
        q = i // cols
        r = i `rem` cols
        x = (width+gap)*r
        y = (height+gap)*q
    in
        (toFloat x, toFloat y)

drawCard : Cards.Card -> C.Form
drawCard card =
    let
        col = case card.suit of
            Cards.Diamonds -> Color.red
            Cards.Hearts -> Color.red
            Cards.Clubs -> Color.black
            Cards.Spades -> Color.black
    in
        C.group
            [ C.rect width height |> C.filled Color.black
            , C.rect (width-2) (height-2) |> C.filled Color.white
            , drawSuit card.suit
            , drawFace col card.face |> C.move (-width/3, height/3)
            ]


drawFace : Color.Color -> Cards.Face -> C.Form
drawFace col face =
    Text.color col (Text.fromString (faceToString face)) |> C.text


drawSuit : Cards.Suit -> C.Form
drawSuit suit =
    let
        w = width/3
        w' = w/2
    in
        case suit of
            Cards.Hearts ->
                C.group
                    [ red (C.oval w w) |> C.move (-w', 0)
                    , red (C.oval w w) |> C.move (w', 0) 
                    , red (C.ngon 3 w) |> C.rotate (pi / -2) |> C.move (0,-w')  
                    ]
            Cards.Clubs ->
                C.group
                    [ blk (C.oval w w) |> C.move (-w', 0)
                    , blk (C.oval w w) |> C.move (w', 0) 
                    , blk (C.oval w w) |> C.move (0, 3*w/4) 
                    , blk (C.ngon 3 w') |> C.rotate (pi / 2) |> C.move (0,-3*w/4)  
                    ]
            Cards.Spades ->
                C.group
                    [ blk (C.ngon 3 w) |> C.rotate (pi/2) |> C.move (0, w') -- top tri 
                    , blk (C.oval w w) |> C.move (-w', 0) -- left circ
                    , blk (C.oval w w) |> C.move (w', 0) -- right circ
                    , blk (C.ngon 3 w') |> C.rotate (pi / 2) |> C.move (0,w*(-0.6)) -- bottom tri
                    ]
            Cards.Diamonds ->
                red (C.rect w w) |> C.rotate (pi/4)
            
faceToString : Cards.Face -> String
faceToString face = 
    case face of
        Cards.Ace -> "A"
        Cards.Two -> "2"
        Cards.Three -> "3"
        Cards.Four -> "4"
        Cards.Five -> "5"
        Cards.Six -> "6"
        Cards.Seven -> "7"
        Cards.Eight -> "8"
        Cards.Nine -> "9"
        Cards.Ten -> "10"
        Cards.Jack -> "J"
        Cards.Queen -> "Q"
        Cards.King -> "K"

red = C.filled Color.red
blk = C.filled Color.black
(width, height, gap) = (40, 70, 5)
