module Drawing exposing (elemRow, drawFaceup, drawFacedown)

import Cards
import Color
import Collage as C
import Element exposing (Element)
import Text

-- CONSTANTS
red = C.filled Color.red
blk = C.filled Color.black
(width, height, gap) = (40, 70, 5)


-- Draws a deck of cards as a list of DOM elements, plus a null element to ensure the DOM has the right height for an empty list.
elemRow : Int -> Cards.Deck -> List Element
elemRow numHidden deck =
    let
        collage = C.collage (width+gap) (height+gap)
        drawFn i card = collage [if i < numHidden then drawFacedown else (drawFaceup card)]
        nullElement = collage [] -- to ensure it's the right size
    in
        List.indexedMap drawFn deck ++ [nullElement]

-- Draws a face down card with a nice tiled pattern.
drawFacedown : C.Form
drawFacedown =
    let
        border = 2
        numTiles = 100 -- make this high enough to properly tile the entire card area
        cols = 4 -- enough columns to properly tile the entire card width
        radius = 5 -- size of each tile shape
        tile n = -- function to actually produce each tile shape
            let
                x = toFloat (n `rem` cols)
                y = toFloat (n // cols)
                tileShape = C.filled Color.white (C.ngon 4 radius) -- the actual form for each tile shape
            in
                C.move (radius * 2 * x - width/2 + radius, radius * 2 * y - height/2 + radius) tileShape
        tiles = C.group (List.map tile [0..numTiles]) -- tiles to draw on top of the background
        background = C.filled Color.red (C.rect width height)
    in
        C.group <| [ C.rect width height |> C.filled Color.black
                   , C.filled Color.red (C.rect (width-border) (height-border))
                   ] ++ [tiles]

drawFaceup : Cards.Card -> C.Form
drawFaceup card =
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
