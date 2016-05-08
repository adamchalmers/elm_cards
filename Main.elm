import Graphics.Element exposing (show, flow, down)
import Cards exposing (..)
import Rules
import Drawing exposing (..)
import Random
import Graphics.Collage as C

seed = Random.initialSeed 98447
main = 
    let
        ace = {face = Cards.Ace, suit = Cards.Hearts}
        two = {face = Cards.Two, suit = Cards.Clubs}
        kin = {face = Cards.King, suit = Cards.Spades}
        six = {face = Cards.Six, suit = Cards.Diamonds}
        deck =  [ace, two, kin, six]
        (points, aces) = Rules.order deck
    in
        flow down 
            [ C.collage 400 400 [drawGrid deck 10 2]
            , show (toString points ++ " points, " ++ toString aces ++ " ace(s)")
            ]