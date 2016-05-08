import Graphics.Element exposing (show, flow, down, Element)
import Cards exposing (..)
import Rules
import Drawing exposing (..)
import Random
import Graphics.Collage as C

seed = Random.initialSeed 98447

playersDeck : Cards.Deck
playersDeck = 
    let
        ace = {face = Cards.Ace, suit = Cards.Hearts}
        two = {face = Cards.Two, suit = Cards.Clubs}
        kin = {face = Cards.Ace, suit = Cards.Spades}
        six = {face = Cards.Six, suit = Cards.Diamonds}
    in
        [ace, two, kin, six]
        
gui : Cards.Deck -> Element
gui deck =
    let
        (points, aces) = Rules.pointsAces playersDeck
    in
        flow down 
            [ C.collage 400 400 [drawGrid playersDeck 10 2]
            , show (toString points ++ " points, " ++ toString aces ++ " ace(s)")
            , show ("Hands: " ++ toString (Rules.hands playersDeck))
            ]
        
main = gui playersDeck
    