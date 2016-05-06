import Graphics.Element exposing (show)
import Cards exposing (..)
import Drawing exposing (..)
import Random
import Graphics.Collage as C
import Array

fst (a,b,c) = a
seed = Random.initialSeed 98447
main = 
    let
        ace = {face = Cards.Queen, suit = Cards.Hearts}
        two = {face = Cards.Two, suit = Cards.Clubs}
        kin = {face = Cards.King, suit = Cards.Spades}
        six = {face = Cards.Six, suit = Cards.Diamonds}
        deck = Array.fromList [ace, two, kin, six]
        card = Maybe.withDefault ace (rndFrom seed sortedDeck |> fst)
    in
        C.collage 400 400 [drawCardsGap deck 5]