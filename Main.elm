import Graphics.Element exposing (show)
import Cards exposing (..)
import Drawing exposing (..)
import Random
import Graphics.Collage as C

fst (a,b,c) = a
seed = Random.initialSeed 98447
main = 
    let
        ace = {face = Cards.Ace, suit = Cards.Spades}
        card = Maybe.withDefault ace (rndFrom seed sortedDeck |> fst)
    in
        C.collage 400 400 [drawCard ace]