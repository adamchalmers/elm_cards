module Cards where

import Random
import Array

-- DATA
type alias Card = {face: Face, suit: Suit}
type Suit = Clubs | Spades | Diamonds | Hearts
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type alias Deck = Array.Array Card

intToFace : Int -> Face
intToFace n =
    case n of
        0 -> Ace
        1 -> Two
        2 -> Three
        3 -> Four
        4 -> Five
        5 -> Six
        6 -> Seven
        7 -> Eight
        8 -> Nine
        9 -> Ten
        10 -> Jack
        11 -> Queen
        _ -> King
        
intToSuit : Int -> Suit
intToSuit n =
    case n of
        0 -> Clubs
        1 -> Spades
        2 -> Diamonds
        _ -> Hearts

genFace : Random.Generator Face
genFace = Random.map intToFace (Random.int 0 12)
        
genSuit : Random.Generator Suit
genSuit = Random.map intToSuit (Random.int 0 3)
        
-- CARD OPERATIONS

genCard : Random.Generator Card
genCard = Random.map2 (\f s -> {face = f, suit = s}) genFace genSuit

intToCard : Int -> Card
intToCard n = 
    let
        q = n // 13
        r = n `rem` 13
    in
        {face = intToFace r, suit = intToSuit q}
        
-- DECK OPERATIONS

sortedDeck : Deck
sortedDeck = List.map intToCard [0..51] |> Array.fromList

-- rndFrom removes a random card from the deck
rndFrom : Random.Seed -> Deck -> (Maybe Card, Deck, Random.Seed)
rndFrom seed deck =
    let
        n = (Array.length deck) - 1
        (i, s0) = Random.generate (Random.int 0 n) seed
        card = Array.get i deck
        before = Array.slice 0 i deck 
        after = Array.slice (i+1) (Array.length deck) deck
    in 
        (card, Array.append before after, s0)