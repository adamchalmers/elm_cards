module Cards where

import Random

-- DATA
type alias Card = {face: Face, suit: Suit}
type Suit = Clubs | Spades | Diamonds | Hearts
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type alias Deck = List Card

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
        12 -> King
        _ -> Debug.crash "There's no 14th card face"
        
intToSuit : Int -> Suit
intToSuit n =
    case n of
        0 -> Clubs
        1 -> Spades
        2 -> Diamonds
        3 -> Hearts
        _ -> Debug.crash "There's no 5th card suit"

faceToString : Face -> String
faceToString face = case face of
    Ace -> "A"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"

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
sortedDeck = List.map intToCard [0..51]
