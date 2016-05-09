module Cards where

import Random

-- DATA
type alias Card = {face: Face, suit: Suit}
type Suit = Clubs | Spades | Diamonds | Hearts
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type alias Deck = List Card
type alias TaggedCard = {card: Card, rand: Float}

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
        
without : Int -> List a -> List a
without i arr = 
  let before = List.take i arr
      after = List.drop (i+1) arr
  in
    before ++ after
    
        
-- DECK OPERATIONS

genDeck : Random.Seed -> (Deck, Random.Seed)
genDeck s0 =
    let
        deck = List.map intToCard [0..51]
        (tagged, s1) = tagDeck deck s0
        shuffled = List.map .card (List.sortBy .rand tagged)
    in
        (shuffled, s1)

        
tagDeck : Deck -> Random.Seed -> (List TaggedCard, Random.Seed)
tagDeck deck s0 =
    let
        (randomFloats, s1) = Random.generate (Random.list (List.length deck) (Random.float 0 1)) s0
        makeTag c f = {card = c, rand = f}
    in
        (List.map2 makeTag deck randomFloats, s1)