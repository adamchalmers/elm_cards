module Cards exposing (..)

import Random

-- DATA
type alias Card = {face: Face, suit: Suit}
type Suit = Clubs | Spades | Diamonds | Hearts
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type alias Deck = List Card
type alias TaggedCard = {card: Card, rand: Float}

-- Generates a randomly-ordered complete deck of 52 cards.
genDeck : Random.Seed -> (Deck, Random.Seed)
genDeck s0 =
    let
        -- Make a new deck, sorted Ace to King, Clubs to Hearts
        initDeck = List.map intToCard [0..51]
        -- Make a list of 52 random floats
        (randomFloats, s1) = Random.step (Random.list (List.length initDeck) (Random.float 0 1)) s0
        -- Combine the list of cards and list of floats (so each card is paired with a random float)
        taggedCards = List.map2 (\c f -> {card = c, rand = f}) initDeck randomFloats
    in
        -- Sort the combined card/float list by float, then discard float and return the cards.
        (List.map .card (List.sortBy .rand taggedCards), s1)

-- Maps integers (0-51) to standard playing cards.
intToCard : Int -> Card
intToCard n =
    let
        q = n // 13
        r = n `rem` 13
    in
        {face = intToFace r, suit = intToSuit q}

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
