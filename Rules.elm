module Rules where

import Cards
import Array     
            
-- order returns the total points and number of aces
order : Cards.Deck -> (Int, Int)
order deck =
    case deck of
        [] -> (0,0)
        [card] ->
            case card.face of
                Cards.Ace ->
                    (0,1)
                Cards.Two ->
                    (2,0)
                Cards.Three ->
                    (3,0)
                Cards.Four ->
                    (4,0)
                Cards.Five ->
                    (5,0)
                Cards.Six ->
                    (6,0)
                Cards.Seven ->
                    (7,0)
                Cards.Eight ->
                    (8,0)
                Cards.Nine ->
                    (9,0)
                Cards.Ten ->
                    (10,0)
                Cards.Jack ->
                    (10,0)
                Cards.Queen ->
                    (10,0)
                Cards.King ->
                    (10,0)
        card::rest ->
            pairAdd (order [card]) (order rest)
            
pairAdd : (Int, Int) -> (Int, Int) -> (Int, Int)
pairAdd (a,b) (c,d) = (a+c, b+d)