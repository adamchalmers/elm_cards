module Rules where

import Cards
import Array     
import Debug
            
-- pointsAces returns the total points and number of aces
pointsAces : Cards.Deck -> (Int, Int)
pointsAces deck =
    let 
        pairAdd = \(a,b) (c,d) -> (a+c, b+d)
    in
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
                pairAdd (pointsAces [card]) (pointsAces rest)


score : Cards.Deck -> Int
score deck =
    let
        (p, naces) = pointsAces deck
        options = case naces of
            0 -> (p,p)
            1 -> (p+1, p+11)
            2 -> (p+2, p+12)
            3 -> (p+3, p+13)
            4 -> (p+4, p+14)
            _ -> Debug.crash "How did you get so many aces"
    in
        options |> \(a,b) -> 
            if a > b && a <= 21 
            then a 
            else if b > a && b <= 21
                then b
                else a
