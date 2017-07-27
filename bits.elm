module Bits exposing (..)

import Basics exposing (..)
import Bitwise exposing (..)
import List exposing (..)
import String exposing (..)


type alias Bits =
    Int


type alias BitsList =
    List Int



{-
   Logical functions
-}


rotr : Int -> Bits -> Bits
rotr n x =
    shiftRightZfBy 0 (or (shiftRightZfBy n x) (shiftLeftBy (32 - n) x))


σ1 : Bits -> Bits
σ1 b =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (Bitwise.xor (rotr 17 b) (rotr 19 b)) (shiftRightZfBy 10 b)


σ0 : Bits -> Bits
σ0 b =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (Bitwise.xor (rotr 7 b) (rotr 18 b)) (shiftRightZfBy 3 b)


s0 : Bits -> Bits
s0 b =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (Bitwise.xor (rotr 2 b) (rotr 13 b)) (rotr 22 b)


s1 : Bits -> Bits
s1 b =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (Bitwise.xor (rotr 6 b) (rotr 11 b)) (rotr 25 b)


choose : Bits -> Bits -> Bits -> Bits
choose x y z =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (and x y) (and (complement x) z)


majority : Bits -> Bits -> Bits -> Bits
majority x y z =
    Bitwise.shiftRightZfBy 0 <| Bitwise.xor (Bitwise.xor (and x y) (and x z)) (and y z)



{- Conversions -}


fromBitsToInt : Bits -> Int
fromBitsToInt x =
    x


fromIntToBits : Int -> Bits
fromIntToBits x =
    x


toBitsList : Bits -> BitsList
toBitsList b =
    if b == 0 then
        []
    else
        let
            r =
                fromBitsToInt b % 2

            rest =
                (Basics.toFloat <| fromBitsToInt b) / 2
        in
        List.append (toBitsList <| fromIntToBits <| floor rest) [ r ]


printBits : Bits -> String
printBits x =
    let
        fn a b =
            String.append b a
    in
    List.foldl fn "" (List.map toString <| toBitsList x)


toHexList : Bits -> List String
toHexList b =
    if b == 0 then
        []
    else
        let
            curr =
                b % 16

            rest =
                floor <| Basics.toFloat b / 16

            result =
                case curr of
                    10 ->
                        "A"

                    11 ->
                        "B"

                    12 ->
                        "C"

                    13 ->
                        "D"

                    14 ->
                        "E"

                    15 ->
                        "F"

                    _ ->
                        toString curr
        in
        List.append (toHexList rest) [ result ]


printHex : Bits -> String
printHex b =
    let
        fn a b =
            String.append b a
    in
    String.append "" <| List.foldl fn "" <| toHexList b
