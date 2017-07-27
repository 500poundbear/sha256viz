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


rotr : Bits -> Int -> Bits
rotr x n =
    let
        rest =
            n
    in
    or (shiftRightZfBy x n) (shiftLeftBy rest x)


σ1 : Bits -> Bits
σ1 b =
    and (and (rotr 17 b) (rotr 19 b)) (shiftLeftBy 10 b)


σ0 : Bits -> Bits
σ0 b =
    and (and (rotr 7 b) (rotr 18 b)) (shiftRightZfBy 3 b)


s0 : Bits -> Bits
s0 b =
    and (and (rotr 2 b) (rotr 13 b)) (rotr 22 b)


s1 : Bits -> Bits
s1 b =
    and (and (rotr 6 b) (rotr 11 b)) (rotr 25 b)


choose : Bits -> Bits -> Bits -> Bits
choose x y z =
    Bitwise.xor (and x y) (and x z)


parity : Bits -> Bits -> Bits -> Bits
parity x y z =
    Bitwise.xor (Bitwise.xor x y) z


majority : Bits -> Bits -> Bits -> Bits
majority x y z =
    Bitwise.xor (Bitwise.xor (and x y) (and x z)) (and y z)



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
