module Core exposing (..)

import Array exposing (..)
import Basics exposing (..)
import Bits exposing (..)
import Bitwise exposing (..)
import List exposing (..)
import String exposing (..)


type alias Bits =
    Int


type alias Block =
    List Int


k : Array Bits
k =
    Array.fromList
        [ 0x428A2F98
        , 0x71374491
        , 0xB5C0FBCF
        , 0xE9B5DBA5
        , 0x3956C25B
        , 0x59F111F1
        , 0x923F82A4
        , 0xAB1C5ED5
        , 0xD807AA98
        , 0x12835B01
        , 0x243185BE
        , 0x550C7DC3
        , 0x72BE5D74
        , 0x80DEB1FE
        , 0x9BDC06A7
        , 0xC19BF174
        , 0xE49B69C1
        , 0xEFBE4786
        , 0x0FC19DC6
        , 0x240CA1CC
        , 0x2DE92C6F
        , 0x4A7484AA
        , 0x5CB0A9DC
        , 0x76F988DA
        , 0x983E5152
        , 0xA831C66D
        , 0xB00327C8
        , 0xBF597FC7
        , 0xC6E00BF3
        , 0xD5A79147
        , 0x06CA6351
        , 0x14292967
        , 0x27B70A85
        , 0x2E1B2138
        , 0x4D2C6DFC
        , 0x53380D13
        , 0x650A7354
        , 0x766A0ABB
        , 0x81C2C92E
        , 0x92722C85
        , 0xA2BFE8A1
        , 0xA81A664B
        , 0xC24B8B70
        , 0xC76C51A3
        , 0xD192E819
        , 0xD6990624
        , 0xF40E3585
        , 0x106AA070
        , 0x19A4C116
        , 0x1E376C08
        , 0x2748774C
        , 0x34B0BCB5
        , 0x391C0CB3
        , 0x4ED8AA4A
        , 0x5B9CCA4F
        , 0x682E6FF3
        , 0x748F82EE
        , 0x78A5636F
        , 0x84C87814
        , 0x8CC70208
        , 0x90BEFFFA
        , 0xA4506CEB
        , 0xBEF9A3F7
        , 0xC67178F2
        ]


h : Array Bits
h =
    Array.fromList [ 0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19 ]


msgPad1 : BitsList -> BitsList
msgPad1 bl =
    List.append bl [ 0x80 ]


bitsToBlocks : BitsList -> List Block
bitsToBlocks bl =
    let
        block : Block
        block =
            take 16 bl
    in
    List.append [ block ] (bitsToBlocks <| drop 16 bl)


getSchedule : Int -> Block -> Bits
getSchedule t block =
    if t < 16 then
        case Array.get t (Array.fromList block) of
            Nothing ->
                0

            Just bb ->
                bb
    else
        shiftRightZfBy 0
            (Bits.σ1 (getSchedule (t - 2) block)
                + getSchedule (t - 7) block
                + σ0 (getSchedule (t - 15) block)
                + getSchedule (t - 16) block
            )


getNthArray : Int -> Array Bits -> Bits
getNthArray n arr =
    case head <| drop n (Array.toList arr) of
        Nothing ->
            0

        Just v ->
            v



{- Block Iteration#  a  b c d e f g h -}


computeHash : Block -> Int -> Bits -> Bits -> Bits -> Bits -> Bits -> Bits -> Bits -> Bits -> Array Bits -> Array Bits -> Array Bits
computeHash block t a b c d e f g h k hashes =
    let
        kt =
            getNthArray t k

        t1 =
            h + Bits.s1 e + Bits.choose e f g + kt + getSchedule t block

        t2 =
            Bits.s0 a + Bits.majority a b c

        mhashes =
            List.map (\n -> getNthArray n hashes) (range 0 7)
    in
    Array.fromList
        [ {- a -} shiftRightZfBy 0 (t1 + t2)
        , {- b -} a
        , {- c -} b
        , {- d -} c
        , {- e -} shiftRightZfBy 0 (d + t1)
        , {- f -} e
        , {- g -} f
        , {- h -} g
        ]