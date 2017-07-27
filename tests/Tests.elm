module Tests exposing (..)

import Array exposing (..)
import Basics exposing (..)
import Bits exposing (..)
import Bitwise exposing (..)
import Core exposing (..)
import Debug exposing (..)
import Expect
import Html exposing (..)
import List exposing (..)
import String exposing (..)
import Test exposing (..)


suite =
    describe "The Bits module"
        [ describe "rotr"
            [ test "rotates back to itself" <|
                \_ ->
                    let
                        value =
                            0xABCDEF55
                    in
                    Expect.equal value <| rotr 32 value
            , test "rotates back to itself after 2 shifts" <|
                \_ ->
                    let
                        value =
                            0xABCDEF55
                    in
                    Expect.equal value <| rotr 16 (rotr 16 value)
            , test "rotates back to itself after 3 shifts" <|
                \_ ->
                    let
                        value =
                            0xABCDEF55
                    in
                    Expect.equal value <| rotr 3 (rotr 29 value)
            , test "rotates to correct expected value" <|
                \_ ->
                    let
                        value =
                            0xABCDEF55

                        expected =
                            0xEF55ABCD
                    in
                    Expect.equal value <| rotr 16 expected
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            ]
        , describe "σ0"
            [ test "rotates back to itself" <|
                \_ ->
                    let
                        value =
                            0x0123

                        expected =
                            0x4648C026
                    in
                    Expect.equal expected <| σ0 value
            ]
        , describe "σ1"
            [ test "rotates back to itself" <|
                \_ ->
                    let
                        value =
                            0x0012345A

                        expected =
                            0x5CA64486
                    in
                    Expect.equal expected <| σ1 value
            ]
        , describe "s0"
            [ test "gives expected value" <|
                \_ ->
                    let
                        value =
                            0x12345678

                        expected =
                            0x66146474
                    in
                    Expect.equal expected <| s0 value
            ]
        , describe "s1"
            [ test "gives expected value" <|
                \_ ->
                    let
                        value =
                            0xABCDEF55

                        expected =
                            0x5AEDE4D5
                    in
                    Expect.equal expected <| s1 value
            ]
        , describe "choose"
            [ test "gives expected value" <|
                \_ ->
                    let
                        x =
                            0x5AEDE4D5

                        y =
                            0x9DEABC12

                        z =
                            0x1A4F3D29

                        expected =
                            0x18EABD38
                    in
                    Expect.equal expected <| choose x y z
            ]
        , describe "majority"
            [ test "gives expected value" <|
                \_ ->
                    let
                        x =
                            0x45728192

                        y =
                            0x9DEABC12

                        z =
                            0x4F323D29

                        expected =
                            0x4D72BD12
                    in
                    Expect.equal expected <| majority x y z
            ]
        ]
