module Main exposing (..)

import Array exposing (..)
import Basics exposing (..)
import Bits exposing (..)
import Bitwise exposing (..)
import Core exposing (..)
import Html exposing (..)
import List exposing (..)
import String exposing (..)


test : Bits.Bits
test =
    0x00FAFAFA


type alias HashInternals =
    { input : String
    , k : Array Bits.Bits
    , h : Array Bits.Bits
    }


type alias Model =
    HashInternals


model : HashInternals
model =
    { input = "HIHI"
    , k = Core.k
    , h = Core.h
    }


type Msg
    = Test


getNth : Int -> Array Core.Bits -> Int
getNth n arr =
    case Array.get n arr of
        Nothing ->
            -1

        Just v ->
            v


kValues : List (Html Msg)
kValues =
    List.map (\n -> div [] [ text <| printBits <| getNth n Core.k ]) (range 0 63)


kPrinter : Html Msg
kPrinter =
    div []
        (List.append
            [ h1 [] [ text "Values of K" ] ]
            kValues
        )


hValues : List (Html Msg)
hValues =
    List.map (\n -> div [] [ text <| printBits <| getNth n Core.h ]) (range 0 7)


hPrinter : Html Msg
hPrinter =
    div []
        (List.append
            [ h1 [] [ text "Values of H" ] ]
            hValues
        )


view : Model -> Html Msg
view model =
    div []
        [ {-hPrinter
        , kPrinter
        ,-} div [] [ text "HI" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
