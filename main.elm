module Main exposing (..)

import Array exposing (..)
import Basics exposing (..)
import Bits exposing (..)
import Bitwise exposing (..)
import Core exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import String exposing (..)


test : Bits.Bits
test =
    0x00FAFAFA


testBlock : Core.Block
testBlock =
    [ 0x61626380
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0x18
    ]


type alias HashInternals =
    { input : String
    , k : Array Bits.Bits
    , h : Array Bits.Bits
    , blockState : BlockState
    }


type alias BlockState =
    { a : Block
    , b : Block
    , c : Block
    , d : Block
    , e : Block
    , f : Block
    , g : Block
    , h : Block
    , h0 : Block
    , h1 : Block
    , h2 : Block
    , h3 : Block
    , h4 : Block
    , h5 : Block
    , h6 : Block
    , h7 : Block
    }


emptyBlock : Block
emptyBlock =
    List.repeat 16 0


defaultBlockState : BlockState
defaultBlockState =
    { a = emptyBlock
    , b = emptyBlock
    , c = emptyBlock
    , d = emptyBlock
    , e = emptyBlock
    , f = emptyBlock
    , g = emptyBlock
    , h = emptyBlock
    , h0 = emptyBlock
    , h1 = emptyBlock
    , h2 = emptyBlock
    , h3 = emptyBlock
    , h4 = emptyBlock
    , h5 = emptyBlock
    , h6 = emptyBlock
    , h7 = emptyBlock
    }


type alias Model =
    HashInternals


model : HashInternals
model =
    { input = "HIHI"
    , k = Core.k
    , h = Core.h
    , blockState = defaultBlockState
    }


initialState : Array Core.Bits
initialState =
    Array.fromList
        [ Core.getNthArray 0 Core.h
        , Core.getNthArray 1 Core.h
        , Core.getNthArray 2 Core.h
        , Core.getNthArray 3 Core.h
        , Core.getNthArray 4 Core.h
        , Core.getNthArray 5 Core.h
        , Core.getNthArray 6 Core.h
        , Core.getNthArray 7 Core.h
        , Core.getNthArray 0 Core.h
        , Core.getNthArray 1 Core.h
        , Core.getNthArray 2 Core.h
        , Core.getNthArray 3 Core.h
        , Core.getNthArray 4 Core.h
        , Core.getNthArray 5 Core.h
        , Core.getNthArray 6 Core.h
        , Core.getNthArray 7 Core.h
        ]


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


stepPrinter : Int -> Int -> Html Msg
stepPrinter n p =
    let
        xxx =
            hashComputer testBlock n initialState Core.k

        output =
            case get p xxx of
                Nothing ->
                    0

                Just v ->
                    v
    in
    text <| printHex output


stepValues : Int -> List (Html Msg)
stepValues step =
    List.map (\n -> td [ Html.Attributes.style [ ( "border", "1px solid black" ) ] ] [ stepPrinter step n ]) (range 0 15)



{- Number of steps to print -}


stepsPrinterHelper : Int -> List (Html Msg)
stepsPrinterHelper n =
    List.map (\n -> tr [] (stepValues n)) (range 1 n)



{- stepValues -}


stepsPrinter : Html Msg
stepsPrinter =
    div []
        (List.append
            [ h1 [] [ text <| "Steps" ] ]
            [ table [] <|
                List.append
                    [ tr []
                        [ th [] [ text "a" ]
                        , th [] [ text "b" ]
                        , th [] [ text "c" ]
                        , th [] [ text "d" ]
                        , th [] [ text "e" ]
                        , th [] [ text "f" ]
                        , th [] [ text "g" ]
                        , th [] [ text "h" ]
                        , th [] [ text "h0" ]
                        , th [] [ text "h1" ]
                        , th [] [ text "h2" ]
                        , th [] [ text "h3" ]
                        , th [] [ text "h4" ]
                        , th [] [ text "h5" ]
                        , th [] [ text "h6" ]
                        , th [] [ text "h7" ]
                        ]
                    ]
                    (stepsPrinterHelper 20)
            ]
        )


scheduleValues : List (Html Msg)
scheduleValues =
    List.map (\n -> td [ Html.Attributes.style [ ( "border", "1px solid black" ) ] ] [ text <| printHex <| getSchedule n testBlock ]) (range 0 63)


schedulePrinter : Html Msg
schedulePrinter =
    div []
        (List.append
            [ h1 [] [ text <| "Schedule" ] ]
            [ table [ Html.Attributes.style [ ( "border", "1px solid black" ) ] ]
                [ tr [] (List.map (\n -> td [] [ text <| toString n ]) (range 1 64))
                , tr [] scheduleValues
                ]
            ]
        )


view : Model -> Html Msg
view model =
    div []
        [ {- hPrinter
             , kPrinter
             ,
          -}
          schedulePrinter
        , stepsPrinter
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
