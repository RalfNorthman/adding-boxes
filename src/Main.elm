module Main exposing (..)

import Element exposing (..)
import Browser
import Dict exposing (Dict)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


-- Colors


makeGrey zeroToOne =
    rgb zeroToOne zeroToOne zeroToOne


grey zeroToTen =
    makeGrey <| zeroToTen / 10



-- Model


init =
    { thickBorder = False
    , boxes = Dict.empty
    , latestKey = 0
    }


type alias RowKey =
    Int


type alias NumberOfElements =
    Int


type alias Boxes =
    Dict RowKey NumberOfElements


type alias Model =
    { thickBorder : Bool
    , latestKey : Int
    , boxes : Boxes
    }


type Msg
    = ThickBorder Bool
    | AddRow
    | Lengthen Int



-- Update


helpFunc maybe =
    case maybe of
        Just value ->
            Just <| value + 1

        Nothing ->
            Just 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        ThickBorder bool ->
            { model | thickBorder = bool }

        AddRow ->
            let
                newLatest =
                    model.latestKey + 1
            in
                { model
                    | boxes = Dict.insert newLatest 1 model.boxes
                    , latestKey = newLatest
                }

        Lengthen key ->
            { model
                | boxes = Dict.update key helpFunc model.boxes
            }



-- View


sidebarStyle model =
    [ Background.color <| grey 9
    , Border.color <| grey 7
    , Border.widthEach
        { bottom = 0
        , left = 0
        , right =
            if model.thickBorder then
                6
            else
                2
        , top = 0
        }
    , padding 10
    , spacing 5
    , height fill
    ]


buttonStyle =
    [ Background.color <| grey 8
    , padding 5
    , Border.rounded 6
    , Border.widthEach
        { bottom = 1
        , left = 0
        , right = 1
        , top = 0
        }
    ]


box =
    [ Background.color <| grey 3
    , height <| px 50
    , width <| px 50
    , Border.rounded 8
    ]


makeBoxRow key number =
    row [ spacing 10 ] <|
        List.repeat
            number
        <|
            Input.button box
                { onPress = Just <| Lengthen key
                , label = text ""
                }


view model =
    layout
        [ Font.color <| grey 3
        ]
    <|
        row
            [ spacing 10
            , height fill
            ]
            [ column (sidebarStyle model)
                [ Input.checkbox []
                    { onChange = (\bool -> ThickBorder bool)
                    , icon = Input.defaultCheckbox
                    , checked = model.thickBorder
                    , label = Input.labelRight [] <| text "Thick border?"
                    }
                , Input.button buttonStyle
                    { onPress = Just AddRow
                    , label = text "Add box row"
                    }
                ]
            , column [ spacing 10 ] <|
                if model.boxes == Dict.empty then
                    List.singleton none
                else
                    Dict.map makeBoxRow model.boxes
                        |> Dict.values
            ]



-- Main


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
