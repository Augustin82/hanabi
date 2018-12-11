module Main exposing (Card(..), Col(..), Model, Msg(..), Val(..), init, main, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Random



---- MODEL ----


type alias Model =
    { players : List Player
    , turn : Maybe Player
    , hints : Int
    , errors : Int
    , discard : List Card
    , deck : List Card
    , played : Game
    }


initModel =
    { players = []
    , turn = Nothing
    , hints = 8
    , errors = 0
    , discard = []
    , deck = deck
    , played =
        { red = 0
        , green = 0
        , blue = 0
        , yellow = 0
        , white = 0
        }
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Random.generate GeneratedDeck <| fastShuffle deck
    )



---- UPDATE ----


type Msg
    = NoOp
    | GeneratedDeck (List Card)
    | Discard Val Col
    | Play Val Col
    | HintValue Player Val
    | HintColor Player Col


type alias Game =
    { red : Int
    , green : Int
    , yellow : Int
    , blue : Int
    , white : Int
    }


type alias Player =
    { name : String
    , hand : List Card
    }


type Card
    = Card Val Col


type Val
    = One
    | Two
    | Three
    | Four
    | Five


valToString val =
    case val of
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"


valToNumber val =
    case val of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5


allVal =
    [ One, Two, Three, Four, Five ]


type Col
    = Red
    | Green
    | Blue
    | Yellow
    | White


allCol =
    [ Red, Green, Blue, Yellow, White ]


colToString col =
    case col of
        Red ->
            "Red"

        Green ->
            "Green"

        Blue ->
            "Blue"

        Yellow ->
            "Yellow"

        White ->
            "White"


colToColor col =
    case col of
        Red ->
            rgb255 233 48 43

        Yellow ->
            rgb255 246 219 5

        Blue ->
            rgb255 95 191 239

        White ->
            rgb255 255 255 255

        Green ->
            rgb255 146 189 74


cardColor =
    rgb255 50 40 103


blackColor =
    rgb255 0 0 0


lightGrey =
    rgb255 240 240 240


darkGrey =
    rgb255 140 140 140


deck : List Card
deck =
    let
        values =
            [ List.repeat 3 One
            , List.repeat 2 Two
            , List.repeat 2 Three
            , List.repeat 2 Four
            , List.repeat 1 Five
            ]
                |> List.concat
    in
    allCol
        |> List.map (\col -> values |> List.map (\val -> Card val col))
        |> List.concat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedDeck newDeck ->
            ( { model | deck = newDeck }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ Background.color lightGrey ] <|
        column [ padding 5, spacing 5 ] <|
            List.map (\(Card val col) -> viewCard (Just val) (Just col)) model.deck


viewCard : Maybe Val -> Maybe Col -> Element msg
viewCard mVal mCol =
    let
        val =
            mVal
                |> Maybe.map (valToNumber >> String.fromInt)
                |> Maybe.withDefault "?"

        col =
            mCol
                |> Maybe.map colToColor
                |> Maybe.withDefault darkGrey
    in
    column
        [ Border.width 1
        , Border.color blackColor
        , Font.bold
        , width <| px 80
        , height <| px 80
        ]
        [ el [ width fill, height fill ] <|
            el [ centerY, centerX ] <|
                text val
        , el [ width fill, height fill, Background.color col ] <|
            text "\u{00A0}"
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


randomInt : Random.Generator Int
randomInt =
    Random.int Random.minInt Random.maxInt


fastShuffle : List a -> Random.Generator (List a)
fastShuffle items =
    randomInt
        |> Random.map
            (\seed ->
                let
                    taggedItems =
                        decorate items (Random.initialSeed seed) []

                    shuffled =
                        List.sortBy Tuple.first taggedItems
                in
                undecorate shuffled []
            )


decorate : List a -> Random.Seed -> List ( Int, a ) -> List ( Int, a )
decorate items currentSeed accumulatedTaggedItems =
    case items of
        firstItem :: remainingItems ->
            let
                ( tag, updatedSeed ) =
                    Random.step randomInt currentSeed

                taggedItem =
                    ( tag, firstItem )
            in
            decorate remainingItems
                updatedSeed
                (taggedItem :: accumulatedTaggedItems)

        [] ->
            accumulatedTaggedItems


undecorate : List ( Int, a ) -> List a -> List a
undecorate taggedItems accumulatedItems =
    case taggedItems of
        ( tag, firstItem ) :: remainingTaggedItems ->
            undecorate remainingTaggedItems (firstItem :: accumulatedItems)

        [] ->
            accumulatedItems
