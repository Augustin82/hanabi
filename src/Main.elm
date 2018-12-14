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
    , turn : Maybe String
    , hints : Int
    , errors : Int
    , discard : List Card
    , deck : Deck
    , played : Game
    }


initModel : Model
initModel =
    { players =
        [ { defaultPlayer | name = "Alice" }
        , { defaultPlayer | name = "Bob" }
        ]
    , turn = Just "Alice"
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
    | GeneratedDeck Deck
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
    , hand : List HandCard
    }


defaultPlayer =
    { name = ""
    , hand = []
    }


type alias HandCard =
    { card : Card
    , hints : Hints
    }


type alias Hints =
    { colors :
        { red : Maybe Bool
        , green : Maybe Bool
        , yellow : Maybe Bool
        , blue : Maybe Bool
        , white : Maybe Bool
        }
    , values :
        { one : Maybe Bool
        , two : Maybe Bool
        , three : Maybe Bool
        , four : Maybe Bool
        , five : Maybe Bool
        }
    }


type alias Deck =
    List Card


noHints : Hints
noHints =
    { colors =
        { red = Nothing
        , green = Nothing
        , yellow = Nothing
        , blue = Nothing
        , white = Nothing
        }
    , values =
        { one = Nothing
        , two = Nothing
        , three = Nothing
        , four = Nothing
        , five = Nothing
        }
    }


cardToHandCard : Card -> HandCard
cardToHandCard card =
    { card = card
    , hints = noHints
    }


addCardToHand : Card -> Player -> Player
addCardToHand card player =
    { player | hand = { card = card, hints = noHints } :: player.hand }


drawCard : Deck -> ( Maybe Card, Deck )
drawCard d =
    case d of
        [] ->
            ( Nothing, [] )

        top :: rest ->
            ( Just top, rest )


type Card
    = Card Val Col


type Val
    = One
    | Two
    | Three
    | Four
    | Five


valToString : Val -> String
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


valToNumber : Val -> Int
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


allVal : List Val
allVal =
    [ One
    , Two
    , Three
    , Four
    , Five
    ]


type Col
    = Red
    | Green
    | Blue
    | Yellow
    | White


allCol : List Col
allCol =
    [ Red
    , Green
    , Blue
    , Yellow
    , White
    ]


colToString : Col -> String
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


colToColor : Col -> Color
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


cardColor : Color
cardColor =
    rgb255 50 40 103


blackColor : Color
blackColor =
    rgb255 0 0 0


lightGrey : Color
lightGrey =
    rgb255 240 240 240


darkGrey : Color
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


cardsPerHand : List Player -> Int
cardsPerHand players =
    case List.length players of
        2 ->
            5

        3 ->
            5

        4 ->
            4

        _ ->
            0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedDeck newDeck ->
            ( distributeHands { model | deck = newDeck }, Cmd.none )

        _ ->
            ( model, Cmd.none )


fullHands model =
    List.foldr (\cur acc -> acc && (List.length cur.hand == cardsPerHand model.players)) True model.players


distributeHands : Model -> Model
distributeHands model =
    case List.map .name model.players of
        current :: next :: rest ->
            giveTopCardToPlayer current next rest model

        _ ->
            model


giveTopCardToPlayer : String -> String -> List String -> Model -> Model
giveTopCardToPlayer currentName nextName restNames model =
    if fullHands model then
        model

    else
        let
            players =
                model.players

            cards =
                model.deck

            newCurrent =
                nextName

            ( newNext, newRest ) =
                case restNames of
                    n :: r ->
                        ( n, r ++ [ currentName ] )

                    [] ->
                        ( currentName, [] )
        in
        case cards of
            [] ->
                model

            top :: rest ->
                let
                    newPlayers =
                        players
                            |> List.map
                                (\p ->
                                    if p.name == currentName then
                                        addCardToHand top p

                                    else
                                        p
                                )

                    newModel =
                        { model | players = newPlayers, deck = rest }
                in
                giveTopCardToPlayer newCurrent newNext newRest newModel



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ Background.color lightGrey ] <|
        column [ padding 5, spacing 5 ] <|
            List.map (viewPlayerHand model.turn) <|
                model.players


viewCardOfOthers : Card -> Element msg
viewCardOfOthers (Card val col) =
    viewCard (Just val) (Just col)


viewOwnCard : Card -> Element msg
viewOwnCard card =
    viewCard Nothing Nothing


viewPlayerHand : Maybe String -> Player -> Element msg
viewPlayerHand turn player =
    column []
        [ el [] <|
            text player.name
        , row
            [ padding 5
            , spacing 5
            ]
          <|
            (player
                |> .hand
                |> List.map .card
                |> List.map
                    (turn
                        |> Maybe.map
                            (\n ->
                                if n == player.name then
                                    viewOwnCard

                                else
                                    viewCardOfOthers
                            )
                        |> Maybe.withDefault viewCardOfOthers
                    )
            )
        ]


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



---- RANDOM ----


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
