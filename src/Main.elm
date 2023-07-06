module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onBlur, onClick, onFocus, onInput)
import Json.Decode as Decode
import Random
import Set exposing (Set)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscription
        , view = view
        }



-- MODEL


type alias Matchup =
    { winning : String
    , loosing : String
    }


type alias Model =
    { items : Set String
    , content : String
    , focusContent : Bool
    , kFactor : Float
    , currentMatchup : Maybe Matchup
    , matchupHistory : List Matchup
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = Set.empty
      , content = ""
      , focusContent = False
      , kFactor = 40
      , currentMatchup = Nothing
      , matchupHistory = []
      }
    , Cmd.none
    )



-- UPDATE


type Key
    = Y
    | N
    | Enter
    | Other


type Msg
    = Focus
    | Blur
    | Change String
    | RandomMatchup
    | NewMatchup (Maybe Matchup)
    | NewKFactor Float
    | SetWinning String
    | SetLoosing String
    | AddItem
    | Commit
    | CommitSwapped
    | PressedKey Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus ->
            ( { model | focusContent = True }, Cmd.none )

        Blur ->
            ( { model | focusContent = False }, Cmd.none )

        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        RandomMatchup ->
            ( model
            , Random.generate NewMatchup (matchupGenerator model.items)
            )

        NewMatchup e ->
            ( { model | currentMatchup = e }, Cmd.none )

        NewKFactor k ->
            ( { model | kFactor = k }, Cmd.none )

        SetLoosing s ->
            ( { model
                | currentMatchup =
                    Maybe.map
                        (\x ->
                            if x.winning == s then
                                x

                            else
                                { winning = x.winning
                                , loosing = s
                                }
                        )
                        model.currentMatchup
              }
            , Cmd.none
            )

        --             )
        SetWinning s ->
            ( { model
                | currentMatchup =
                    Maybe.map
                        (\x ->
                            if x.loosing == s then
                                x

                            else
                                { winning = s
                                , loosing = x.loosing
                                }
                        )
                        model.currentMatchup
              }
            , Cmd.none
            )

        AddItem ->
            let
                invalidContent =
                    model.content == "" || Set.member model.content model.items

                newModel =
                    if invalidContent then
                        model

                    else
                        { model
                            | content = ""
                            , items = Set.insert model.content model.items
                        }
            in
            case model.currentMatchup of
                Nothing ->
                    update RandomMatchup newModel

                Just _ ->
                    ( newModel, Cmd.none )

        Commit ->
            case model.currentMatchup of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    update RandomMatchup
                        { model
                            | matchupHistory = c :: model.matchupHistory
                        }

        CommitSwapped ->
            update Commit
                { model
                    | currentMatchup =
                        Maybe.map
                            (\x ->
                                { winning = x.loosing
                                , loosing = x.winning
                                }
                            )
                            model.currentMatchup
                }

        PressedKey k ->
            if model.focusContent then
                case k of
                    Enter ->
                        update AddItem model

                    _ ->
                        ( model, Cmd.none )

            else
                case k of
                    Y ->
                        update Commit model

                    N ->
                        update CommitSwapped model

                    _ ->
                        ( model, Cmd.none )


matchupGenerator : Set String -> Random.Generator (Maybe Matchup)
matchupGenerator s =
    case Set.toList s of
        [] ->
            Random.constant Nothing

        x :: xs ->
            Random.uniform x xs
                |> Random.andThen
                    (\i ->
                        case Set.toList (Set.remove i s) of
                            [] ->
                                Random.constant Nothing

                            b :: bs ->
                                Random.uniform b bs
                                    |> Random.map (\j -> Just (Matchup i j))
                    )



-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    onKeyDown (Decode.map toKey (Decode.field "key" Decode.string))


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            case char of
                'y' ->
                    PressedKey Y

                'n' ->
                    PressedKey N

                _ ->
                    PressedKey Other

        _ ->
            case string of
                "Enter" ->
                    PressedKey Enter

                _ ->
                    PressedKey Other



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ onBlur Blur
            , onFocus Focus
            , placeholder "Item Name"
            , value model.content
            , onInput Change
            ]
            []
        , button [ onClick AddItem ] [ text "Add to List [Enter]" ]
        , div [] (viewRankList model.kFactor model.items model.matchupHistory)
        , div [] (viewKFactor model.kFactor)
        , div [] (viewCompare model.currentMatchup model.items)
        , div [] (viewMatchupHistory model.matchupHistory)
        ]


viewRankList : Float -> Set String -> List Matchup -> List (Html Msg)
viewRankList k s e =
    Set.toList s
        |> List.map (\x -> ( x, 1500.0 ))
        |> Dict.fromList
        |> (\d -> List.foldr (updateWithMatchup k) d e)
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> viewItems


updateWithMatchup : Float -> Matchup -> Dict String Float -> Dict String Float
updateWithMatchup k e d =
    let
        quality =
            \r -> 10 ^ (r / 400)

        eA =
            \rA -> \rB -> quality rA / (quality rA + quality rB)

        rankUpdateW =
            \rA -> \rB -> rA + k * (1 - eA rA rB)

        rankUpdateL =
            \rA -> \rB -> rA + k * (0 - eA rA rB)

        rankW =
            Dict.get e.winning d

        rankL =
            Dict.get e.loosing d

        newRankW =
            Maybe.map2 rankUpdateW rankW rankL

        newRankL =
            Maybe.map2 rankUpdateL rankL rankW
    in
    d
        |> Dict.update e.winning (\_ -> newRankW)
        |> Dict.update e.loosing (\_ -> newRankL)


viewItems : List ( String, Float ) -> List (Html Msg)
viewItems =
    List.map
        (\x ->
            let
                name =
                    Tuple.first x

                rank =
                    Tuple.second x

                rankStr =
                    round rank
                        |> String.fromInt

                s =
                    round ((1800 - rank) * 255 / 600)
                        |> String.fromInt

                color =
                    "rgb(" ++ s ++ " " ++ s ++ " " ++ s ++ ")"
            in
            div
                [ style "background-color" color
                , style "color" "white"
                ]
                [ text (name ++ " " ++ rankStr) ]
        )


viewMatchupHistory : List Matchup -> List (Html Msg)
viewMatchupHistory =
    List.map
        (\x ->
            let
                message =
                    x.winning ++ " won against " ++ x.loosing
            in
            div [] [ text message ]
        )


viewKFactor : Float -> List (Html Msg)
viewKFactor k =
    [ text ("K-Factor = " ++ String.fromFloat k)
    , button [ onClick (NewKFactor (k + 1)) ] [ text "+1" ]
    , button [ onClick (NewKFactor (k - 1)) ] [ text "-1" ]
    ]


viewCompare : Maybe Matchup -> Set String -> List (Html Msg)
viewCompare en s =
    case en of
        Nothing ->
            []

        Just e ->
            [ button [ onClick RandomMatchup ] [ text "Random Pair" ]
            , div []
                [ text "Does "
                , select [ onInput SetWinning ]
                    (List.map
                        (\x ->
                            option
                                [ value x
                                , selected (x == e.winning)
                                ]
                                [ text x ]
                        )
                        (Set.toList (Set.remove e.loosing s))
                    )
                , text " win against "
                , select [ onInput SetLoosing ]
                    (List.map
                        (\x ->
                            option
                                [ value x
                                , selected (x == e.loosing)
                                ]
                                [ text x ]
                        )
                        (Set.toList (Set.remove e.winning s))
                    )
                , text " ?"
                ]
            , button [ onClick Commit ] [ text "Yes [y]" ]
            , button [ onClick CommitSwapped ] [ text "No [n]" ]
            ]
