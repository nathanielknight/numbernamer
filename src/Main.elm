-- Given a number, split it into thousands, millions, etc. and display the result.


module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox
        { init = "", update = update, view = view }


type alias Model =
    String


type Msg
    = Input String


update : Msg -> Model -> Model
update (Input s) m =
    case s of
        "" ->
            ""

        _ ->
            if isAllDigits s then
                s

            else
                m


isAllDigits s =
    s
        |> String.toList
        |> List.all Char.isDigit


view : Model -> Html.Html Msg
view m =
    Html.div []
        [ inputform m
        , display m
        ]


inputform : Model -> Html.Html Msg
inputform m =
    Html.input
        [ attribute "type" "number"
        , value m
        , onInput Input
        , Html.Attributes.placeholder "Got a big number?"
        , Html.Attributes.max numbermax
        , Html.Attributes.min numbermin
        , Html.Attributes.step numberstep
        ]
        []


display : Model -> Html.Html Msg
display m =
    let
        makeRow ( num, name ) =
            Html.tr []
                [ Html.td [] [ Html.text num ]
                , Html.td [] [ Html.text name ]
                ]
    in
    Html.table [] (m |> numberGroups |> namedNumberGroups |> List.map makeRow)



-------------------------------------------------------------------


numbermax =
    "999999999999999"


numbermin =
    "0"


numberstep =
    "1"


partnames =
    [ ".", "thousand", "million", "billion", "trillion", "quadrillion", "qintillion", "sextillion", "septillion" ]


type alias ParsedNumber =
    List String


namedNumberGroups : List String -> List ( String, String )
namedNumberGroups ns =
    zipShortest ns partnames


numberGroups : Model -> List String
numberGroups m =
    let
        groups =
            m
                |> String.toList
                |> List.reverse
                |> groupsOf 3

        combine gs =
            gs
                |> List.map (String.reverse << String.fromList)
                |> List.reverse
    in
    combine groups


groupsOf : Int -> List a -> List (List a)
groupsOf n xs =
    group xs n []


group xs n acc =
    let
        grp =
            List.take n xs
    in
    if grp == [] then
        acc

    else if List.length grp < n then
        grp :: acc

    else
        group (List.drop n xs) n (grp :: acc)


zipShortest : List a -> List b -> List ( a, b )
zipShortest aa bb =
    let
        innerZip xs ys acc =
            case ( xs, ys ) of
                ( [], _ ) ->
                    acc

                ( _, [] ) ->
                    acc

                ( x :: xr, y :: yr ) ->
                    innerZip xr yr (( x, y ) :: acc)
    in
    innerZip aa bb []
