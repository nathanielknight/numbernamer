-- Given a number, split it into thousands, millions, etc. and display the result.


module Main exposing (main)

import Browser
import Html
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = "", update = update, view = view }


type alias Model =
    String


type Msg
    = Input String


update : Msg -> Model -> Model
update (Input s) m =
    if isAllDigits s && (66 >= String.length s) then
        s

    else
        m


isAllDigits : String -> Bool
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
        [ attribute "type" "text"
        , value m
        , onInput Input
        , Html.Attributes.placeholder "Got a big number?"
        , Html.Attributes.autofocus True
        , Html.Attributes.attribute "inputmode" "numeric"
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


partnames : List String
partnames =
    [ "◾"
    , "thousand"
    , "million"
    , "billion"
    , "trillion"
    , "quadrillion"
    , "qintillion"
    , "sextillion"
    , "septillion"
    , "octillion"
    , "nonillion"
    , "decillion"
    , "undecillion"
    , "duodecillion"
    , "tredecillion"
    , "quattuordecillion"
    , "quindecillion"
    , "sexdecillion"
    , "septdecillion"
    , "octdecillion"
    , "novemdecillion"
    , "vingdecillion"
    ]


namedNumberGroups : List String -> List ( String, String )
namedNumberGroups ns =
    zipShortest ns partnames
        |> List.filter (Tuple.first >> String.isEmpty >> not)


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
                |> List.map (stripLeadingZeros << String.reverse << String.fromList)
                |> List.reverse
    in
    combine groups


stripLeadingZeros : String -> String
stripLeadingZeros s =
    case String.uncons s of
        Nothing ->
            s

        Just ( '0', r ) ->
            stripLeadingZeros r

        Just _ ->
            s


groupsOf : Int -> List a -> List (List a)
groupsOf n xs =
    group xs n []


group : List a -> Int -> List (List a) -> List (List a)
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
