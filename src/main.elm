port module Main exposing (Entry, Model, Msg(..), emptyModel, init, main, newEntry, onEnter, setStorage, update, updateWithStorage, view, viewControls, viewEntries, viewEntry, viewInput, viewKeyedEntry, viewTotal)

{-| entryMVC implemented in Elm, using plain HTML and CSS for rendering.
This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML
    This clean division of concerns is a core part of Elm. You can read more about
    this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Browser
import Browser.Dom as Dom
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Moneymoney", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL
-- The full application state of our entry app.


type alias Model =
    { entries : List Entry
    , descField : String
    , valField : Int
    , uid : Int
    }


type alias Entry =
    { description : String
    , value : Int
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = []
    , descField = ""
    , valField = 0
    , uid = 0
    }


newEntry : String -> Int -> Int -> Entry
newEntry desc val id =
    { description = desc
    , value = val
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateDescField String
    | UpdateValField String
    | Blur String
    | UpdateEntryDesc Int String
    | UpdateEntryVal Int String
    | Add
    | Delete Int



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            ( { model
                | uid = model.uid + 1
                , descField = ""
                , valField = 0
                , entries =
                    if String.isEmpty model.descField then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.descField model.valField model.uid ]
              }
            , Cmd.none
            )

        UpdateDescField str ->
            ( { model | descField = str }
            , Cmd.none
            )

        UpdateValField str ->
            case String.toInt str of
                Just int ->
                    ( { model | valField = int }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | valField = model.valField }
                    , Cmd.none
                    )

        Blur target ->
            ( model
            , Task.attempt (\_ -> NoOp) (Dom.blur target)
            )

        UpdateEntryDesc id task ->
            let
                updateEntryDesc t =
                    if t.id == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.map updateEntryDesc model.entries }
            , Cmd.none
            )

        UpdateEntryVal id task ->
            let
                updateEntryVal t =
                    if t.id == id then
                        case String.toInt task of
                            Just int ->
                                { t | value = int }

                            Nothing ->
                                { t | value = t.value }

                    else
                        t

                resetEntryVal t =
                    if t.id == id then
                        { t | value = t.value }

                    else
                        t

                zeroField t =
                    if t.id == id then
                        { t | value = 0 }

                    else
                        t
            in
            if String.isEmpty task then
                ( { model | entries = List.map zeroField model.entries }
                , Cmd.none
                )

            else
                case String.toInt task of
                    Just int ->
                        ( { model | entries = List.map updateEntryVal model.entries }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | entries = List.map resetEntryVal model.entries }
                        , Cmd.none
                        )

        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id) model.entries }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "entrymvc-wrapper" ]
        [ section
            [ class "entryapp" ]
            [ lazy2 viewInput model.descField (String.fromInt model.valField)
            , lazy viewEntries model.entries
            , lazy viewControls model.entries
            ]
        ]


viewInput : String -> String -> Html Msg
viewInput desc val =
    header
        [ class "header" ]
        [ h1 [] [ text "Moneymoney" ]
        , input
            [ class "new-entry"
            , placeholder "Enter a thing"
            , autofocus True
            , value desc
            , name "newEntryDescription"
            , onInput UpdateDescField
            , onEnter Add
            ]
            []
        , input
            [ class "new-entry"
            , placeholder "$10"
            , type_ "number"
            , value val
            , name "newEntryValue"
            , onInput UpdateValField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : List Entry -> Html Msg
viewEntries entries =
    section
        [ class "main"
        ]
        [ Keyed.ul [ class "entry-list" ] <|
            List.map viewKeyedEntry entries
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry entry =
    ( String.fromInt entry.id, lazy viewEntry entry )


viewEntry : Entry -> Html Msg
viewEntry entry =
    li
        [ class "entry" ]
        [ div
            []
            [ input
                [ class "entry-input"
                , value entry.description
                , name "title"
                , id ("decr-" ++ String.fromInt entry.id)
                , onInput (UpdateEntryDesc entry.id)
                , onEnter (Blur ("decr-" ++ String.fromInt entry.id))
                ]
                []
            , input
                [ class "entry-input"
                , value (String.fromInt entry.value)
                , name "title"
                , id ("val-" ++ String.fromInt entry.id)
                , onInput (UpdateEntryVal entry.id)
                , onEnter (Blur ("val-" ++ String.fromInt entry.id))
                ]
                []
            , button
                [ class "destroy"
                , onClick (Delete entry.id)
                ]
                []
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : List Entry -> Html Msg
viewControls entries =
    let
        entriesLen =
            List.length entries

        total =
            List.foldl sum 0 entries
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewTotal total
        ]


sum : Entry -> Int -> Int
sum e i =
    i + e.value


viewTotal : Int -> Html Msg
viewTotal total =
    span
        [ class "entry-total" ]
        [ text "Total $ "
        , strong [] [ text (String.fromInt total) ]
        ]
