module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type State
    = Open
    | Closed


type alias Model =
    { state : State
    , values : List String
    , selected : Maybe String
    , query : String
    }


initialModel : Model
initialModel =
    { state = Closed
    , values = fruits
    , selected = Nothing
    , query = ""
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


fruits : List String
fruits =
    [ "Rick James"
    , "Prince"
    , "James Brown"
    , "Kaytra"
    , "Shorter"
    , "Monk"
    , "Aka Big Time Tap in"
    ]


view : Model -> Html Msg
view model =
    case model.state of
        Open ->
            viewOpen model

        Closed ->
            viewClosed model


viewOpen : Model -> Html Msg
viewOpen model =
    div [ class "dropdown" ]
        [ dropdownHead model.selected CloseSelect
        , dropdownBody model
        ]


viewClosed : Model -> Html Msg
viewClosed model =
    div [ class "dropdown" ]
        [ dropdownHead model.selected OpenSelect
        ]


dropdownItem : String -> Html Msg
dropdownItem value =
    li [ onClick (ItemSelected value) ] [ text value ]


dropdownHead : Maybe String -> Msg -> Html Msg
dropdownHead selected msg =
    case selected of
        Nothing ->
            p [ onClick msg ] [ text "Click to toggle" ]

        Just value ->
            p [ onClick msg ] [ text value ]


dropdownBody : Model -> Html Msg
dropdownBody model =
    div [ class "dropdown-body" ]
        [ input [ id "search-box", onInput SearchInput ] []
        , ul [] (List.map dropdownItem (filteredValues model))
        ]


filteredValues : Model -> List String
filteredValues model =
    List.filter (\v -> matchQuery model.query v) model.values


matchQuery : String -> String -> Bool
matchQuery needle haystack =
    String.contains (String.toLower needle) (String.toLower haystack)


type Msg
    = OpenSelect
    | CloseSelect
    | ItemSelected String
    | SearchInput String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        OpenSelect ->
            ( { model | state = Open }, Cmd.none )

        CloseSelect ->
            ( { model | state = Closed }, Cmd.none )

        ItemSelected value ->
            ( { model | selected = Just value, state = Closed, query = "" }
            , Cmd.none
            )

        SearchInput query ->
            ( { model | query = query }, Cmd.none )