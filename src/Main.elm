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
    = Searching
    | Found


type alias Model =
    { state : State
    , values : List String
    , selected : Maybe String
    , query : String
    }


initialModel : Model
initialModel =
    { state = Searching
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
        Searching ->
            viewSearching model

        Found ->
            viewFound model


viewSearching : Model -> Html Msg
viewSearching model =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody model
        ]


viewFound : Model -> Html Msg
viewFound model =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody model
        , maybeItemFound model.selected
        ]


dropdownHead : Html Msg
dropdownHead =
    p [] [ text "Search for a Tap In!" ]

dropdownItem : String -> Html Msg
dropdownItem value =
    li [ onClick (ItemSelected value) ] [ text value ]

dropdownBody : Model -> Html Msg
dropdownBody model =
    div [ class "dropdown-body" ]
        [ input [ id "search-box", onInput SearchInput ] []
        , ul [] (List.map dropdownItem (filteredValues model))
        ]

itemFound: String -> Html Msg
itemFound item =
    div [ class "item-found"] [text item]

maybeItemFound: Maybe String -> Html Msg
maybeItemFound item =
    case item of
        Nothing -> itemFound ""

        Just str -> itemFound str


filteredValues : Model -> List String
filteredValues model =
    List.filter (\v -> matchQuery model.query v) model.values


matchQuery : String -> String -> Bool
matchQuery needle haystack =
    String.contains (String.toLower needle) (String.toLower haystack)


type Msg
    = SearchAgain
    | ItemSelected String
    | SearchInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchAgain ->
            ( { model | state = Searching }, Cmd.none )

        ItemSelected value ->
            ( { model | selected = Just value, state = Found, query = "" }
            , Cmd.none
            )

        SearchInput query ->
            ( { model | query = query, selected = Nothing }, Cmd.none )
