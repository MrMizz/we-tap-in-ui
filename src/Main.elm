module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { state : State
    , query : Maybe String
    }


type State
    = BuildingRequest
    | Loading
    | RequestSuccess Response
    | RequestFailure Http.Error


type alias Response =
    { titles : List String
    , related_pages : List String
    }


initialModel : Model
initialModel =
    { state = BuildingRequest
    , query = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = SearchInput String
    | RequestMade Direction
    | PostReceived (Result Http.Error Response)
    | ClearSearch

type Direction
    = In
    | Out

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = Just query }, Cmd.none )

        RequestMade direction ->
            case direction of
                In ->
                    updateWithRequest model buildRequestInDirection
                Out ->
                    updateWithRequest model buildRequestOutDirection

        PostReceived result ->
            case result of
                Ok response ->
                    ( { model | state = (RequestSuccess response) }, Cmd.none )

                Err error ->
                    ( { model | state = (RequestFailure error) }, Cmd.none )

        ClearSearch ->
            ( initialModel, Cmd.none )

updateWithRequest : { a | query : Maybe b, state : State } -> (b -> Request) -> ({ a | query : Maybe b, state : State }, Cmd Msg)
updateWithRequest model msg =
    case model.query of
        Just query ->
            ( { model | state = Loading }, post (msg query) )

        Nothing ->
            ( { model | state = Loading }, Cmd.none )



-- HTTP


type alias Request =
    { titles : List String
    , query : String
    }


post : Request -> Cmd Msg
post request =
    Http.post
        { url = "https://am121f9ih9.execute-api.us-west-2.amazonaws.com/default/v1/tap-in"
        , body = Http.jsonBody (requestEncoder request)
        , expect = Http.expectJson PostReceived responseDecoder
        }


requestEncoder : Request -> Encode.Value
requestEncoder request =
    Encode.object
        [ ( "titles", Encode.list Encode.string request.titles )
        , ( "query", Encode.string request.query )
        ]


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map2 Response
        (Decode.field "titles" (Decode.list Decode.string))
        (Decode.field "related_pages" (Decode.list Decode.string))


buildRequestInDirection : String -> Request
buildRequestInDirection selected =
    Request [ selected ] "in"


buildRequestOutDirection : String -> Request
buildRequestOutDirection selected =
    Request [ selected ] "out"

buildLoading : Direction -> String -> Request
buildLoading direction selected =
    case direction of
        In -> buildRequestInDirection selected

        Out -> buildRequestOutDirection selected





-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest

        Loading ->
            viewLoading

        RequestSuccess response ->
            viewRequestSuccess response

        RequestFailure error ->
            viewRequestFailure error


viewBuildingRequest : Html Msg
viewBuildingRequest =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ."]


viewRequestSuccess : Response -> Html Msg
viewRequestSuccess response =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody
        , clearSearchButton "Clear Search"
        , viewResponse response
        ]


viewRequestFailure : Http.Error -> Html Msg
viewRequestFailure error =
    case error of
        Http.BadUrl string ->
            clearSearchButton ("Bad Url: " ++ string ++ "\nTry Again!")

        Http.Timeout ->
            clearSearchButton "Server Timeout, Try Again!"

        Http.NetworkError ->
            clearSearchButton "Network Error, Try Again!"

        Http.BadStatus int ->
            clearSearchButton (String.fromInt int ++ " Error: Bad Title Input, Try Again!")

        Http.BadBody body ->
            clearSearchButton ("Bad Body: " ++ body ++ "\nTry Again!")


dropdownHead : Html Msg
dropdownHead =
    p [ class "header" ] [ text "Search for a Tap In!" ]


dropdownBody : Html Msg
dropdownBody =
    div [ class "dropdown-body" ]
        [ input [ class "search-box", onInput SearchInput ] []
        , makeRequestInDirectionButton
        , makeRequestOutDirectionButton
        ]


makeRequestInDirectionButton : Html Msg
makeRequestInDirectionButton =
    button [ class "button", onClick (RequestMade In) ] [ text "in" ]

makeRequestOutDirectionButton : Html Msg
makeRequestOutDirectionButton =
    button [ class "button", onClick (RequestMade Out) ] [ text "out" ]

viewResponse : Response -> Html Msg
viewResponse response =
    ul [ class "response" ]
        [ ul [] ([ text "Related Pages:" ] ++ responseItems response.related_pages) ]

clearSearchButton : String -> Html Msg
clearSearchButton string =
    button [ class "dropdown", onClick ClearSearch ]
        [ text string ]


responseItems : List String -> List (Html Msg)
responseItems items =
    List.map responseItem items


responseItem : String -> Html Msg
responseItem item =
    li [] [ a [ Html.Attributes.href (fromTitleToUrl item) ] [ text item ] ]


cleanTitle : String -> String
cleanTitle title =
    title
        |> String.replace "[" ""
        |> String.replace "]" ""


fromTitleToUrl : String -> String
fromTitleToUrl title =
    "https://en.wikipedia.org/wiki/"
        ++ (title
                |> cleanTitle
                |> String.replace " " "_"
           )
