module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Http

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
    | MakeRequest
    | PostReceived (Result Http.Error Response)
    | ClearSearch

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = Just query }, Cmd.none )

        MakeRequest ->
            case model.query of
                Just query ->
                    ( model, post (buildRequest query) )
                Nothing ->
                    ( model, Cmd.none )

        PostReceived result ->
            case result of
                Ok response -> ( { model | state = RequestSuccess response }, Cmd.none )
                Err error->  ( { model | state = RequestFailure error }, Cmd.none )

        ClearSearch ->
            ( initialModel, Cmd.none )



-- HTTP

type alias Request =
    { titles: List String
    , direction: String
    }

post : Request -> Cmd Msg
post request =
  Http.request
    { method = "POST"
    , headers = headers
    , url = "https://am121f9ih9.execute-api.us-west-2.amazonaws.com/default/v1/tap-in"
    , body = Http.jsonBody (requestEncoder request)
    , expect = Http.expectJson PostReceived responseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

headers: List Http.Header
headers =
    [ Http.header "Access-Control-Allow-Origin" "*"
    , Http.header "Access-Control-Allow-Credentials" "false"
    , Http.header "Access-Control-Allow-Methods" "GET,HEAD,OPTIONS,POST,PUT"
    , Http.header "Access-Control-Allow-Headers" bigAssHeaderParam
    ]

bigAssHeaderParam : String
bigAssHeaderParam =
    "Origin, X-Requested-With, Content-Type, Accept, Authorization"

requestEncoder: Request -> Encode.Value
requestEncoder request=
    Encode.object
        [ ( "titles", Encode.list Encode.string request.titles )
        , ( "direction", Encode.string request.direction )
        ]

responseDecoder: Decode.Decoder Response
responseDecoder =
    Decode.map2 Response
        (Decode.field "titles" (Decode.list Decode.string))
        (Decode.field "related_pages" (Decode.list Decode.string))

buildRequest : String -> Request
buildRequest selected =
    Request [selected] "in"



-- VIEW

view : Model -> Html Msg
view model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest

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

viewRequestSuccess : Response -> Html Msg
viewRequestSuccess response =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody
        , viewResponse response
        ]

viewRequestFailure : Http.Error -> Html Msg
viewRequestFailure error  =
    case error of
        Http.BadUrl string ->
            button [ class "dropdown", onClick ClearSearch ]
            [ text ("Bad Url: " ++ string ++ "\nTry Again!") ]

        Http.Timeout ->
            button [ class "dropdown", onClick ClearSearch ]
            [ text "Server Timeout, Try Again!" ]

        Http.NetworkError ->
            button [ class "dropdown", onClick ClearSearch ]
            [ text "Network Error, Try Again!" ]

        Http.BadStatus int ->
            button [ class "dropdown", onClick ClearSearch ]
            [ text  ((String.fromInt int) ++ ", Try Again!") ]

        Http.BadBody body ->
            button [ class "dropdown", onClick ClearSearch ]
            [ text ("Bad Body: " ++ body ++ "\nTry Again!") ]

dropdownHead : Html Msg
dropdownHead =
    p [class "header"] [ text "Search for a Tap In!" ]

dropdownBody : Html Msg
dropdownBody =
    div [ class "dropdown-body" ]
        [ input [ class "search-box", onInput SearchInput ] []
        , makeRequestButton
         ]

makeRequestButton : Html Msg
makeRequestButton =
    button [ class "button", onClick MakeRequest ] [ text "Make Request!" ]

viewResponse: Response -> Html Msg
viewResponse response =
    ul [ class "response" ]
    [ ul [] ([text "titles:"] ++ (responseItems response.titles))
    , ul [] ([text "related pages:"] ++ (responseItems response.related_pages))
    ]

responseItems: List String -> List (Html Msg)
responseItems items =
    List.map (responseItem) items

responseItem: String -> Html Msg
responseItem item =
    li [] [text item]
