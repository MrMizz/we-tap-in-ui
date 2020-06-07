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
    , rank : Int
    }


type State
    = BuildingRequest
    | Loading
    | RequestSuccess Response Direction
    | RequestFailure Http.Error


type alias Response =
    { titles : List String
    , related_pages : List String
    }


initialModel : Model
initialModel =
    { state = BuildingRequest
    , query = Nothing
    , rank = 1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = SearchInput String
    | RequestMade Direction
    | PostReceivedIn (Result Http.Error Response)
    | PostReceivedOut (Result Http.Error Response)
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
                    updateWithRequest model buildRequestInDirection PostReceivedIn

                Out ->
                    updateWithRequest model buildRequestOutDirection PostReceivedOut

        PostReceivedIn result ->
            updateWithResponse model result In

        PostReceivedOut result ->
            updateWithResponse model result Out

        ClearSearch ->
            ( initialModel, Cmd.none )


updateWithRequest model buildRequest toMsg =
    case model.query of
        Just query ->
            ( { model | state = Loading }, post (buildRequest query) toMsg )

        Nothing ->
            ( { model | state = BuildingRequest }, Cmd.none )


updateWithResponse model result direction =
    case result of
        Ok response ->
            ( { model | state = RequestSuccess response direction }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )



-- HTTP


type alias Request =
    { titles : List String
    , query : String
    }


post : Request -> (Result Http.Error Response -> msg) -> Cmd msg
post request msg =
    Http.post
        { url = "https://am121f9ih9.execute-api.us-west-2.amazonaws.com/default/v1/tap-in"
        , body = Http.jsonBody (requestEncoder request)
        , expect = Http.expectJson msg responseDecoder
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



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest

        Loading ->
            viewLoading

        RequestSuccess response direction ->
            viewRequestSuccess response direction

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
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : Response -> Direction -> Html Msg
viewRequestSuccess response direction =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody
        , almostClearSearchButton [ text "Clear Search" ]
        , viewTitlesSearched response.titles
        , viewDirectedResponse response direction
        ]


viewRequestFailure : Http.Error -> Html Msg
viewRequestFailure error =
    case error of
        Http.BadUrl string ->
            almostClearSearchButton [ text ("Bad Url: " ++ string ++ ", Try Again!") ]

        Http.Timeout ->
            almostClearSearchButton [ text "Server Timeout, Try Again!" ]

        Http.NetworkError ->
            almostClearSearchButton [ text "Network Error, Try Again!" ]

        Http.BadStatus int ->
            almostClearSearchButton [ text (String.fromInt int ++ " Error: Bad Title Input, Try Again!") ]

        Http.BadBody body ->
            almostClearSearchButton [ text ("Bad Body: " ++ body ++ ", Try Again!") ]


dropdownHead : Html Msg
dropdownHead =
    p [ class "header" ] [ text "Search For A Tap In!" ]


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


viewTitlesSearched : List String -> Html Msg
viewTitlesSearched titles =
    ul [ class "dropdown" ] ([ text "Titles Searched: " ] ++ List.map fromTitleToUrlHtml titles)


viewDirectedResponse : Response -> Direction -> Html Msg
viewDirectedResponse response direction =
    case direction of
        In ->
            viewResponse response "Direction: In"

        Out ->
            viewResponse response "Direction: Out"


viewResponse : Response -> String -> Html Msg
viewResponse response textToDisplay =
    ul [ class "response" ]
        [ ul [] ([ text textToDisplay ] ++ responseItems response.related_pages) ]


almostClearSearchButton : List (Html Msg) -> Html Msg
almostClearSearchButton =
    button [ class "dropdown", onClick ClearSearch ]


responseItems : List String -> List (Html Msg)
responseItems items =
    List.map fromTitleToUrlHtml items


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


fromTitleToUrlHtml : String -> Html Msg
fromTitleToUrlHtml title =
    li [] [ a [ Html.Attributes.target "_blank", Html.Attributes.href (fromTitleToUrl title) ] [ text title ] ]
