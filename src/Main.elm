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
    , selected : List String
    , rank : Int
    }


type State
    = BuildingRequest
    | SearchConfirmed
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
    , selected = []
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
    | AddSearch
    | ConfirmSearch String


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
                    updateWithRequest model (buildRequest "in") PostReceivedIn

                Out ->
                    updateWithRequest model (buildRequest "out") PostReceivedOut

        PostReceivedIn result ->
            updateWithResponse model result In

        PostReceivedOut result ->
            updateWithResponse model result Out

        ClearSearch ->
            ( initialModel, Cmd.none )

        AddSearch ->
            ( { model | rank = model.rank + 1 }, Cmd.none )

        ConfirmSearch title ->
            ( { model | state = SearchConfirmed, selected = model.selected ++ [ title ] }, Cmd.none )


updateWithRequest model buildRequestArg toMsg =
    ( { model | state = Loading }, post (buildRequestArg model.selected) toMsg )


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


buildRequest : String -> List String -> Request
buildRequest directionString selected =
    Request selected directionString



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest model

        SearchConfirmed ->
            viewSearchConfirmed model

        Loading ->
            viewLoading

        RequestSuccess response direction ->
            viewRequestSuccess response direction

        RequestFailure error ->
            viewRequestFailure error


viewSearchConfirmed : Model -> Html Msg
viewSearchConfirmed model =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
        , viewConfirmations model
        ]


viewConfirmations : Model -> Html Msg
viewConfirmations model =
    ul [ class "dropdown" ]
        ([ text "We're Searching For:" ] ++ List.map fromTitleToUrlHtml model.selected)


viewBuildingRequest : Model -> Html Msg
viewBuildingRequest model =
    case model.query of
        Nothing ->
            viewNoInput

        Just title ->
            case title of
                "" ->
                    viewNoInput

                _ ->
                    div [ class "dropdown" ]
                        [ dropDownHeadAndBody [ confirmSearchButton title ] ]


viewNoInput : Html Msg
viewNoInput =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [] ]


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : Response -> Direction -> Html Msg
viewRequestSuccess response direction =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
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


dropdownBody : List (Html Msg) -> Html Msg
dropdownBody moreHtml =
    div [ class "dropdown-body" ]
        ([ input [ class "search-box", onInput SearchInput ] []
         ]
            ++ moreHtml
        )


dropDownHeadAndBody : List (Html Msg) -> Html Msg
dropDownHeadAndBody moreHtml =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody moreHtml
        ]


makeRequestInDirectionButton : Html Msg
makeRequestInDirectionButton =
    button [ class "button", onClick (RequestMade In) ] [ text "in" ]


makeRequestOutDirectionButton : Html Msg
makeRequestOutDirectionButton =
    button [ class "button", onClick (RequestMade Out) ] [ text "out" ]


almostClearSearchButton : List (Html Msg) -> Html Msg
almostClearSearchButton =
    button [ class "button", onClick ClearSearch ]


defaultClearSearchButton : Html Msg
defaultClearSearchButton =
    almostClearSearchButton [ text "Clear Search" ]


addSearchButton : Html Msg
addSearchButton =
    button [ class "button", onClick AddSearch ] [ text "Add Search" ]


confirmSearchButton : String -> Html Msg
confirmSearchButton title =
    button [ class "button", onClick (ConfirmSearch title) ] [ text "Confirm" ]


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
