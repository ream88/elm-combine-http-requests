module Main exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Http
import Json.Decode as JD
import RemoteData exposing (WebData)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    WebData Comment


type alias Comment =
    { text : String, user : User }


type alias User =
    { firstName : String, lastName : String }


type Msg
    = CommentResponse (WebData Comment)


init : ( Model, Cmd Msg )
init =
    ( RemoteData.NotAsked, getComment CommentResponse )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommentResponse response ->
            ( response, Cmd.none )


view : Model -> Html Msg
view model =
    pre []
        [ text <| toString model ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


commentDecoder : JD.Decoder Comment
commentDecoder =
    JD.map2 Comment
        (JD.field "text" JD.string)
        (JD.succeed { firstName = "foo", lastName = "bar" })


getComment : (WebData Comment -> msg) -> Cmd msg
getComment tagger =
    commentDecoder
        |> getRequest "http://localhost:8080/comment.json"
        |> RemoteData.sendRequest
        |> Cmd.map tagger


getRequest : String -> JD.Decoder a -> Http.Request a
getRequest url decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }
