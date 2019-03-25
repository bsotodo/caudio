module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, input, label, text)
import Html.Attributes exposing (checked, class, max, min, name, step, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { volume : String, mute : Bool, muteRes : MuteResponse, error : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { volume = "5", mute = False, muteRes = { result = [ "" ], id = -1 }, error = "" }
    , Cmd.none
    )



--  UPDATE


type Msg
    = SetVolume String 
    | Mute Bool
    | GotMuteResponse (Result Http.Error MuteResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mute True ->
            ( { model | mute = True }, postMute "on" )

        Mute False ->
            ( { model | mute = False }, postMute "off" )

        GotMuteResponse (Ok rec) ->
            ( { model | muteRes = rec }, Cmd.none )

        GotMuteResponse (Err _) ->
            ( { model | error = "Something went wrong with the request" }, Cmd.none )

        SetVolume volume ->
            ( { model | volume = volume }, postVolume volume )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "root" ] [ audioControlView model ]


audioControlView : Model -> Html Msg
audioControlView model =
    div [ class "view-audio-control" ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked (model.mute == True)
                , onCheck (\isChecked -> Mute isChecked)
                ]
                []
            , text "Mute"
            ]
        , label []
            [ input
                [ type_ "range"
                , min "0"
                , max "50"
                , step "1"
                , value model.volume
                , onInput (\value -> SetVolume value)
                ]
                []
            , text ("Volume: " ++ model.volume)
            ]
        ]


postMuteBody : String -> Encode.Value
postMuteBody param =
    Encode.object
        [ ( "method", Encode.string "setAudioMute" )
        , ( "id", Encode.int 601 )
        , ( "version", Encode.string "1.1" )
        , ( "params", Encode.list Encode.object [ [ ( "mute", Encode.string param ) ] ] )
        ]


type alias MuteResponse =
    { result : List String, id : Int }


muteResponseDecoder : Decoder MuteResponse
muteResponseDecoder =
    map2 MuteResponse
        (field "result" (list string))
        (field "id" int)


postMute : String -> Cmd Msg
postMute param =
    Http.post
        { url = "http://192.168.0.241:54480/sony/audio"
        , body = Http.jsonBody (postMuteBody param)
        , expect = Http.expectJson GotMuteResponse muteResponseDecoder
        }


postVolumeBody : String -> Encode.Value
postVolumeBody volume =
    Encode.object
        [ ( "method", Encode.string "setAudioVolume" )
        , ( "id", Encode.int 98 )
        , ( "version", Encode.string "1.1" )
        , ( "params", Encode.list Encode.object [ [ ( "volume", Encode.string volume ) ] ] )
        ]


postVolume : String -> Cmd Msg
postVolume volume =
    Http.post
        { url = "http://192.168.0.241:54480/sony/audio"
        , body = Http.jsonBody (postVolumeBody volume)
        , expect = Http.expectJson GotMuteResponse muteResponseDecoder
        }
