module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, max, min, name, step, type_, value)
import Html.Events exposing (onCheck, onInput)
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


type PowerStatus
    = Active
    | Off


stringToPowerStatus : String -> PowerStatus
stringToPowerStatus powerStatus =
    if powerStatus == "active" then
        Active

    else
        Off


powerStatusToString : PowerStatus -> String
powerStatusToString powerStatus =
    if powerStatus == Active then
        "active"

    else
        "off"



-- MODEL


type alias Model =
    { volume : String
    , mute : Bool
    , muteRes : ApiResponse
    , error : String
    , audioInput : String
    , powerStatus : PowerStatus
    }



-- TODO: load initial data from the api


init : () -> ( Model, Cmd Msg )
init _ =
    ( { volume = "5"
      , mute = False
      , muteRes = { result = [ "" ], id = -1 }
      , error = ""
      , audioInput = "extInput:hdmi"
      , powerStatus = Off
      }
    , Cmd.none
    )



--  UPDATE


type Msg
    = SetVolume String
    | SetMute Bool
    | HandleApiResponse (Result Http.Error ApiResponse)
    | ChangeAudioInput String
    | ChangePowerStatus PowerStatus



-- TODO: update model based on the server response


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMute True ->
            ( { model | mute = True }, setMute "on" )

        SetMute False ->
            ( { model | mute = False }, setMute "off" )

        SetVolume volume ->
            ( { model | volume = volume }, setVolume volume )

        ChangeAudioInput source ->
            ( { model | audioInput = source }, changeAudioInput source )

        ChangePowerStatus powerStatus ->
            ( { model | powerStatus = powerStatus }, changePowerStatus powerStatus )

        HandleApiResponse (Ok rec) ->
            ( { model | muteRes = rec }, Cmd.none )

        HandleApiResponse (Err _) ->
            ( { model | error = "Something went wrong with the request" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ audioControlView model
        , audioInputView model
        , powerStatusView model.powerStatus
        , text model.error
        ]


audioControlView : Model -> Html Msg
audioControlView model =
    div [ class "view-audio-control" ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked (model.mute == True)
                , onCheck (\isChecked -> SetMute isChecked)
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


audioInputView : Model -> Html Msg
audioInputView model =
    div []
        [ label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:btAudio"
                , onInput (\value -> ChangeAudioInput value)
                , checked (model.audioInput == "extInput:btAudio")
                ]
                []
            , text "Bluetooth"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:line?port=1"
                , onInput (\value -> ChangeAudioInput value)
                , checked (model.audioInput == "extInput:line?port=1")
                ]
                []
            , text "Audio in (Port 1)"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:hdmi"
                , onInput (\value -> ChangeAudioInput value)
                , checked (model.audioInput == "extInput:hdmi")
                ]
                []
            , text "HDMI"
            ]
        ]


powerStatusView : PowerStatus -> Html Msg
powerStatusView powerStatus =
    div []
        [ label []
            [ input
                [ type_ "radio"
                , name "system-power"
                , value "off"
                , onInput (\value -> ChangePowerStatus (stringToPowerStatus value))
                , checked (powerStatus == Off)
                ]
                []
            , text "Off"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "system-power"
                , value "active"
                , onInput (\value -> ChangePowerStatus (stringToPowerStatus value))
                , checked (powerStatus == Active)
                ]
                []
            , text "On"
            ]
        ]



-- REQUESTS


type alias Endpoints =
    { audio : String, avContent : String, system : String }


endpoints : Endpoints
endpoints =
    { audio = "http://192.168.0.241:54480/sony/audio"
    , avContent = "http://192.168.0.241:54480/sony/avContent"
    , system = "http://192.168.0.241:54480/sony/system"
    }


type alias ApiResponse =
    { result : List String, id : Int }


apiResponseDecoder : Decoder ApiResponse
apiResponseDecoder =
    map2 ApiResponse
        (field "result" (list string))
        (field "id" int)



-- MUTE


setMuteBody : String -> Encode.Value
setMuteBody param =
    Encode.object
        [ ( "method", Encode.string "setAudioMute" )
        , ( "id", Encode.int 601 )
        , ( "version", Encode.string "1.1" )
        , ( "params", Encode.list Encode.object [ [ ( "mute", Encode.string param ) ] ] )
        ]


setMute : String -> Cmd Msg
setMute param =
    Http.post
        { url = endpoints.audio
        , body = Http.jsonBody (setMuteBody param)
        , expect = Http.expectJson HandleApiResponse apiResponseDecoder
        }



-- VOLUME


setVolumeBody : String -> Encode.Value
setVolumeBody volume =
    Encode.object
        [ ( "method", Encode.string "setAudioVolume" )
        , ( "id", Encode.int 98 )
        , ( "version", Encode.string "1.1" )
        , ( "params", Encode.list Encode.object [ [ ( "volume", Encode.string volume ) ] ] )
        ]


setVolume : String -> Cmd Msg
setVolume volume =
    Http.post
        { url = endpoints.audio
        , body = Http.jsonBody (setVolumeBody volume)
        , expect = Http.expectJson HandleApiResponse apiResponseDecoder
        }



-- AUDIO INPUT


changeAudioInputBody : String -> Encode.Value
changeAudioInputBody source =
    Encode.object
        [ ( "method", Encode.string "setPlayContent" )
        , ( "id", Encode.int 47 )
        , ( "version", Encode.string "1.2" )
        , ( "params", Encode.list Encode.object [ [ ( "uri", Encode.string source ) ] ] )
        ]


changeAudioInput : String -> Cmd Msg
changeAudioInput source =
    Http.post
        { url = endpoints.avContent
        , body = Http.jsonBody (changeAudioInputBody source)
        , expect = Http.expectJson HandleApiResponse apiResponseDecoder
        }



-- POWER STATUS


changePowerStatusBody : PowerStatus -> Encode.Value
changePowerStatusBody powerStatus =
    Encode.object
        [ ( "method", Encode.string "setPowerStatus" )
        , ( "id", Encode.int 55 )
        , ( "version", Encode.string "1.1" )
        , ( "params", Encode.list Encode.object [ [ ( "status", Encode.string (powerStatusToString powerStatus) ) ] ] )
        ]


changePowerStatus : PowerStatus -> Cmd Msg
changePowerStatus powerStatus =
    Http.post
        { url = endpoints.system
        , body = Http.jsonBody (changePowerStatusBody powerStatus)
        , expect = Http.expectJson HandleApiResponse apiResponseDecoder
        }
