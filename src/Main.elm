module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, disabled, max, min, name, step, type_, value)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, map2, string)
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- CUSTOM TYPES


type PowerStatus
    = On
    | Off


stringToPowerStatus : String -> PowerStatus
stringToPowerStatus powerStatus =
    case powerStatus of
        "active" ->
            On

        "off" ->
            Off

        _ ->
            Off


powerStatusToString : PowerStatus -> String
powerStatusToString powerStatus =
    case powerStatus of
        On ->
            "active"

        Off ->
            "off"


type AudioInput
    = HDMI
    | Bluetooth
    | AudioIn


audioInputToString : AudioInput -> String
audioInputToString audioInput =
    case audioInput of
        HDMI ->
            "extInput:hdmi"

        Bluetooth ->
            "extInput:btAudio"

        AudioIn ->
            "extInput:line?port=1"


stringToAudioInput : String -> AudioInput
stringToAudioInput audioInput =
    case audioInput of
        "extInput:hdmi" ->
            HDMI

        "extInput:btAudio" ->
            Bluetooth

        "extInput:line?port=1" ->
            AudioIn

        _ ->
            AudioIn



-- MODEL


type alias Model =
    { volume : String
    , volumeTemp : String
    , mute : Bool
    , apiResponse : ApiResponse
    , error : String
    , audioInput : AudioInput
    , powerStatus : PowerStatus
    }



-- TODO: load initial data from the api


init : () -> ( Model, Cmd Msg )
init _ =
    ( { volume = "5"
      , volumeTemp = ""
      , mute = False
      , apiResponse = { result = [ "" ], id = -1 }
      , error = ""
      , audioInput = HDMI
      , powerStatus = Off
      }
    , getPowerStatus
    )



--  UPDATE


type Msg
    = SetVolume String
    | SetMute Bool
    | HandleApiResponse (Result Http.Error ApiResponse)
    | HandlePowerStatusResponse (Result Http.Error PowerStatusRes)
    | ChangeAudioInput AudioInput
    | ChangePowerStatus PowerStatus



-- TODO: update model based on the server response


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMute True ->
            ( { model | mute = True, volumeTemp = model.volume, volume = "0" }, setVolume "0" )

        SetMute False ->
            ( { model | mute = False, volume = model.volumeTemp, volumeTemp = "" }, setVolume model.volumeTemp )

        SetVolume volume ->
            ( { model | volume = volume }, setVolume volume )

        ChangeAudioInput audioInput ->
            ( { model | audioInput = audioInput }, changeAudioInput audioInput )

        ChangePowerStatus powerStatus ->
            ( { model | powerStatus = powerStatus }, changePowerStatus powerStatus )

        HandleApiResponse (Ok rec) ->
            ( { model | apiResponse = rec }, Cmd.none )

        HandleApiResponse (Err err) ->
            ( { model | error = "ERROR:" ++ Debug.toString err }, Cmd.none )

        HandlePowerStatusResponse response ->
            case response of
                Ok powerStatus ->
                    ( { model
                        | powerStatus =
                            case List.head powerStatus.result of
                                Just status ->
                                    stringToPowerStatus status.status

                                Nothing ->
                                    Off
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = "ERROR:" ++ Debug.toString err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ volumeControlView model
        , audioInputView model.audioInput model.powerStatus
        , powerStatusView model.powerStatus
        , text model.error
        , text (Debug.toString model.apiResponse)
        ]


volumeControlView : Model -> Html Msg
volumeControlView model =
    div [ class "view-audio-control" ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked (model.mute == True)
                , onCheck (\isChecked -> SetMute isChecked)
                , disabled (model.powerStatus == Off)
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
                , disabled (model.powerStatus == Off)
                ]
                []
            , text ("Volume: " ++ model.volume)
            ]
        ]


audioInputView : AudioInput -> PowerStatus -> Html Msg
audioInputView audioInput powerStatus =
    div []
        [ label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:btAudio"
                , onInput (\value -> ChangeAudioInput (stringToAudioInput value))
                , checked (audioInput == Bluetooth)
                , disabled (powerStatus == Off)
                ]
                []
            , text "Bluetooth"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:line?port=1"
                , onInput (\value -> ChangeAudioInput (stringToAudioInput value))
                , checked (audioInput == AudioIn)
                , disabled (powerStatus == Off)
                ]
                []
            , text "Audio in (Port 1)"
            ]
        , label []
            [ input
                [ type_ "radio"
                , name "source-input"
                , value "extInput:hdmi"
                , onInput (\value -> ChangeAudioInput (stringToAudioInput value))
                , checked (audioInput == HDMI)
                , disabled (powerStatus == Off)
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
                , checked (powerStatus == On)
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
changeAudioInputBody audioInput =
    Encode.object
        [ ( "method", Encode.string "setPlayContent" )
        , ( "id", Encode.int 47 )
        , ( "version", Encode.string "1.2" )
        , ( "params", Encode.list Encode.object [ [ ( "uri", Encode.string audioInput ) ] ] )
        ]


changeAudioInput : AudioInput -> Cmd Msg
changeAudioInput audioInput =
    Http.post
        { url = endpoints.avContent
        , body = Http.jsonBody (changeAudioInputBody (audioInputToString audioInput))
        , expect = Http.expectJson HandleApiResponse apiResponseDecoder
        }



-- CHANGE POWER STATUS


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



-- GET POWER STATUS


type alias PowerStatusRes =
    { result : List { status : String }, id : Int }


type alias PowerStatusResStatus =
    { status : String }


powerStatusResStatusDec : Decoder PowerStatusResStatus
powerStatusResStatusDec =
    map PowerStatusResStatus
        (field "status" string)


powerStatusDecoder : Decoder PowerStatusRes
powerStatusDecoder =
    map2 PowerStatusRes
        (field "result" (list powerStatusResStatusDec))
        (field "id" int)


getPowerStatusBody : Encode.Value
getPowerStatusBody =
    Encode.object
        [ ( "method", Encode.string "getPowerStatus" )
        , ( "id", Encode.int 50 )
        , ( "params", Encode.list identity [] )
        , ( "version", Encode.string "1.1" )
        ]


getPowerStatus : Cmd Msg
getPowerStatus =
    Http.post
        { url = endpoints.system
        , body = Http.jsonBody getPowerStatusBody
        , expect = Http.expectJson HandlePowerStatusResponse powerStatusDecoder
        }
