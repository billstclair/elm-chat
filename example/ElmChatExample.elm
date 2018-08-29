----------------------------------------------------------------------
--
-- ElmChatExample.elm
-- Example of using the ElmChat module.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ElmChatExample exposing (Model, Msg(..), Settings, b, center, chatSend, chatSendInternal, copyright, defaultAttributes, delayedAction, init, main, nbsp, stringFromCode, update, view)

import Browser
import Char
import Debug exposing (log)
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , h2
        , input
        , p
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (disabled, href, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time exposing (Posix)


main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Settings =
    ElmChat.Settings Msg


type alias Model =
    { settings : Settings

    -- Your code won't need a duplicate settings property.
    -- It's here only because this example has two input boxes.
    , settings2 : ElmChat.Settings Msg
    , time : Posix
    , json : String
    , error : Maybe String
    }


type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend1 String String (ElmChat.Settings Msg)
      -- Your code won't need these additional messages.
      -- They're here only because this examples has two input boxes.
    | ChatUpdate2 (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend2 String String (ElmChat.Settings Msg)
    | UpdateJson String
    | Restore
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix


defaultAttributes =
    { defaultExtraAttributes
        | textArea =
            style "border-color" "blue"
                :: defaultExtraAttributes.textArea
    }


init : ( Model, Cmd Msg )
init =
    let
        settings =
            ElmChat.makeSettings "id1" 14 True ChatUpdate

        settings2 =
            { settings | attributes = defaultAttributes }
    in
    ( { settings = settings2
      , settings2 =
            { settings2
                | id = "id2"
                , updater = ElmChat.TheUpdater ChatUpdate2
            }
      , time = Time.millisToPosix 0
      , json = ""
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChatUpdate settings cmd ->
            ( { model | settings = log "update" settings }
            , cmd
            )

        ChatSend1 name line settings ->
            chatSend name line settings model.settings2 model

        ChatUpdate2 settings cmd ->
            ( { model | settings2 = settings }
            , cmd
            )

        ChatSend2 name line settings2 ->
            chatSend name line model.settings settings2 model

        UpdateJson json ->
            ( { model | json = json }
            , Cmd.none
            )

        Restore ->
            case ElmChat.decodeSettings ChatUpdate model.json of
                Err m ->
                    ( { model | error = Just m }
                    , Cmd.none
                    )

                Ok settings ->
                    ( { model
                        | settings = settings
                        , error = Nothing
                      }
                    , ElmChat.restoreScroll settings
                    )

        DelayedAction updater time ->
            updater { model | time = time }


chatSend : String -> String -> Settings -> Settings -> Model -> ( Model, Cmd Msg )
chatSend name line settings1 settings2 model =
    ( model
    , delayedAction <|
        chatSendInternal name line settings1 settings2
    )


chatSendInternal : String -> String -> Settings -> Settings -> Model -> ( Model, Cmd Msg )
chatSendInternal name line settings1 settings2 model =
    let
        ( settings, cmd ) =
            ElmChat.addLineSpec settings1 <|
                ElmChat.makeLineSpec line (Just name) (Just model.time)
    in
    ( { model
        | settings = settings1
        , settings2 = settings2
      }
    , cmd
    )


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


center : List (Attribute msg) -> List (Html msg) -> Html msg
center attributes body =
    Html.node "center" attributes body


view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "ElmChat Example" ]
        , p [] [ ElmChat.chat model.settings ]
        , p []
            [ table []
                [ tr []
                    [ td [] [ b "Bill: " ]
                    , td []
                        [ ElmChat.inputBox
                            40
                            "Send"
                            (ChatSend1 "Bill")
                            model.settings
                        ]
                    ]
                , tr []
                    [ td [] [ b "Mary: " ]
                    , td []
                        [ ElmChat.inputBox
                            40
                            "Send"
                            (ChatSend2 "Mary")
                            model.settings2
                        ]
                    ]
                , tr []
                    [ td [] [ b "JSON: " ]
                    , td []
                        [ input
                            [ type_ "text"
                            , disabled True
                            , size 40
                            , value <| ElmChat.encodeSettings model.settings
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] []
                    , td []
                        [ input
                            [ type_ "text"
                            , onInput UpdateJson
                            , size 40
                            , value model.json
                            ]
                            []
                        , text " "
                        , button [ onClick Restore ]
                            [ text "Restore" ]
                        ]
                    ]
                ]
            ]
        , p [ style "color" "red" ]
            [ case model.error of
                Nothing ->
                    text nbsp

                Just msg ->
                    text msg
            ]
        , p []
            [ text <| "Copyright " ++ copyright ++ " 2017-2018 Bill St. Clair"
            , br [] []
            , a [ href "https://gibgoygames.com/" ]
                [ text "Gib Goy Games" ]
            , text " "
            , a [ href "https://github.com/billstclair/elm-chat" ]
                [ text "GitHub" ]
            ]
        ]


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


nbsp : String
nbsp =
    stringFromCode 160



-- \u00A0


copyright : String
copyright =
    stringFromCode 169



-- \u00A9
