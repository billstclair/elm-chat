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

module ElmChatExample exposing (..)

import ElmChat

import Html exposing ( Html
                     , div, text, p, h2, table, tr, th, td
                     )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { settings1 : ElmChat.Settings Msg
    , settings2 : ElmChat.Settings Msg
    }

type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend1 String String (ElmChat.Settings Msg)
    | ChatUpdate2 (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend2 String String (ElmChat.Settings Msg)

init : ( Model, Cmd Msg )
init =
    ( { settings1 = ElmChat.makeSettings "id1" 14 True ChatUpdate
      , settings2 = ElmChat.makeSettings "id2" 14 True ChatUpdate2
      }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChatUpdate settings cmd ->
            ( { model | settings1 = settings }
            , cmd
            )
        ChatSend1 name line settings ->
            let (settings1, cmd) =
                    ElmChat.addChat settings (name ++ ": " ++ line)
            in
                ( { model | settings1 = settings1 }
                , cmd
                )
        ChatUpdate2 settings cmd ->
            ( { model | settings2 = settings }
            , cmd
            )
        ChatSend2 name line settings2 ->
            let (settings1, cmd) =
                    ElmChat.addChat model.settings1 (name ++ ": " ++ line)
            in
                ( { model
                      | settings1 = settings1
                      , settings2 = settings2
                  }
                , cmd
                )

b : String -> Html Msg
b string =
    Html.b [] [ text string ]

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "ElmChat Example" ]
        , p [] [ ElmChat.chat model.settings1 ]
        , p []
            [ table []
                  [ tr []
                        [ td [] [ b "Bill: " ]
                        , td []
                            [ ElmChat.inputBox
                                  40 "Send" (ChatSend1 "Bill") model.settings1
                            ]
                        ]
                  , tr []
                      [ td [] [ b "Mary: " ]
                      , td []
                          [ ElmChat.inputBox
                                40 "Send" (ChatSend2 "Mary") model.settings2
                          ]
                      ]
                  ]
            ]
        ]
