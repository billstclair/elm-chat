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

import Html exposing ( Html, Attribute
                     , div, text, p, h2, table, tr, th, td, a, br
                     )
import Html.Attributes exposing ( href )
import Char

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { settings : ElmChat.Settings Msg
    -- Your code won't need a duplicate settings property.
    -- It's here only because this example has two input boxes.
    , settings2 : ElmChat.Settings Msg
    }

type Msg
    = ChatUpdate (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend1 String String (ElmChat.Settings Msg)
    -- Your code won't need these additional messages.
    -- They're here only because this examples has two input boxes.
    | ChatUpdate2 (ElmChat.Settings Msg) (Cmd Msg)
    | ChatSend2 String String (ElmChat.Settings Msg)

init : ( Model, Cmd Msg )
init =
    ( { settings = ElmChat.makeSettings "id1" 14 True ChatUpdate
      , settings2 = ElmChat.makeSettings "id2" 14 True ChatUpdate2
      }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChatUpdate settings cmd ->
            ( { model | settings = settings }
            , cmd
            )
        ChatSend1 name line settings ->
            let (settings1, cmd) =
                    ElmChat.addChat settings (name ++ ": " ++ line)
            in
                ( { model | settings = settings1 }
                , cmd
                )
        ChatUpdate2 settings cmd ->
            ( { model | settings2 = settings }
            , cmd
            )
        ChatSend2 name line settings2 ->
            let (settings, cmd) =
                    ElmChat.addChat model.settings (name ++ ": " ++ line)
            in
                ( { model
                      | settings = settings
                      , settings2 = settings2
                  }
                , cmd
                )

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
                                  40 "Send" (ChatSend1 "Bill") model.settings
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
        , p []
            [ text <| "Copyright " ++ copyright ++ " 2017 Bill St. Clair"
            , br [][]
            , a [ href "https://gibgoygames.com/" ]
                [ text "Gib Goy Games" ]
            , text " "
            , a [ href "https://github.com/billstclair/elm-chat" ]
                [ text "GitHub" ]
            ]
        ]

stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ (Char.fromCode code) ]

nbsp : String
nbsp =
    stringFromCode 160  -- \u00A0

copyright : String
copyright =
    stringFromCode 169  -- \u00A9
