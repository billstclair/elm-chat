----------------------------------------------------------------------
--
-- ElmChat.elm
-- Simple chat component
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ElmChat exposing ( Settings, makeSettings, chat, addChat, inputBox )

{-| This module contains a simple chat component
that you can easily add to your Elm user interface.

# Types
@docs Settings

# Functions
@docs makeSettings, chat, addChat, inputBox
-}

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3, a, node
                     , input, table, tr, th, td, button
                     , textarea
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected, type_, name, checked
                                , placeholder, disabled, target
                                , width, height, class
                                , readonly, id
                                )
import Html.Events exposing ( onClick, onInput, on, keyCode )
import Dom.Scroll as Scroll
import Task
import Json.Decode as Json

{-| Settings for the chat component
-}
type alias Settings msg =
    { fontSize : Int
    , text : String
    , input : String
    , scroll : Float
    , id : String
    , updater : TheUpdater msg
    }

type alias Updater msg =
    Settings msg -> Cmd msg -> msg

type TheUpdater msg
    = TheUpdater (Updater msg)

defaultFontSize : Int
defaultFontSize =
    20

{-| Make a Settings record to add to your Model.
id is the Html id for the textarea showing the chat.
updater will be called to generate messages to update the Settings in your Model.
-}
makeSettings : String -> Updater msg -> Settings msg
makeSettings id updater =
    { fontSize = defaultFontSize
    , text = ""
    , input = ""
    , scroll = -1000
    , id = id
    , updater = TheUpdater updater
    }

update : Settings msg -> Cmd msg -> msg
update settings cmd =
    case settings.updater of
        TheUpdater updater ->
            updater settings cmd

noUpdate : Settings msg -> msg
noUpdate settings =
    update settings Cmd.none

scroll : Settings msg -> Float -> msg
scroll settings amount =
    let s = amount-1
    in
        if s >= settings.scroll then
            update
                { settings | scroll = s }
                (Task.attempt (\_ -> noUpdate settings)
                     <| Scroll.toBottom "chat"
                )
        else
            noUpdate settings

setFontSize : Settings msg -> Int -> msg
setFontSize settings dir =
    let size = settings.fontSize
        inc = 4
        newsize = if dir > 0 then
                      size + inc
                  else if dir == 0 then
                           defaultFontSize
                       else
                           size - inc
    in
        update
            { settings | fontSize = newsize }
            Cmd.none

br : Html msg
br =
    Html.br [][]

chatSizeButton : Settings msg -> (Int, String, String) -> List (Html msg)
chatSizeButton settings (size, title_, label) =
    [ button [ onClick <| setFontSize settings size
             , title title_
             --, class "chatsizebutton"
             ]
          [ text label ]
    , br
    ]

{-| Create a chat component.
-}
chat : Settings msg -> Html msg
chat settings =
    table []
        [ tr []
              [ td [ class "chatsizecolumn" ]
                    <| List.concatMap (chatSizeButton settings)
                    [ (1, "Increase chat size", "^")
                    , (0, "Default chat size", "O")
                    , (-1, "Decrease chat size", "v")
                    ]
              , td []
                  [ textarea [ id "chat"
                             , class "chat"
                             , readonly True
                             ]
                        -- TODO: make player names bold.
                        [ text <| settings.text ]
                  ]
              ]
        ]

{-| Add a line to the chat box.
-}
addChat : Settings msg -> String -> msg
addChat settings message =
    let newText = if settings.text == "" then
                      settings.text
                  else
                      settings.text ++ "\n"
        newSettings = { settings | text = newText ++ message }

    in
        update
            newSettings
            <| Task.attempt (\res ->
                                 case res of
                                     Ok amount -> scroll settings amount
                                     Err _ -> noUpdate settings
                            )
                <| Scroll.y "chat"

---
--- Input boxes
---

type alias Sender msg =
    String -> Settings msg -> msg

keyDown : Sender msg -> Settings msg -> Int -> msg
keyDown sender settings keycode =
    if keycode == 13 then
        sender settings.input { settings | input = "" }
    else
        noUpdate settings

send : Sender msg -> Settings msg -> msg
send sender settings =
    sender settings.input { settings | input = "" }

onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
  on "keydown" (Json.map tagger keyCode)

{-| Create a text input area of the given textSize (characters),
with a send button with the given buttonText.
-}
inputBox : Int -> String -> Sender msg -> Settings msg -> Html msg
inputBox textSize buttonText sender settings =
    span []
        [ input [ type_ "text"
                , onInput (\text -> noUpdate { settings | text = text } )
                , onKeydown <| keyDown sender settings
                , size textSize
                , value settings.input
                ]
              []
        , text " "
        , button [ onClick <| send sender settings ]
            [ text buttonText ]
    ]
