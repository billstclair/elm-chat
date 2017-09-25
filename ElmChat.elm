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


module ElmChat exposing ( Settings, makeSettings, chat )

{-| This module contains a simple chat component
that you can easily add to your Elm user interface.

# Types
@docs Settings

# Functions
@docs makeSettings, chat
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
import Html.Events exposing ( onClick, onInput, onFocus, onCheck, on, keyCode )
import Dom.Scroll as Scroll
import Task

{-| Settings for the chat component
-}
type alias Settings msg =
    { fontSize : Int
    , chat : String
    , input : String
    , scroll : Float
    , id : String
    , updater : TheUpdater msg
    }

type alias Updater msg =
    Settings msg -> Cmd msg -> msg

type TheUpdater msg
    = TheUpdater (Updater msg)

{-| Create a chat component.
-}
chat : Settings msg -> List (Attribute msg) -> Html msg
chat settings attributes =
    text ""

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
    , chat = ""
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
scroll settings scroll =
    let s = scroll-1
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

---
--- Input boxes
---

keyDown : (String -> Settings msg -> msg) -> Settings msg -> Int -> msg
keyDown sender settings keycode =
    if keycode == 13 then
        sender settings.input { settings | input = "" }
    else
        noUpdate settings

