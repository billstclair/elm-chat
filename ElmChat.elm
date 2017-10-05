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

module ElmChat exposing ( Settings, ExtraAttributes, Updater, Sender
                        , makeSettings, chat, addChat, inputBox
                        , defaultExtraAttributes
                        , encodeSettings, settingsEncoder
                        , decodeSettings, settingsDecoder
                        , restoreScroll
                        )

{-| This module contains a simple chat component
that you can easily add to your Elm user interface.

# Types
@docs Settings, ExtraAttributes, Updater, Sender

# Functions
@docs makeSettings, chat, addChat, inputBox
@docs encodeSettings, settingsEncoder, decodeSettings, settingsDecoder
@docs restoreScroll

# Variables
@docs defaultExtraAttributes
-}

import Html exposing ( Html, Attribute
                     , text, span
                     , input, table, tr, th, td, button
                     , textarea
                     )
import Html.Attributes exposing ( value, size, title
                                , style, type_
                                , readonly, id
                                )
import Html.Events exposing ( onClick, onInput, on, keyCode )
import Dom.Scroll as Scroll
import Task
import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )


{-| Settings for the chat component.

Make one of these with `makeSettings`, and store it in your model.
-}
type alias Settings msg =
    { fontSize : Int
    , text : String
    , input : String
    , scroll : Float
    , attributes : ExtraAttributes msg
    , id : String
    , defaultFontSize : Int
    , showSizeControls : Bool
    , updater : TheUpdater msg
    }

{-| Extra attributes for the UI components.

This is the initial value of `Settings.attributes`.

You'll usually create one by changing `defaultAttributes`.
-}
type alias ExtraAttributes msg =
    { chatTable : List (Attribute msg)
    , sizeButtons : List (Attribute msg)
    , sizeColumn : List (Attribute msg)
    , textColumn : List (Attribute msg)
    , textArea : List (Attribute msg)
    }

{-| The default value of the `Settings.attributes` property.
-}
defaultExtraAttributes : ExtraAttributes msg
defaultExtraAttributes =
    { chatTable = []
    , sizeButtons = [ style [ ("font-weight", "bold") ] ]
    , sizeColumn = [ style [ ("text-align", "center")
                           , ("vertical-align", "top")
                           ]
                   ]
    , textColumn = []
    , textArea = [ style [ ("width", "30em")
                         , ("height", "6em")
                         ]
                 ]
    }

{-| A function to turn a `Settings` record and a `Cmd` into a `Msg`.
-}
type alias Updater msg =
    Settings msg -> Cmd msg -> msg

type TheUpdater msg
    = TheUpdater (Updater msg)

{-| Make a Settings record to add to your Model.

Args are `id initialFontSize showSizeControls updater`.

`id` is the Html id for the textarea showing the chat.

`initialFontSize` is the initial font size of the textarea in `pt`.

`showSizeControls` is `True` to show the font size controls to the left of the text area.

`updater` will be called to generate messages to update the Settings in your Model.
-}
makeSettings : String -> Int -> Bool -> Updater msg -> Settings msg
makeSettings id initialFontSize showSizeControls updater =
    { fontSize = initialFontSize
    , text = ""
    , input = ""
    , scroll = -1000
    , attributes = defaultExtraAttributes
    , id = id
    , defaultFontSize = initialFontSize
    , showSizeControls = showSizeControls
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
            let newSettings = { settings | scroll = s }
            in
                update
                    newSettings
                    (Task.attempt (\_ -> noUpdate newSettings)
                         <| Scroll.toBottom settings.id
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
                           settings.defaultFontSize
                       else
                           size - inc
    in
        noUpdate { settings | fontSize = newsize }

br : Html msg
br =
    Html.br [][]

chatSizeButton : Settings msg -> (Int, String, String) -> List (Html msg)
chatSizeButton settings (size, title_, label) =
    [ button (List.append
                  [ onClick <| setFontSize settings size
                  , title title_
                  ]
                  settings.attributes.sizeButtons
             )
          [ text label ]
    , br
    ]

{-| Create a chat component.
-}
chat : Settings msg -> Html msg
chat settings =
    table (settings.attributes.chatTable)
        [ tr []
              [ if settings.showSizeControls then
                    td settings.attributes.sizeColumn
                        <| List.concatMap (chatSizeButton settings)
                            [ (1, "Increase chat size", "^")
                            , (0, "Default chat size", "O")
                            , (-1, "Decrease chat size", "v")
                            ]
                else
                    text ""
              , td settings.attributes.textColumn
                  [ textarea
                        ( List.append
                              [ id settings.id
                              , style [ ( "font-size"
                                        , (toString settings.fontSize) ++ "pt"
                                        )
                                      ]
                              , readonly True
                              ]
                              settings.attributes.textArea
                        )
                        [ text <| settings.text ]
                  ]
              ]
        ]

{-| Add a line to the chat box.
-}
addChat : Settings msg -> String -> (Settings msg, Cmd msg)
addChat settings message =
    let newText = if settings.text == "" then
                      settings.text
                  else
                      settings.text ++ "\n"
        newSettings = { settings | text = newText ++ message }

    in
        ( newSettings
        , Task.attempt (\res ->
                            case res of
                                Ok amount -> scroll newSettings amount
                                Err _ -> noUpdate newSettings
                       )
              <| Scroll.y settings.id
        )

---
--- Input boxes
---

{-| A function to turn an input string and a `Settings` record into a `Msg`.
-}
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
  on "keydown" (JD.map tagger keyCode)

{-| Create a text input control.

Args are `textSize buttonText sender settings`.

`textSize` is the width in characters of the input control.

`buttonText` is the text for the button that sends the input.

`sender` is a function to turn an input string and `settings into a `Msg`.

`settings` is your `Settings` record.
-}
inputBox : Int -> String -> Sender msg -> Settings msg -> Html msg
inputBox textSize buttonText sender settings =
    span []
        [ input [ type_ "text"
                , onInput (\text -> noUpdate { settings | input = text } )
                , onKeydown <| keyDown sender settings
                , size textSize
                , value settings.input
                ]
              []
        , text " "
        , button [ onClick <| send sender settings ]
            [ text buttonText ]
    ]

---
--- Encoder & Decoder
---

{-| Turn chat `Settings` into a JSON string.

Does not encode the `attributes` or `updater` properties.
-}
encodeSettings : Settings msg -> String
encodeSettings settings =
    JE.encode 0 <| settingsEncoder settings

{-| The JSON encoder for `encodeSettings`.
-}
settingsEncoder : Settings msg -> Value
settingsEncoder settings =
    JE.object
        [ ("fontSize", JE.int settings.fontSize)
        , ("text", JE.string settings.text)
        , ("input", JE.string settings.input)
        , ("scroll", JE.float settings.scroll)
        , ("id", JE.string settings.id)
        , ("defaultFontSize", JE.int settings.defaultFontSize)
        , ("showSizeControls", JE.bool settings.showSizeControls)
        ]

{-| Turn a JSON string back into a `Settings` record.

`Updater` is as to `makeSettings`.

Restores with default `attributes`, so you'll need to change those
after decoding, if you customize them.
-}
decodeSettings : Updater msg -> String -> Result String (Settings msg)
decodeSettings updater json =
    JD.decodeString (settingsDecoder updater) json

restoreSettings : Updater msg -> Int -> String -> String -> Float -> String -> Int -> Bool -> Settings msg
restoreSettings updater fontSize text input scroll id defaultFontSize showSizeControls =
    { fontSize = fontSize
    , text = text
    , input = input
    , scroll = scroll
    , attributes = defaultExtraAttributes
    , id = id
    , defaultFontSize = defaultFontSize
    , showSizeControls = showSizeControls
    , updater = TheUpdater updater
    }
    
{-| The JSON Decoder for `decodeSettings`.
-}
settingsDecoder : Updater msg -> Decoder (Settings msg)
settingsDecoder updater =
    JD.map8 restoreSettings
        (JD.succeed updater)
        (JD.field "fontSize" JD.int)
        (JD.field "text" JD.string)
        (JD.field "input" JD.string)
        (JD.field "scroll" JD.float)
        (JD.field "id" JD.string)
        (JD.field "defaultFontSize" JD.int)
        (JD.field "showSizeControls" JD.bool)

---
--- Restore scroll
---

{-| Restore the scroll position after restoring from a JSON string.
-}
restoreScroll : Settings msg -> Cmd msg
restoreScroll settings =
    Task.attempt (\_ -> noUpdate settings)
         <| Scroll.toY settings.id (settings.scroll + 1)
