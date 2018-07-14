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


module ElmChat
    exposing
        ( CustomRenderers(..)
        , CustomSettings
        , ExtraAttributes
        , LineSpec(..)
        , Overrider
        , Sender
        , Settings
        , StateRenderer
        , TheUpdater(..)
        , Updater
        , addChat
        , addLineSpec
        , chat
        , customSettingsDecoder
        , customSettingsEncoder
        , decodeCustomSettings
        , decodeSettings
        , defaultExtraAttributes
        , encodeCustomSettings
        , encodeSettings
        , inputBox
        , makeLineSpec
        , makeSettings
        , parseOutUrl
        , restoreScroll
        , settingsDecoder
        , settingsEncoder
        , timeString
        , timestampString
        )

{-| This module contains a simple chat component that you can easily add to your Elm user interface.


# Types

@docs Settings, ExtraAttributes, Updater, Sender, LineSpec


# Less used types

@docs TheUpdater, CustomSettings


# Functions

@docs makeSettings, chat, addChat, addLineSpec, inputBox, makeLineSpec
@docs encodeSettings, settingsEncoder, decodeSettings, settingsDecoder
@docs restoreScroll


# Utilities

@docs timeString, timestampString, parseOutUrl


# Custom rendering

@docs CustomRenderers, Overrider, StateRenderer
@docs encodeCustomSettings, customSettingsEncoder
@docs decodeCustomSettings, customSettingsDecoder


# Variables

@docs defaultExtraAttributes

-}

import Date exposing (Date)
import Dom.Scroll as Scroll
import Formatting as F exposing ((<>), Format)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , b
        , button
        , div
        , input
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( href
        , id
        , readonly
        , size
        , style
        , target
        , title
        , type_
        , value
        )
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Regex exposing (HowMany(..), Regex, regex)
import Task
import Time exposing (Time)


{-| Represents a single line in a chat.
-}
type LineSpec state
    = StringLineSpec String
    | UrlLineSpec String
    | UserLineSpec
        { user : String
        , linespec : LineSpec state
        }
    | TimeLineSpec
        { time : Time
        , linespec : LineSpec state
        }
    | CustomLineSpec state


{-| Make a `LineSpec` including a message and an optional user and time.
-}
makeLineSpec : String -> Maybe String -> Maybe Time -> LineSpec state
makeLineSpec message user time =
    case time of
        Just t ->
            TimeLineSpec
                { time = t
                , linespec = makeLineSpec message user Nothing
                }

        Nothing ->
            case user of
                Nothing ->
                    UrlLineSpec message

                Just user ->
                    UserLineSpec
                        { user = user
                        , linespec = UrlLineSpec message
                        }


urlRegex : Regex
urlRegex =
    regex "(.*\\s+|^)([A-Za-z]*://\\S*|\\w+\\.[A-Za-z]+\\S*)(.*)"


{-| Parse the first URL out of a string.

The result is `Just (prefix, url, suffix)`, if there is a URL, or `Nothing` otherwise.

"foo.com" is interpreted as a URL, but is returned as just "foo.com". You have to prepend the <http://> yourself, if that's what you need.

-}
parseOutUrl : String -> Maybe ( String, String, String )
parseOutUrl string =
    case Regex.find (AtMost 1) urlRegex string of
        [ match ] ->
            case match.submatches of
                [ Just prefix, Just url, Just suffix ] ->
                    let
                        ( realUrl, urlSuffix ) =
                            trimUrl url
                    in
                    Just ( prefix, realUrl, urlSuffix ++ suffix )

                _ ->
                    Nothing

        _ ->
            Nothing


trimUrl : String -> ( String, String )
trimUrl url =
    let
        lastChar =
            String.right 1 url
    in
    if List.member lastChar [ ".", "?", "!" ] then
        ( String.dropRight 1 url, lastChar )
    else
        ( url, "" )


{-| Used to customize rendering.

Set the `customRenderers` field of your settings record to one of these, if you want to override the default rendering, or render your own CustomLineSpec.

The `overrider` can provide an alternate to the default renderers for `StringLineSpec`, `UserLineSpec`, and `TimeLineSpec`.

The `renderer` will be called on `CustomLineSpec`s.

-}
type CustomRenderers state msg
    = CustomRenderers
        { overrider : Maybe (Overrider state msg)
        , renderer : Maybe (StateRenderer state msg)
        }


{-| User function to override rendering of standard `LineSpec` options.
-}
type alias Overrider state msg =
    CustomSettings state msg -> LineSpec state -> Maybe (Html msg)


{-| User function to render `CustomLineSpec` state.
-}
type alias StateRenderer state msg =
    state -> CustomSettings state msg -> Html msg


{-| Customizable Settings.

Make one of these with `makeSettings`, optionally customize the `customRenderers` field, and store it in your model.

-}
type alias CustomSettings state msg =
    { fontSize : Int
    , lines : List (LineSpec state)
    , customRenderers : Maybe (CustomRenderers state msg)
    , input : String
    , scroll : Float
    , attributes : ExtraAttributes msg
    , id : String
    , defaultFontSize : Int
    , showSizeControls : Bool
    , updater : TheUpdater state msg
    }


{-| Old, uncustomizable Settings for the chat component.

Make one of these with `makeSettings`, and store it in your model.

-}
type alias Settings msg =
    CustomSettings () msg


{-| Extra attributes for the UI components.

This is the initial value of `Settings.attributes`.

You'll usually create one by changing `defaultExtraAttributes`.

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
    , sizeButtons = [ style [ ( "font-weight", "bold" ) ] ]
    , sizeColumn =
        [ style
            [ ( "text-align", "center" )
            , ( "vertical-align", "top" )
            ]
        ]
    , textColumn = []
    , textArea =
        [ style
            [ ( "width", "30em" )
            , ( "height", "6em" )
            ]
        ]
    }


{-| A function to turn a `Settings` record and a `Cmd` into a `Msg`.
-}
type alias Updater state msg =
    CustomSettings state msg -> Cmd msg -> msg


{-| A wrapper around Updater to prevent type recursion.
-}
type TheUpdater state msg
    = TheUpdater (Updater state msg)


{-| Make a settings record to add to your Model.

Args are `id initialFontSize showSizeControls updater`.

`id` is the Html id for the div showing the chat.

`initialFontSize` is the initial font size of the div in `pt`.

`showSizeControls` is `True` to show the font size controls to the left of the text area.

`updater` will be called to generate messages to update the settings in your Model.

-}
makeSettings : String -> Int -> Bool -> Updater state msg -> CustomSettings state msg
makeSettings id initialFontSize showSizeControls updater =
    { fontSize = initialFontSize
    , lines = []
    , customRenderers = Nothing
    , input = ""
    , scroll = -1000
    , attributes = defaultExtraAttributes
    , id = id
    , defaultFontSize = initialFontSize
    , showSizeControls = showSizeControls
    , updater = TheUpdater updater
    }


update : CustomSettings state msg -> Cmd msg -> msg
update settings cmd =
    case settings.updater of
        TheUpdater updater ->
            updater settings cmd


noUpdate : CustomSettings state msg -> msg
noUpdate settings =
    update settings Cmd.none


getScroll : CustomSettings state msg -> msg
getScroll settings =
    update settings
        (Task.attempt
            (\result ->
                case result of
                    Err _ ->
                        noUpdate settings

                    Ok y ->
                        noUpdate { settings | scroll = y }
            )
         <|
            Scroll.y settings.id
        )


scroll : CustomSettings state msg -> Float -> msg
scroll settings amount =
    let
        s =
            amount
    in
    if s >= settings.scroll then
        let
            newSettings =
                { settings | scroll = s }
        in
        update newSettings
            (Task.attempt (\_ -> getScroll newSettings) <|
                Scroll.toBottom settings.id
            )
    else
        noUpdate settings


setFontSize : CustomSettings state msg -> Int -> msg
setFontSize settings dir =
    let
        size =
            settings.fontSize

        inc =
            4

        newsize =
            if dir > 0 then
                size + inc
            else if dir == 0 then
                settings.defaultFontSize
            else
                size - inc
    in
    noUpdate { settings | fontSize = newsize }


br : Html msg
br =
    Html.br [] []


chatSizeButton : CustomSettings state msg -> ( Int, String, String ) -> List (Html msg)
chatSizeButton settings ( size, title_, label ) =
    [ button
        (List.append
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
chat : CustomSettings state msg -> Html msg
chat settings =
    table settings.attributes.chatTable
        [ tr []
            [ if settings.showSizeControls then
                td settings.attributes.sizeColumn <|
                    List.concatMap (chatSizeButton settings)
                        [ ( 1, "Increase chat size", "^" )
                        , ( 0, "Default chat size", "O" )
                        , ( -1, "Decrease chat size", "v" )
                        ]
              else
                text ""
            , td settings.attributes.textColumn
                [ div
                    (List.append
                        [ id settings.id
                        , style
                            [ ( "font-size"
                              , toString settings.fontSize ++ "pt"
                              )
                            , ( "overflow-y", "scroll" )
                            , ( "border", "1px black solid" )
                            , ( "resize", "both" )
                            ]
                        , readonly True
                        ]
                        settings.attributes.textArea
                    )
                  <|
                    List.map lineDiv <|
                        List.map (renderLineSpec settings) settings.lines
                ]
            ]
        ]


lineDiv : Html msg -> Html msg
lineDiv line =
    div [ style [ ( "padding-bottom", "2px" ) ] ]
        [ line ]


renderLineSpec : CustomSettings state msg -> LineSpec state -> Html msg
renderLineSpec settings linespec =
    case settings.customRenderers of
        Nothing ->
            renderLineSpecInternal settings linespec

        Just (CustomRenderers { overrider }) ->
            case overrider of
                Nothing ->
                    renderLineSpecInternal settings linespec

                Just renderer ->
                    case renderer settings linespec of
                        Nothing ->
                            renderLineSpecInternal settings linespec

                        Just html ->
                            html


renderLineSpecInternal : CustomSettings state msg -> LineSpec state -> Html msg
renderLineSpecInternal settings linespec =
    case linespec of
        StringLineSpec string ->
            text string

        UrlLineSpec string ->
            renderStringWithUrls string

        UserLineSpec { user, linespec } ->
            span []
                [ b []
                    [ text user
                    , text ": "
                    ]
                , renderLineSpec settings linespec
                ]

        TimeLineSpec { time, linespec } ->
            span []
                [ span
                    [ style [ ( "font-size", "75%" ) ]
                    ]
                    [ text "["
                    , span [ style [ ( "font-family", "monospace" ) ] ]
                        [ text <| timeString time ]
                    , text "]"
                    ]
                , text " "
                , renderLineSpec settings linespec
                ]

        CustomLineSpec state ->
            case settings.customRenderers of
                Nothing ->
                    text ""

                Just (CustomRenderers { renderer }) ->
                    case renderer of
                        Nothing ->
                            text ""

                        Just r ->
                            r state settings


renderStringWithUrls : String -> Html msg
renderStringWithUrls string =
    let
        loop : String -> List (Html msg) -> Html msg
        loop =
            \tail res ->
                if tail == "" then
                    span [] res
                else
                    case parseOutUrl tail of
                        Nothing ->
                            loop "" <|
                                List.concat [ res, [ text tail ] ]

                        Just ( prefix, url, suffix ) ->
                            let
                                withProtocol =
                                    if String.contains "://" url then
                                        url
                                    else
                                        "http://" ++ url
                            in
                            loop suffix <|
                                List.concat
                                    [ res
                                    , [ text prefix
                                      , a [ href withProtocol, target "_blank" ]
                                            [ text url ]
                                      ]
                                    ]
    in
    loop string []


hourFormat : Format Date String
hourFormat =
    F.padLeft 2 '0' (Date.hour >> F.int)


minuteFormat : Format Date String
minuteFormat =
    F.padLeft 2 '0' (Date.minute >> F.int)


secondFormat : Format Date String
secondFormat =
    F.padLeft 2 '0' (Date.second >> F.int)


timeStampFormat : Format Date String
timeStampFormat =
    hourFormat <> F.s ":" <> minuteFormat <> F.s ":" <> secondFormat


timeFormat : Format Date String
timeFormat =
    hourFormat <> F.s ":" <> minuteFormat


{-| Convert a `Time` to a string in the format "HH:MM:SS"
-}
timestampString : Time -> String
timestampString time =
    let
        time2 =
            time

        date =
            Date.fromTime time2
    in
    F.print timeStampFormat date


{-| Convert a `Time` to a string in the format "HH:MM"
-}
timeString : Time -> String
timeString time =
    let
        time2 =
            time

        date =
            Date.fromTime time2
    in
    F.print timeFormat date


{-| Add a string to the chat box.
-}
addChat : CustomSettings state msg -> String -> ( CustomSettings state msg, Cmd msg )
addChat settings message =
    addLineSpec settings <| StringLineSpec message


{-| Add a line to the chat box.
-}
addLineSpec : CustomSettings state msg -> LineSpec state -> ( CustomSettings state msg, Cmd msg )
addLineSpec settings linespec =
    let
        newSettings =
            { settings
                | lines =
                    List.concat
                        [ settings.lines
                        , [ linespec ]
                        ]
            }
    in
    ( newSettings
    , Task.attempt
        (\res ->
            case res of
                Ok amount ->
                    scroll newSettings amount

                Err _ ->
                    noUpdate newSettings
        )
      <|
        Scroll.y settings.id
    )



---
--- Input boxes
---


{-| A function to turn an input string and a `Settings` record into a `Msg`.
-}
type alias Sender state msg =
    String -> CustomSettings state msg -> msg


keyDown : Sender state msg -> CustomSettings state msg -> Int -> msg
keyDown sender settings keycode =
    if keycode == 13 then
        sender settings.input { settings | input = "" }
    else
        noUpdate settings


send : Sender state msg -> CustomSettings state msg -> msg
send sender settings =
    sender settings.input { settings | input = "" }


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (JD.map tagger keyCode)


{-| Create a text input control.

Args are `textSize buttonText sender settings`.

`textSize` is the width in characters of the input control.

`buttonText` is the text for the button that sends the input.

`sender` is a function to turn an input string and `settings into a`Msg`.

`settings` is your `Settings` record.

-}
inputBox : Int -> String -> Sender state msg -> CustomSettings state msg -> Html msg
inputBox textSize buttonText sender settings =
    span []
        [ input
            [ type_ "text"
            , onInput (\text -> noUpdate { settings | input = text })
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
encodeSettings : CustomSettings state msg -> String
encodeSettings settings =
    JE.encode 0 <| settingsEncoder settings


{-| The JSON encoder for `encodeSettings`.
-}
settingsEncoder : CustomSettings state msg -> Value
settingsEncoder settings =
    JE.object
        [ ( "fontSize", JE.int settings.fontSize )
        , ( "lines", JE.list <| List.map lineSpecEncoder settings.lines )
        , ( "scroll", JE.float settings.scroll )
        , ( "id", JE.string settings.id )
        , ( "defaultFontSize", JE.int settings.defaultFontSize )
        , ( "showSizeControls", JE.bool settings.showSizeControls )
        ]


lineSpecEncoder : LineSpec state -> Value
lineSpecEncoder linespec =
    case linespec of
        StringLineSpec string ->
            JE.string string

        UrlLineSpec string ->
            JE.object
                [ ( "url", JE.string string ) ]

        UserLineSpec r ->
            JE.object
                [ ( "user", JE.string r.user )
                , ( "s", lineSpecEncoder r.linespec )
                ]

        TimeLineSpec r ->
            JE.object
                [ ( "time", JE.float r.time )
                , ( "s", lineSpecEncoder r.linespec )
                ]

        CustomLineSpec state ->
            JE.object
                [ ( "error"
                  , JE.string "Use customLineSpecEncoder to encode a CustomLineSpec."
                  )
                ]


{-| Turn `CustomSettings` into a JSON string.

Use the `(state -> Value)` function to encode `CustomLineSpec`s.

Does not encode the `attributes` or `updater` properties.

-}
encodeCustomSettings : CustomSettings state msg -> (state -> Value) -> String
encodeCustomSettings settings encoder =
    JE.encode 0 <| customSettingsEncoder settings encoder


{-| The JSON encoder for `encodeSettings`.
-}
customSettingsEncoder : CustomSettings state msg -> (state -> Value) -> Value
customSettingsEncoder settings encoder =
    JE.object
        [ ( "fontSize", JE.int settings.fontSize )
        , ( "lines", JE.list <| List.map (customLineSpecEncoder encoder) settings.lines )
        , ( "scroll", JE.float settings.scroll )
        , ( "id", JE.string settings.id )
        , ( "defaultFontSize", JE.int settings.defaultFontSize )
        , ( "showSizeControls", JE.bool settings.showSizeControls )
        ]


customLineSpecEncoder : (state -> Value) -> LineSpec state -> Value
customLineSpecEncoder encoder linespec =
    case linespec of
        CustomLineSpec state ->
            JE.object
                [ ( "custom", encoder state ) ]

        _ ->
            lineSpecEncoder linespec


{-| Turn a JSON string back into a `CustomSettings` record.

`Updater` is as to `makeSettings`.

Restores with default `attributes`, so you'll need to change those
after decoding, if you customize them.

-}
decodeSettings : Updater state msg -> String -> Result String (CustomSettings state msg)
decodeSettings updater json =
    JD.decodeString (settingsDecoder updater) json


restoreSettings : Updater state msg -> Int -> List (LineSpec state) -> Float -> String -> Int -> Bool -> CustomSettings state msg
restoreSettings updater fontSize lines scroll id defaultFontSize showSizeControls =
    { fontSize = fontSize
    , lines = lines
    , customRenderers = Nothing
    , input = ""
    , scroll = scroll
    , attributes = defaultExtraAttributes
    , id = id
    , defaultFontSize = defaultFontSize
    , showSizeControls = showSizeControls
    , updater = TheUpdater updater
    }


{-| The JSON Decoder for `decodeSettings`.
-}
settingsDecoder : Updater state msg -> Decoder (CustomSettings state msg)
settingsDecoder updater =
    JD.map7 restoreSettings
        (JD.succeed updater)
        (JD.field "fontSize" JD.int)
        (JD.field "lines" <| JD.list lineSpecDecoder)
        (JD.field "scroll" JD.float)
        (JD.field "id" JD.string)
        (JD.field "defaultFontSize" JD.int)
        (JD.field "showSizeControls" JD.bool)


lineSpecDecoder : Decoder (LineSpec state)
lineSpecDecoder =
    JD.lazy (\() -> lsDecoder)


lsDecoder : Decoder (LineSpec state)
lsDecoder =
    JD.oneOf
        [ JD.map StringLineSpec JD.string
        , JD.map
            (\string ->
                UrlLineSpec string
            )
            (JD.field "url" JD.string)
        , JD.map2
            (\user spec ->
                UserLineSpec
                    { user = user
                    , linespec = spec
                    }
            )
            (JD.field "user" JD.string)
            (JD.field "s" <| JD.lazy (\() -> lineSpecDecoder))
        , JD.map2
            (\time spec ->
                TimeLineSpec
                    { time = time
                    , linespec = spec
                    }
            )
            (JD.field "time" JD.float)
            (JD.field "s" <| JD.lazy (\() -> lineSpecDecoder))
        ]


{-| Turn a JSON string that may contain `CustomLineSpec`s back into a `CustomSettings` record.

`Updater` is as to `makeSettings`.

The `state` decoder turns a `Value` into your state.

Restores with default `attributes`, so you'll need to change those
after decoding, if you customize them.

-}
decodeCustomSettings : Updater state msg -> Decoder state -> String -> Result String (CustomSettings state msg)
decodeCustomSettings updater decoder json =
    JD.decodeString (customSettingsDecoder updater decoder) json


{-| The JSON Decoder for `decodeCustomSettings`.
-}
customSettingsDecoder : Updater state msg -> Decoder state -> Decoder (CustomSettings state msg)
customSettingsDecoder updater decoder =
    JD.map7 restoreSettings
        (JD.succeed updater)
        (JD.field "fontSize" JD.int)
        (JD.field "lines" <| JD.list (customLineSpecDecoder decoder))
        (JD.field "scroll" JD.float)
        (JD.field "id" JD.string)
        (JD.field "defaultFontSize" JD.int)
        (JD.field "showSizeControls" JD.bool)


customLineSpecDecoder : Decoder state -> Decoder (LineSpec state)
customLineSpecDecoder decoder =
    JD.lazy (\() -> customLsDecoder decoder)


customLsDecoder : Decoder state -> Decoder (LineSpec state)
customLsDecoder decoder =
    JD.oneOf
        [ JD.map CustomLineSpec <| JD.field "custom" decoder
        , lsDecoder
        ]



---
--- Restore scroll
---


{-| Restore the scroll position after restoring from a JSON string.
-}
restoreScroll : CustomSettings state msg -> Cmd msg
restoreScroll settings =
    Task.attempt (\_ -> noUpdate settings) <|
        Scroll.toY settings.id (settings.scroll + 1)
