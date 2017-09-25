# elm-chat

This is a simple chat component, designed to be easy to add to your user interface.

I use a WebSocket interface to feed it input, which you can see in my [Archmage game](https://github.com/billstclair/archmage/).

The [examples directory](examples/) has a simple self-contained UI, with separate text input areas for two chatters. It's live at [gibgoygames.com/elm-chat](https://gibgoygames.com/elm-chat/).

You need to reserve space for the chat `Settings` record in your model:

    type alias Model =
        { ...
        , chatSettings : ElmChat.Settings
        , ...
        }
        
You need two messages, one to send chat input, and one to update the settings:

    type Msg
       = ...
       | ChatUpdate Settings (Cmd Msg)
       | ChatSend String Settings
       | ...
       
When you initialize your model, create a `Settings` record:

    init : ( Model, Msg )
    init =
        ( { ...
          , chatSettings = ElmChat.makeSettings "chatid" ChatUpdate
          , ...
          }
          
Inside your view function, create a chat display area:

    ElmChat.chat model.chatSettings
  
And an input box:

    ElmChat.inputBox 40 "Send" ChatSend model.chatSettings
  
Handle the `ChatUpdate` and `ChatSend` messages in your `update` function.

    update : Model -> Msg -> ( Model, Cmd Msg )
    update model msg =
        ...
        ChatUpdate settings cmd ->
            ( { model | chatSettings = settings }
            , cmd
            )
        ChatSend line settings ->
            update <| ChatUpdate settings (sendLine line model)
        ....

Where `sendLine` is a function that returns a `Cmd` to send the line over the wire.    

Open source: [github.com/billstclair/elm-chat](https://github.com/billstclair/elm-chat)

Bill St. Clair<br/>
24 September, 2017
