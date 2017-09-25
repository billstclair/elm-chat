The [billstclair/elm-chat](http://package.elm-lang.org/packages/billstclair/elm-chat/latest) package implements a simple chat component, designed to be easy to add to your user interface.

I use a WebSocket interface to feed it input, which you can see in my [Archmage game](https://github.com/billstclair/archmage/).

The [example/ElmChatExample.elm](example/ElmChatExample.elm) is a simple self-contained UI, with separate text input areas for two chatters. It's live at [gibgoygames.com/elm-chat](https://gibgoygames.com/elm-chat/).

You need to reserve space for the chat `Settings` record in your model:

    type alias Model =
        { ...
        , chatSettings : ElmChat.Settings
        , ...
        }
        
You need three messages, one to send chat input, one to update the settings, and one to receive chat input from the other side.

    type Msg
       = ...
       | ChatUpdate Settings (Cmd Msg)
       | ChatSend String Settings
       | ChatReceive String
       | ...
       
When you initialize your model, create a `Settings` record:

    init : ( Model, Msg )
    init =
        ( { ...
          , chatSettings = ElmChat.makeSettings "chatid" 14 True ChatUpdate
          , ...
          }
          
Inside your view function, create a chat display area:

    ElmChat.chat model.chatSettings
  
And an input box:

    ElmChat.inputBox 40 "Send" ChatSend model.chatSettings
  
Handle the messages in your `update` function.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ...
            ChatUpdate settings cmd ->
                ( { model | chatSettings = settings }
                , cmd
                )
            ChatSend line settings ->
                update (ChatUpdate settings (sendLine line model)) model
            ChatReceive line ->
                let (settings, cmd) = ElmChat.addChat settings line
                in
                    update (ChatUpdate settings cmd) model
            ...

Where `sendLine` is a function you write that returns a `Cmd` to send the line over the wire.

You can style the UI components with the `Settings.attributes` property. See the code for details.

Bill St. Clair<br/>
24 September, 2017
