module Main exposing (init, main, update, view)

import Browser
import Html exposing (..)


type alias Flags =
    ()


init : Flags -> ( Model, Effect )
init _ =
    ( Model 0 "modelInitialValue", None )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init >> Tuple.mapSecond perform
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond perform
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { property : Int
    , property2 : String
    }


type Msg
    = Msg1
    | Msg2


type Effect
    = None


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        None ->
            Cmd.none


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Msg1 ->
            ( model, None )

        Msg2 ->
            ( model, None )



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "Document Title"
    , body =
        [ div []
            [ text "New Document" ]
        ]
    }
