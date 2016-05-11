import Html.App as Html
import Game exposing (init, update, view)

main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

