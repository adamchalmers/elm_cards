import Html.App as Html
import Game exposing (init, update, view, subscriptions)

main : Program Never
main =
  Html.program
    { init = init 0
    , update = update
    , view = view
    , subscriptions = subscriptions
    }