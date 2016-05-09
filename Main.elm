import StartApp
import Game exposing (init, update, view)

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html