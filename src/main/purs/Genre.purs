module Genre where

import Prelude

data Genre = All | Comedy | Musical | Drama

derive instance eqGenreFilter :: Eq Genre

showGenre :: Genre -> String
showGenre All       = "All"
showGenre Comedy    = "Comedy"
showGenre Musical   = "Musical"
showGenre Drama     = "Drama"

initGenre :: String -> Genre
initGenre "comedy"  = Comedy
initGenre "drama"   = Drama
initGenre "musical" = Musical
initGenre _         = All
