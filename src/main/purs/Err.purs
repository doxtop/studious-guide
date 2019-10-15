module Err where

import Thermite as T
import React.DOM (text, a, div) as R
import React.DOM.Props as RP

data ErrorAction
    = Clear

notification :: forall eff props. T.Spec eff String props ErrorAction
notification = T.simpleSpec T.defaultPerformAction render
    where 
    render :: T.Render String props ErrorAction
    render dispatch _ s _ = [ 
        R.div [RP.className "content note center"]
              [ R.a [ RP.href "javascript:void(0);"
                    , RP.className "clear" 
                    , RP.title "clear" 
                    , RP.onClick \_ -> dispatch Clear
                    ] [R.text "x"]
              , R.text s ] ]
