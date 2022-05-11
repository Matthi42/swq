module Style where

import Prelude (($))
import Concur.Core (Widget)
import Concur.React (HTML)
-- import Concur.React.DOM hiding (style)
-- import Concur.React.Props

import Concur.React.MUI.DOM as MD
import Concur.React.DOM as D
import Concur.React.Props as P

kontaktCSS :: String
kontaktCSS = """
.header { 
  display: flex
} 
.header h4 {
  margin-right: auto;
  flex-grow: 1;
}
.header a { margin-left: 1rem; }

.content > * {
  margin-top: 2rem;
}
.error-msg {
    background-color: red;
    height: 50vh;
    display: flex;
    align-items: center;
    justify-content: center;
}
"""

-- toolbar :: forall a. Widget HTML a
-- toolbar = div [ className "header" ] 
--         [ div [ className "container", style {display: "flex"} ] 
--                 [ h1_ [] $ text "Kontaktsplitter"
--                 , a_ [ href "/tests" ] $ text "Tests"
--                 , a_ [ href "/docs" ] $ text "Dokumentation"
--                 ]
--             ]

toolbar :: forall a. Widget HTML a
toolbar = MD.appBar []
    [ MD.toolbar []
      [ MD.typography [] [D.text "Kontaktsplitter"]
      ]
    ]
