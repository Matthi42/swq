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
/*html { 
  font-family: Verdana, Arial, Helvetica, sans-serif;
}*/
.header { 
  //margin-bottom: 1rem;
  display: flex
} 
.header h4 {
  //display: inline;
  //width: 100%;
  //margin: 0;
  margin-right: auto;
  flex-grow: 1;
}
.header a {
  //display: inline;
  //font-size: 2rem;
  margin-left: 1rem;
}
/*.container {
  padding: 0 .25em;
  margin: 0 auto;
}
@media (min-width: 768px) { .container { width: 750px; } }
@media (min-width: 992px) { .container { width: 970px; } }
@media (min-width: 1200px) { .container { width: 1170px; } }
    */
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
