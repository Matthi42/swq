module Style where

import Prelude (($))
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM hiding (style)
import Concur.React.Props

kontaktCSS :: String
kontaktCSS = """
html { 
  font-family: Verdana, Arial, Helvetica, sans-serif;
}
.header { 
    margin-bottom: 1rem;
} 
.header h1 {
  display: inline;
  width: 100%;
  margin: 0;
  margin-right: auto;
  flex-grow: 1;
}
.header a {
  display: inline;
  font-size: 2rem;
  margin-left: 1rem;
}
.container {
  padding: 0 .25em;
  margin: 0 auto;
}
@media (min-width: 768px) { .container { width: 750px; } }
@media (min-width: 992px) { .container { width: 970px; } }
@media (min-width: 1200px) { .container { width: 1170px; } }
"""

toolbar :: forall a. Widget HTML a
toolbar = div [ className "header" ] 
        [ div [ className "container", style {display: "flex"} ] 
                [ h1_ [] $ text "Kontaktsplitter"
                , a_ [ href "/tests" ] $ text "Tests"
                , a_ [ href "/docs" ] $ text "Dokumentation"
                ]
            ]
