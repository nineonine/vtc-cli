module Component.Footer (genFooter) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (className, HTML)
import Halogen.Themes.Bootstrap3 (textCenter)


genFooter :: forall p i . HTML p i
genFooter =
    HH.div [ HP.classes [ className "footer"] ] [
      HH.h6 [ HP.id_ "footer-text", HP.class_ textCenter ] [
        HH.text "VTC client 2017"
      ]
    ]
