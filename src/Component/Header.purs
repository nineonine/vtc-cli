module Component.Header (genHeader) where

import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (className, prop, propName, Prop, HTML)
import Halogen.Themes.Bootstrap3 (container, navbarToggle, navbarCollapse, collapse, nav, navbarNav)


newProp :: forall i . String -> String -> Prop i
newProp k v = prop ( propName k ) Nothing v

genHeader :: forall p i . HTML p i
genHeader =
    HH.nav [ HP.classes [className "nav", className "navbar-default", className "navbar-fixed-top", className "st-header"] ] [
      HH.div [ HP.classes [container] ] [
        HH.div [ HP.classes [ className "navbar-header"] ] [
          HH.button [ HP.type_ "button"
                    , HP.classes [navbarToggle, className "default"]
                    , newProp "data-toggle" "collapse"
                    , newProp "data-target" "#navbar"
                    , newProp "aria-expanded" "false"
                    , newProp "aria-controls" "navbar" ]
                    [
                      HH.span [ HP.classes [ className "sr-only" ] ] [ HH.text "Toggle navigation"]
                    , HH.span [ HP.classes [ className "icon-bar" ] ] []
                    , HH.span [ HP.classes [ className "icon-bar" ] ] []
                    , HH.span [ HP.classes [ className "icon-bar" ] ] []
                    ]
        , HH.a [ HP.classes [ className "navbar-brand"], HP.href "#" ] [ HH.text "VTC"]
        ]
        , HH.div [ HP.id_ "navbar", HP.classes [navbarCollapse, collapse] ] [
            HH.ul [ HP.classes [nav, navbarNav] ] [
              HH.li_ [ HH.a [ HP.href "#"] [ HH.text "Quotes" ] ]
            ]
            , HH.ul [ HP.classes [nav, navbarNav, className "navbar-right"] ] [
              HH.li_  [  HH.a [ HP.href "#" ] [ HH.text "604 000 00 00" ] ]
            ]
        ]
      ]
    ]
