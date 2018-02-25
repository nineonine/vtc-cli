module Main where

import Prelude (Unit, bind)

import Component.Calculator (VTCEffects, calcUi, initialState)

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.Driver (runUI)
import Halogen.Util (runHalogenAff, awaitBody)


main :: Eff (H.HalogenEffects (VTCEffects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI calcUi initialState body
