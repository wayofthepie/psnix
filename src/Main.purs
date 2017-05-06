module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

-- | The button state.
type State = Boolean

-- | Query algebra for the button.
data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

myButton :: forall m. H.Component HH.HTML Query Input Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = false

    render :: State -> H.ComponentHTML Query
    render state =
      let
        label = if state then "On" else "Off"
      in
        HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text label ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Toggle next -> do
        state <- H.get
        let nextState = not state
        H.put nextState
        H.raise $ Toggled nextState
        pure next
      IsOn reply -> do
        state <- H.get
        pure (reply state)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"
