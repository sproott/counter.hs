{-# LANGUAGE TemplateHaskell #-}

module State where

import           Lens.Micro.TH

data State = State
  { _isIncrement :: Bool,
    _count       :: Int
  }

makeLenses ''State

getInitialState :: State
getInitialState = State {_isIncrement = True, _count = 0}
