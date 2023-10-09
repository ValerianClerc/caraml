{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Type
  = TInt
  | TBool
  | TVoid
  | TFun {tArgs :: [Type], tReturnType :: Type}
  | TUnknown -- used in type inference for recursive functions
  -- TODO: add char, string, list, tuple, function
  deriving (Show, Eq, NFData, Generic)