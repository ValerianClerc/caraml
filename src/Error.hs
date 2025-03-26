{-# LANGUAGE TemplateHaskell #-}

module Error where

import Control.Exception (Exception)
import Data.Data (Typeable)

data ParserException
  = ExtraTokens String
  | ExpectedToken String
  | InvalidFunctionDeclarationArgs String
  | ExpectedEndOfConditional String
  | Unimplemented String
  | UnexpectedToken String
  | UnexpectedEndOfExpression String
  deriving (Show, Eq, Typeable)

instance Exception ParserException