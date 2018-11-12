-- | Re-export functions that are needed when creating a Contract for use in the playground
module Playground.Contract
  ( mkFunction
  , ToSchema
  , Schema
  , ToJSON
  , FromJSON
  , FunctionSchema
  ) where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Swagger   (Schema, ToSchema)
import           Playground.API (FunctionSchema)
import           Playground.TH  (mkFunction)
