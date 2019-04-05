{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Language.PlutusTx.Plugin
import qualified Language.PlutusTx            as P

f :: CompiledCode (P.SizedByteString 0 -> P.SizedByteString 1)
f = $$(P.compile [|| $$(P.resizeByteString) ||])