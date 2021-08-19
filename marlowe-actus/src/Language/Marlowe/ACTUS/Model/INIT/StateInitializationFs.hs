module Language.Marlowe.ACTUS.Model.INIT.StateInitializationFs where

import           Data.Maybe                                                 (fromJust)
import           Language.Marlowe                                           (Contract)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms           (ContractTerms)
import           Language.Marlowe.ACTUS.MarloweCompat                       (stateInitialisation)
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (_INIT)

inititializeStateFs :: ContractTerms -> Contract -> Contract
inititializeStateFs ct continue = let s = fromJust $ _INIT ct in stateInitialisation s continue -- TODO: reconsider fromJust
