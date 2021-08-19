{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel where

import           Control.Applicative                                    (liftA2)
import           Data.Maybe                                             (fromJust, isJust, isNothing)
import           Data.Time.Calendar                                     (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents
import           Language.Marlowe.ACTUS.Definitions.ContractState       (ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (CR, CT (..), ContractTerms (..), DCC, FEB (..),
                                                                         IPCB (..), PRF, SCEF (..), n)
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule    (schedule)
import           Language.Marlowe.ACTUS.Model.Utility.ContractRoleSign  (contractRoleSign)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (inf, plusCycle, sup)
import           Language.Marlowe.ACTUS.Model.Utility.YearFraction      (yearFraction)

{-# ANN module "HLint: ignore Use camelCase" #-}

r :: CR -> Double
r = contractRoleSign

y :: DCC -> Day -> Day -> Maybe Day -> Double
y = yearFraction

scef_xNx :: SCEF -> Bool
scef_xNx SE_0N0 = True
scef_xNx SE_0NM = True
scef_xNx SE_IN0 = True
scef_xNx SE_INM = True
scef_xNx _      = False

scef_Ixx :: SCEF -> Bool
scef_Ixx SE_IN0 = True
scef_Ixx SE_INM = True
scef_Ixx SE_I00 = True
scef_Ixx SE_I0M = True
scef_Ixx _      = False

initMd :: ContractTerms -> Day -> Maybe Day
initMd ContractTerms{..} tpr_minus =
  case contractType of
    PAM -> ct_MD
    _   -> let t0 = Just ct_SD

               tminus | isJust ct_PRANX && ct_PRANX >= t0     = ct_PRANX
                      | liftA2 plusCycle ct_IED ct_PRCL >= t0 = liftA2 plusCycle ct_IED ct_PRCL
                      | otherwise                             = Just tpr_minus

               tmd | isJust ct_MD = ct_MD
                   | otherwise    = do _NT    <- ct_NT
                                       _PRNXT <- ct_PRNXT
                                       _PRCL  <- ct_PRCL

                                       let _PRCL' = _PRCL { n = ceiling (_NT / _PRNXT) * n _PRCL }
                                       liftA2 plusCycle tminus (Just _PRCL')

            in tmd

initNt :: ContractTerms -> Maybe Double
initNt ContractTerms{..} =
  let t0 = Just ct_SD

      nt | ct_IED > t0 = Just 0.0
         | otherwise   = (\x -> r ct_CNTRL * x) <$> ct_NT
  in nt

initIpnr :: ContractTerms -> Maybe Double
initIpnr ContractTerms{..} =
  let t0 = Just ct_SD

      ipnr | ct_IED > t0 = Just 0.0
           | otherwise   = ct_IPNR
  in ipnr

initIpac :: ContractTerms -> Double -> Double -> Day -> Maybe Double
initIpac ContractTerms{..} nt ipnr tminus =
  let t0 = ct_SD

      ipac | isNothing ct_IPNR = Just 0.0
           | isJust ct_IPAC    = ct_IPAC
           | otherwise         = (\d -> y d tminus t0 ct_MD * nt * ipnr) <$> ct_DCC
  in ipac

initFeac :: ContractTerms -> Double -> Day -> Day -> Maybe Double
initFeac ContractTerms{..} nt tfp_minus tfp_plus =
  let t0 = ct_SD
      feac | isNothing ct_FER     = Just 0.0
           | isJust ct_FEAC       = ct_FEAC
           | ct_FEB == Just FEB_N = (\d -> y d tfp_minus t0 ct_MD * nt * fromJust ct_FER) <$> ct_DCC
           | otherwise            = (\d -> y d tfp_minus t0 ct_MD / y d tfp_minus tfp_plus ct_MD * fromJust ct_FER) <$> ct_DCC
  in feac

initNsc :: ContractTerms -> Maybe Double
initNsc ContractTerms{..} =
  let nsc | maybe False scef_xNx ct_SCEF = ct_SCCDD
          | otherwise                    = Just 1.0
  in nsc

initIsc :: ContractTerms -> Maybe Double
initIsc ContractTerms{..} =
  let isc | maybe False scef_Ixx ct_SCEF = ct_SCCDD
          | otherwise                    = Just 1.0
  in isc

initPrf :: ContractTerms -> Maybe PRF
initPrf ContractTerms{..} = ct_PRF

initSd :: ContractTerms -> Maybe Day
initSd ContractTerms{..} = Just ct_SD

initPrnxt :: ContractTerms -> Day -> Day -> Maybe Double
initPrnxt ContractTerms{..} tpr_minus tmd =
  case contractType of
    PAM -> Just 0.0
    LAM ->
      let t0 = ct_SD

          s | isJust ct_PRANX && ct_PRANX > Just t0                           = ct_PRANX
            | isNothing ct_PRANX && liftA2 plusCycle ct_IED ct_PRCL > Just t0 = liftA2 plusCycle ct_IED ct_PRCL
            | otherwise                                                       = Just tpr_minus

          prnxt | isJust ct_PRNXT = ct_PRNXT
                | otherwise       = do _NT   <- ct_NT
                                       _PRCL <- ct_PRCL
                                       _DCC  <- ct_DCC
                                       s'    <- s
                                       return $ _NT * (1.0 / fromIntegral (ceiling (y _DCC s' tmd (Just tmd) / y _DCC s' (s' `plusCycle` _PRCL) (Just tmd)) :: Integer))

      in prnxt

    NAM -> liftA2 (*) (Just $ r ct_CNTRL) ct_PRNXT
    ANN -> Just 0.0

initIpcb :: ContractTerms -> Maybe Double
initIpcb ContractTerms{..} =
  case contractType of
    PAM -> Just 0.0
    _   ->
      let t0 = Just ct_SD
          ipcb | t0 < ct_IED             = Just 0.0
               | ct_IPCB == Just IPCB_NT = liftA2 (*) (Just $ r ct_CNTRL) ct_NT
               | otherwise               = liftA2 (*) (Just $ r ct_CNTRL) ct_IPCBA
      in ipcb

_INIT :: ContractTerms -> Maybe (ContractStatePoly Double Day)
_INIT ct@ContractTerms{..} = let
    fpSchedule = schedule FP ct
    ipSchedule = schedule IP ct
    prSchedule = schedule PR ct

    t0         = ct_SD
    tfp_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< fpSchedule)
    tfp_plus   = maybe t0 calculationDay ((\sc -> inf sc t0) =<< fpSchedule)
    tminus     = maybe t0 calculationDay ((\sc -> sup sc t0) =<< ipSchedule)
    tpr_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< prSchedule)

    in do
      tmd   <- initMd ct tpr_minus
      nt    <- initNt ct
      ipnr  <- initIpnr ct
      ipac  <- initIpac ct nt ipnr tminus
      feac  <- initFeac ct nt tfp_minus tfp_plus
      nsc   <- initNsc ct
      isc   <- initIsc ct
      prf   <- initPrf ct
      sd    <- initSd ct
      prnxt <- initPrnxt ct tpr_minus tmd
      ipcb  <- initIpcb ct

      return $ ContractStatePoly {
            prnxt = prnxt
          , ipcb  = ipcb
          , tmd   = tmd
          , nt    = nt
          , ipnr  = ipnr
          , ipac  = ipac
          , feac  = feac
          , nsc   = nsc
          , isc   = isc
          , prf   = prf
          , sd    = sd
        }
