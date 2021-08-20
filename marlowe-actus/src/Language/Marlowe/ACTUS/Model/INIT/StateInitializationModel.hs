{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract state initialization per t0

The implementation is a transliteration of the ACTUS specification v1.1
Note: initial states rely also on some schedules (and vice versa)

-}

module Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel where

import           Control.Applicative                                    (liftA2)
import           Data.Maybe                                             (isJust, isNothing)
import           Data.Time.Calendar                                     (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents
import           Language.Marlowe.ACTUS.Definitions.ContractState       (ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (CR, CT (..), ContractTerms (..), DCC, FEB (..),
                                                                         IPCB (..), SCEF (..), n)
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule    (schedule)
import           Language.Marlowe.ACTUS.Model.Utility.ContractRoleSign  (contractRoleSign)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (inf, plusCycle, sup)
import           Language.Marlowe.ACTUS.Model.Utility.YearFraction      (yearFraction)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- |_INIT initializes the state variables at t0
_INIT :: ContractTerms -> Maybe (ContractStatePoly Double Day)
_INIT ct@ContractTerms {..} =
  do
    tmd <- case contractType of
      PAM -> ct_MD
      _ ->
        let tminus'
              | isJust ct_PRANX && ct_PRANX >= Just t0 = ct_PRANX
              | liftA2 plusCycle ct_IED ct_PRCL >= Just t0 = liftA2 plusCycle ct_IED ct_PRCL
              | otherwise = Just tpr_minus

            tmd
              | isJust ct_MD = ct_MD
              | otherwise = do
                _NT <- ct_NT
                _PRNXT <- ct_PRNXT
                _PRCL <- ct_PRCL
                liftA2 plusCycle tminus' (Just _PRCL {n = ceiling (_NT / _PRNXT) * n _PRCL})
         in tmd

    nt <-
      let nt
            | ct_IED > Just t0 = Just 0.0
            | otherwise = (\x -> r ct_CNTRL * x) <$> ct_NT
       in nt

    ipnr <-
      let ipnr
            | ct_IED > Just t0 = Just 0.0
            | otherwise = ct_IPNR
       in ipnr

    ipac <-
      let ipac
            | isNothing ct_IPNR = Just 0.0
            | isJust ct_IPAC = ct_IPAC
            | otherwise = (\d -> y d tminus t0 ct_MD * nt * ipnr) <$> ct_DCC
       in ipac

    feac <-
      let feac
            | isNothing ct_FER = Just 0.0
            | isJust ct_FEAC = ct_FEAC
            | ct_FEB == Just FEB_N = do
                _DCC <- ct_DCC
                _FER <- ct_FER
                pure $ y _DCC tfp_minus t0 ct_MD * nt * _FER
            | otherwise = do
                _DCC <- ct_DCC
                _FER <- ct_FER
                pure $ y _DCC tfp_minus t0 ct_MD / y _DCC tfp_minus tfp_plus ct_MD * _FER
       in feac

    nsc <-
      let nsc
            | maybe False scef_xNx ct_SCEF = ct_SCCDD
            | otherwise = Just 1.0
       in nsc

    isc <-
      let isc
            | maybe False scef_Ixx ct_SCEF = ct_SCCDD
            | otherwise = Just 1.0
       in isc

    prf <- ct_PRF

    let sd = ct_SD

    prnxt <- case contractType of
      PAM -> Just 0.0
      LAM ->
        let s
              | isJust ct_PRANX && ct_PRANX > Just t0 = ct_PRANX
              | isNothing ct_PRANX && liftA2 plusCycle ct_IED ct_PRCL > Just t0 = liftA2 plusCycle ct_IED ct_PRCL
              | otherwise = Just tpr_minus

            prnxt
              | isJust ct_PRNXT = ct_PRNXT
              | otherwise = do
                _NT <- ct_NT
                _PRCL <- ct_PRCL
                _DCC <- ct_DCC
                s' <- s
                return $ _NT * (1.0 / fromIntegral (ceiling (y _DCC s' tmd (Just tmd) / y _DCC s' (s' `plusCycle` _PRCL) (Just tmd)) :: Integer))
         in prnxt
      NAM -> liftA2 (*) (Just $ r ct_CNTRL) ct_PRNXT
      ANN ->
        let prnxt
              | isJust ct_PRNXT = liftA2 (*) (Just $ r ct_CNTRL) ct_PRNXT
              | otherwise = Just $ ipac * nt
         in prnxt

    ipcb <- case contractType of
      PAM -> Just 0.0
      _ ->
        let ipcb
              | Just t0 < ct_IED = Just 0.0
              | ct_IPCB == Just IPCB_NT = liftA2 (*) (Just $ r ct_CNTRL) ct_NT
              | otherwise = liftA2 (*) (Just $ r ct_CNTRL) ct_IPCBA
         in ipcb

    return
      ContractStatePoly
        { prnxt = prnxt,
          ipcb = ipcb,
          tmd = tmd,
          nt = nt,
          ipnr = ipnr,
          ipac = ipac,
          feac = feac,
          nsc = nsc,
          isc = isc,
          prf = prf,
          sd = sd
        }
  where
    fpSchedule = schedule FP ct
    ipSchedule = schedule IP ct
    prSchedule = schedule PR ct

    t0 = ct_SD
    tfp_minus = maybe t0 calculationDay ((\sc -> sup sc t0) =<< fpSchedule)
    tfp_plus = maybe t0 calculationDay ((\sc -> inf sc t0) =<< fpSchedule)
    tminus = maybe t0 calculationDay ((\sc -> sup sc t0) =<< ipSchedule)
    tpr_minus = maybe t0 calculationDay ((\sc -> sup sc t0) =<< prSchedule)

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
