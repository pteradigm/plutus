{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule where

import           Control.Applicative                                      (liftA2)
import           Data.Maybe                                               (isJust)
import           Data.Time.Calendar                                       (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents        (EventType (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms         (CT (..), ContractTerms (..), DCC, n)
import           Language.Marlowe.ACTUS.Definitions.Schedule              (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator   (plusCycle, sup)
import           Language.Marlowe.ACTUS.Model.Utility.YearFraction        (yearFraction)

schedule :: EventType -> ContractTerms -> Maybe [ShiftedDay]
schedule ev ct@ContractTerms{..} =
    let _Md = initMd -- Schedule relies on partial state initialization
     in case contractType of

        PAM -> case ev of
            IED  -> _SCHED_IED_PAM ct
            MD   -> _SCHED_MD_PAM ct { ct_MD = _Md }
            PP   -> _SCHED_PP_PAM ct
            PY   -> _SCHED_PY_PAM ct
            FP   -> _SCHED_FP_PAM ct
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_PAM ct
            IPCI -> _SCHED_IPCI_PAM ct
            RR   -> _SCHED_RR_PAM ct
            RRF  -> _SCHED_RRF_PAM ct
            SC   -> _SCHED_SC_PAM ct
            _    -> Nothing

        LAM -> case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _Md }
            MD   -> _SCHED_MD_LAM ct { ct_MD = _Md }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _Md }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _Md }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _Md }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_PAM ct { ct_MD = _Md }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _Md}
            RR   -> _SCHED_RR_PAM ct { ct_MD = _Md }
            RRF  -> _SCHED_RRF_PAM ct { ct_MD = _Md }
            SC   -> _SCHED_SC_PAM ct { ct_MD = _Md }
            _    -> Nothing

        NAM -> case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _Md }
            MD   -> _SCHED_MD_PAM ct { ct_MD = _Md }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _Md }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _Md }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _Md }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_NAM ct { ct_MD = _Md }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _Md}
            RR   -> _SCHED_RR_PAM ct { ct_MD = _Md }
            RRF  -> _SCHED_RRF_PAM ct { ct_MD = _Md }
            SC   -> _SCHED_SC_PAM ct { ct_MD = _Md }
            _    -> Nothing

        ANN -> case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _Md }
            MD   -> _SCHED_MD_PAM ct { ct_MD = _Md }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _Md }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _Md }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _Md }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_NAM ct { ct_MD = _Md }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _Md }
            RR   -> _SCHED_RR_PAM ct
            RRF  -> _SCHED_RRF_PAM ct
            SC   -> _SCHED_SC_PAM ct
            _    -> Nothing

  where

    -- |initMd initialize the maturity date (Md) state variable per t0
    initMd :: Maybe Day
    initMd =
      case contractType of

        PAM -> ct_MD

        LAM -> let ipSchedule = _SCHED_IP_PAM ct
                   prSchedule = _SCHED_PR_LAM ct

                   t0         = ct_SD
                   tminus     = maybe t0 calculationDay ((\sc -> sup sc t0) =<< ipSchedule)
                   tpr_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< prSchedule)

                   tMinus | isJust ct_PRANX && ct_PRANX >= Just t0     = ct_PRANX
                          | liftA2 plusCycle ct_IED ct_PRCL >= Just t0 = liftA2 plusCycle ct_IED ct_PRCL
                          | otherwise                                  = Just tpr_minus

                   _Md | isJust ct_MD = ct_MD
                       | otherwise    = liftA2 plusCycle tMinus $ do _NT    <- ct_NT
                                                                     _PRNXT <- ct_PRNXT
                                                                     _IPNR  <- ct_IPNR
                                                                     _DCC   <- ct_DCC
                                                                     d      <- liftA2 plusCycle (Just tminus) ct_PRCL
                                                                     let y' = y _DCC tminus d ct_MD
                                                                     let ceil = ceiling $ _NT / (_PRNXT - _NT  * y' * _IPNR) -- TODO: correct?
                                                                     (\x -> x {n = ceil}) <$> ct_PRCL
               in _Md

        NAM -> ct_MD -- TODO: correct?
        ANN -> ct_MD -- TODO: correct?

    y :: DCC -> Day -> Day -> Maybe Day -> Double
    y = yearFraction
