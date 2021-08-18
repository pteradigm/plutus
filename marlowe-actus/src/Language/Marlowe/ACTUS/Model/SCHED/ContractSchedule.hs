{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule where

import           Language.Marlowe.ACTUS.Definitions.BusinessEvents          (EventType (FP, IED, IP, IPCB, IPCI, MD, PP, PR, PRD, PY, RR, RRF, SC, TD))
import           Language.Marlowe.ACTUS.Definitions.ContractState           (ContractStatePoly (tmd))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms           (CT (..), ContractTerms (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule                (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (_INIT_ANN, _INIT_LAM, _INIT_NAM)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator     (inf, sup)

schedule :: EventType -> ContractTerms -> Maybe [ShiftedDay]
schedule ev ct@ContractTerms{..} =
    let fpSchedule = schedule FP ct
        ipSchedule = schedule IP ct
        prSchedule = schedule PR ct

        t0         = ct_SD
        tminus     = maybe t0 calculationDay ((\sc -> sup sc t0) =<< ipSchedule)
        tfp_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< fpSchedule)
        tfp_plus   = maybe t0 calculationDay ((\sc -> inf sc t0) =<< fpSchedule)
        tpr_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< prSchedule)
    in
      case contractType of

        PAM -> let _MD = ct_MD
          in case ev of
            IED  -> _SCHED_IED_PAM ct
            MD   -> _SCHED_MD_PAM ct { ct_MD = _MD }
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

        LAM -> let _MD = Just . tmd $ _INIT_LAM ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _MD }
            MD   -> _SCHED_MD_LAM ct { ct_MD = _MD }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _MD }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _MD }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _MD }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_PAM ct { ct_MD = _MD }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _MD}
            RR   -> _SCHED_RR_PAM ct { ct_MD = _MD }
            RRF  -> _SCHED_RRF_PAM ct { ct_MD = _MD }
            SC   -> _SCHED_SC_PAM ct { ct_MD = _MD }
            _    -> Nothing

        NAM -> let _MD = Just . tmd $ _INIT_NAM ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _MD }
            MD   -> _SCHED_MD_PAM ct { ct_MD = _MD }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _MD }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _MD }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _MD }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_NAM ct { ct_MD = _MD }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _MD}
            RR   -> _SCHED_RR_PAM ct { ct_MD = _MD }
            RRF  -> _SCHED_RRF_PAM ct { ct_MD = _MD }
            SC   -> _SCHED_SC_PAM ct { ct_MD = _MD }
            _    -> Nothing

        ANN -> let _MD = Just . tmd $ _INIT_ANN ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM ct
            PR   -> _SCHED_PR_LAM ct { ct_MD = _MD }
            MD   -> _SCHED_MD_PAM ct { ct_MD = _MD }
            PP   -> _SCHED_PP_PAM ct { ct_MD = _MD }
            PY   -> _SCHED_PY_PAM ct { ct_MD = _MD }
            FP   -> _SCHED_FP_PAM ct { ct_MD = _MD }
            PRD  -> _SCHED_PRD_PAM ct
            TD   -> _SCHED_TD_PAM ct
            IP   -> _SCHED_IP_NAM ct { ct_MD = _MD }
            IPCI -> _SCHED_IPCI_PAM ct
            IPCB -> _SCHED_IPCB_LAM ct { ct_MD = _MD }
            RR   -> _SCHED_RR_PAM ct
            RRF  -> _SCHED_RRF_PAM ct
            SC   -> _SCHED_SC_PAM ct
            _    -> Nothing
