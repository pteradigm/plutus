{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule where

import           Data.Maybe                                                 (fromJust)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents          (EventType (FP, IED, IP, IPCB, IPCI, MD, PP, PR, PRD, PY, RR, RRF, SC, TD))
import           Language.Marlowe.ACTUS.Definitions.ContractState           (ContractStatePoly (tmd))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms           (CT (..), ContractTerms (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule                (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (_INIT_ANN, _INIT_LAM, _INIT_NAM)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator     (inf, sup)

schedule :: EventType -> ContractTerms -> Maybe [ShiftedDay]
schedule ev ct@ContractTerms {..} =
    let
        _IED       = fromJust ct_IED
        _FER       = fromJust ct_FER
        _MD        = fromJust ct_MD
        _SCEF      = fromJust ct_SCEF
        _PYTP      = fromJust ct_PYTP
        _PPEF      = fromJust ct_PPEF

        fpSchedule = schedule FP ct
        ipSchedule = schedule IP ct
        prSchedule = schedule PR ct

        t0         = ct_SD

        tminus     = maybe t0 calculationDay ((\sc -> sup sc t0) =<< ipSchedule)
        tfp_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< fpSchedule)
        tfp_plus   = maybe t0 calculationDay ((\sc -> inf sc t0) =<< fpSchedule)
        tpr_minus  = maybe t0 calculationDay ((\sc -> sup sc t0) =<< prSchedule)
    in
      case contractType of

        PAM -> case ev of
            IED  -> _SCHED_IED_PAM scfg _IED
            MD   -> _SCHED_MD_PAM scfg _MD
            PP   -> _SCHED_PP_PAM scfg _PPEF ct_OPCL _IED ct_OPANX _MD
            PY   -> _SCHED_PY_PAM scfg _PYTP _PPEF ct_OPCL _IED ct_OPANX _MD
            FP   -> _SCHED_FP_PAM scfg _FER ct_FECL _IED ct_FEANX _MD
            PRD  -> _SCHED_PRD_PAM scfg ct_PRD
            TD   -> _SCHED_TD_PAM scfg ct_TD
            IP   -> _SCHED_IP_PAM scfg ct_IPNR _IED ct_IPANX ct_IPCL ct_IPCED _MD
            IPCI -> _SCHED_IPCI_PAM scfg _IED ct_IPANX ct_IPCL ct_IPCED _MD ct_IPNR
            RR   -> _SCHED_RR_PAM scfg _IED ct_SD ct_RRANX ct_RRCL ct_RRNXT _MD
            RRF  -> _SCHED_RRF_PAM scfg _IED ct_RRANX ct_RRCL _MD
            SC   -> _SCHED_SC_PAM scfg _IED _SCEF ct_SCANX ct_SCCL _MD
            _    -> Nothing

        LAM -> let _tmd = tmd $ _INIT_LAM ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM scfg _IED
            PR   -> _SCHED_PR_LAM scfg ct_PRCL _IED ct_PRANX _tmd
            MD   -> _SCHED_MD_LAM scfg _tmd
            PP   -> _SCHED_PP_PAM scfg _PPEF ct_OPCL _IED ct_OPANX _tmd
            PY   -> _SCHED_PY_PAM scfg _PYTP _PPEF ct_OPCL _IED ct_OPANX _tmd
            FP   -> _SCHED_FP_PAM scfg _FER ct_FECL _IED ct_FEANX _tmd
            PRD  -> _SCHED_PRD_PAM scfg ct_PRD
            TD   -> _SCHED_TD_PAM scfg ct_TD
            IP   -> _SCHED_IP_PAM scfg ct_IPNR _IED ct_IPANX ct_IPCL ct_IPCED _tmd
            IPCI -> _SCHED_IPCI_PAM scfg _IED ct_IPANX ct_IPCL ct_IPCED _MD ct_IPNR
            IPCB -> _SCHED_IPCB_LAM scfg _IED ct_IPCB ct_IPCBCL ct_IPCBANX _tmd
            RR   -> _SCHED_RR_PAM scfg _IED ct_SD ct_RRANX ct_RRCL ct_RRNXT _tmd
            RRF  -> _SCHED_RRF_PAM scfg _IED ct_RRANX ct_RRCL _tmd
            SC   -> _SCHED_SC_PAM scfg _IED _SCEF ct_SCANX ct_SCCL _tmd
            _    -> Nothing

        NAM -> let _tmd = tmd $ _INIT_NAM ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM scfg _IED
            PR   -> _SCHED_PR_LAM scfg ct_PRCL _IED ct_PRANX _tmd
            MD   -> _SCHED_MD_PAM scfg _tmd
            PP   -> _SCHED_PP_PAM scfg _PPEF ct_OPCL _IED ct_OPANX _tmd
            PY   -> _SCHED_PY_PAM scfg _PYTP _PPEF ct_OPCL _IED ct_OPANX _tmd
            FP   -> _SCHED_FP_PAM scfg _FER ct_FECL _IED ct_FEANX _tmd
            PRD  -> _SCHED_PRD_PAM scfg ct_PRD
            TD   -> _SCHED_TD_PAM scfg ct_TD
            IP   -> _SCHED_IP_NAM scfg _IED ct_PRCL ct_PRANX ct_IPCED ct_IPANX ct_IPCL _tmd
            IPCI -> _SCHED_IPCI_PAM scfg _IED ct_IPANX ct_IPCL ct_IPCED _MD ct_IPNR
            IPCB -> _SCHED_IPCB_LAM scfg _IED ct_IPCB ct_IPCBCL ct_IPCBANX _tmd
            RR   -> _SCHED_RR_PAM scfg _IED ct_SD ct_RRANX ct_RRCL ct_RRNXT _tmd
            RRF  -> _SCHED_RRF_PAM scfg _IED ct_RRANX ct_RRCL _tmd
            SC   -> _SCHED_SC_PAM scfg _IED _SCEF ct_SCANX ct_SCCL _tmd
            _    -> Nothing

        ANN -> let _tmd = tmd $ _INIT_ANN ct_SD tminus tpr_minus tfp_minus tfp_plus ct
          in case ev of
            IED  -> _SCHED_IED_PAM scfg _IED
            PR   -> _SCHED_PR_LAM scfg ct_PRCL _IED ct_PRANX _tmd
            MD   -> _SCHED_MD_PAM scfg _MD
            PP   -> _SCHED_PP_PAM scfg _PPEF ct_OPCL _IED ct_OPANX _MD
            PY   -> _SCHED_PY_PAM scfg _PYTP _PPEF ct_OPCL _IED ct_OPANX _MD
            FP   -> _SCHED_FP_PAM scfg _FER ct_FECL _IED ct_FEANX _MD
            PRD  -> _SCHED_PRD_PAM scfg ct_PRD
            TD   -> _SCHED_TD_PAM scfg ct_TD
            IP   -> _SCHED_IP_NAM scfg _IED ct_PRCL ct_PRANX ct_IPCED ct_IPANX ct_IPCL _tmd
            IPCI -> _SCHED_IPCI_PAM scfg _IED ct_IPANX ct_IPCL ct_IPCED _MD ct_IPNR
            IPCB -> _SCHED_IPCB_LAM scfg _IED ct_IPCB ct_IPCBCL ct_IPCBANX _tmd
            RR   -> _SCHED_RR_PAM scfg _IED ct_SD ct_RRANX ct_RRCL ct_RRNXT _MD
            RRF  -> _SCHED_RRF_PAM scfg _IED ct_RRANX ct_RRCL _MD
            SC   -> _SCHED_SC_PAM scfg _IED _SCEF ct_SCANX ct_SCCL _MD
            _    -> Nothing
