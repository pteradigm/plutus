{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS state transition functions dispatching to the model

Note: In case of a missing configuration in the `ContractTerms` the
state that is passed in will be returned.

-}

module Language.Marlowe.ACTUS.Model.STF.StateTransition where

import           Data.Maybe                                             (fromMaybe)
import           Data.Time                                              (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents      (EventType (..), RiskFactors (..))

import           Language.Marlowe.ACTUS.Definitions.ContractState       (ContractState,
                                                                         ContractStatePoly (ContractStatePoly, feac, ipac, ipcb, ipnr, isc, nsc, nt, prf, prnxt, sd, tmd))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (CT (..), ContractTerms (..), ScheduleConfig)
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule    (schedule)
import           Language.Marlowe.ACTUS.Model.STF.StateTransitionModel
import           Language.Marlowe.ACTUS.Model.Utility.DateShift
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (inf, sup)


import           Language.Marlowe.ACTUS.Ops                             (YearFractionOps (_y))

shift :: ScheduleConfig -> Day -> ShiftedDay
shift = applyBDCWithCfg

stateTransition :: EventType -> RiskFactors -> ContractTerms -> ContractState -> Day -> ContractState
stateTransition ev RiskFactors{..} ct@ContractTerms{..} st@ContractStatePoly{..} t =
    case contractType of

        PAM ->
            case ev of
                AD   -> stf_AD_PAM
                IED  -> stf_IED_PAM
                MD   -> stf_MD_PAM
                PP   -> stf_PP_PAM
                PY   -> stf_PY_PAM
                FP   -> stf_FP_PAM
                PRD  -> stf_PRD_PAM
                TD   -> stf_TD_PAM
                IP   -> stf_IP_PAM
                IPCI -> stf_IPCI_PAM
                RR   -> stf_RR_PAM
                RRF  -> stf_RRF_PAM
                SC   -> stf_SC_PAM
                CE   -> stf_CE_PAM
                _    -> st

        LAM ->
            case ev of
                AD   -> stf_AD_LAM
                IED  -> stf_IED_LAM
                PR   -> stf_PR_LAM
                MD   -> stf_MD_LAM
                PP   -> stf_PP_LAM
                PY   -> stf_PY_LAM
                FP   -> stf_FP_LAM
                PRD  -> stf_PRD_LAM
                TD   -> stf_TD_PAM
                IP   -> stf_IP_PAM
                IPCI -> stf_IPCI_LAM
                IPCB -> stf_IPCB_LAM
                RR   -> stf_RR_LAM
                RRF  -> stf_RRF_LAM
                SC   -> stf_SC_LAM
                CE   -> stf_AD_PAM
                _    -> st

        NAM ->
            case ev of
                AD   -> stf_AD_PAM
                IED  -> stf_IED_LAM
                PR   -> stf_PR_NAM
                MD   -> stf_MD_LAM
                PP   -> stf_PP_LAM
                PY   -> stf_PY_LAM
                FP   -> stf_FP_LAM
                PRD  -> stf_PRD_LAM
                TD   -> stf_TD_PAM
                IP   -> stf_IP_PAM
                IPCI -> stf_IPCI_LAM
                IPCB -> stf_IPCB_LAM
                RR   -> stf_RR_LAM
                RRF  -> stf_RRF_LAM
                SC   -> stf_SC_LAM
                CE   -> stf_AD_PAM
                _    -> st

        ANN ->
            case ev of
                AD   -> stf_AD_PAM
                IED  -> stf_IED_LAM
                PR   -> stf_PR_NAM
                MD   -> stf_MD_LAM
                PP   -> stf_PP_LAM
                PY   -> stf_PY_LAM
                FP   -> stf_FP_LAM
                PRD  -> stf_PRD_LAM
                TD   -> stf_TD_PAM
                IP   -> stf_IP_PAM
                IPCI -> stf_IPCI_LAM
                IPCB -> stf_IPCB_LAM
                RR   -> stf_RR_ANN
                RRF  -> stf_RRF_ANN
                SC   -> stf_SC_LAM
                CE   -> stf_AD_PAM
                _    -> st

  where
        fpSchedule         = schedule FP ct

        tfp_minus          = maybe t calculationDay ((\sc -> sup sc t) =<< fpSchedule)
        tfp_plus           = maybe t calculationDay ((\sc -> inf sc t) =<< fpSchedule)

        y_sd_t             = (\_DCC -> _y _DCC sd t ct_MD) <$> ct_DCC
        y_tfpminus_t       = (\_DCC -> _y _DCC tfp_minus t ct_MD) <$> ct_DCC
        y_tfpminus_tfpplus = (\_DCC -> _y _DCC tfp_minus tfp_plus ct_MD) <$> ct_DCC
        y_ipanx_t          = do _IPANX <- ct_IPANX
                                _DCC   <- ct_DCC
                                pure $ _y _DCC _IPANX t ct_MD

        getState = fromMaybe st

        -- PAM

        stf_AD_PAM   = getState $ _STF_AD_PAM st t <$> y_sd_t

        stf_IED_PAM  = getState $ do
          _NT        <- ct_NT
          y_ipanx_t' <- y_ipanx_t
          pure $ _STF_IED_PAM st t y_ipanx_t' ct_IPNR ct_IPANX ct_CNTRL ct_IPAC _NT

        stf_MD_PAM   = _STF_MD_PAM st t

        stf_PP_PAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PP_PAM st t pp_payoff y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_PY_PAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PY_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_FP_PAM   = getState $ _STF_FP_PAM st t <$> y_sd_t

        stf_PRD_PAM  = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PRD_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_TD_PAM   = _STF_TD_PAM st t

        stf_IP_PAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_IP_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_IPCI_PAM = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_IPCI_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_RR_PAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          _RRLF               <- ct_RRLF
          _RRLC               <- ct_RRLC
          _RRPC               <- ct_RRPC
          _RRPF               <- ct_RRPF
          _RRMLT              <- ct_RRMLT
          _RRSP               <- ct_RRSP
          pure $ _STF_RR_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO

        stf_RRF_PAM  = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_RRF_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_RRNXT

        stf_SC_PAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          _SCEF               <- ct_SCEF
          _SCIED              <- ct_SCIED
          pure $ _STF_SC_PAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL _SCEF o_rf_SCMO _SCIED

        stf_CE_PAM   = getState $ _STF_CE_PAM st t <$> y_sd_t

        -- LAM

        stf_AD_LAM   = getState $ _STF_AD_LAM st t <$> y_sd_t

        stf_IED_LAM  = getState $ do
          _NT        <- ct_NT
          y_ipanx_t' <- y_ipanx_t
          pure $ _STF_IED_LAM st t y_ipanx_t' ct_IPNR ct_IPANX ct_CNTRL ct_IPAC _NT ct_IPCB ct_IPCBA

        stf_PR_LAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PR_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_IPCB

        stf_MD_LAM   = _STF_MD_LAM st t

        stf_PP_LAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PP_LAM st t pp_payoff y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_IPCB

        stf_PY_LAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PY_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_FP_LAM   = getState $ _STF_FP_LAM st t <$> y_sd_t

        stf_PRD_LAM  = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PRD_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_IPCI_LAM = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_IPCI_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_IPCB

        stf_IPCB_LAM = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_IPCB_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL

        stf_RR_LAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          _RRLF               <- ct_RRLF
          _RRLC               <- ct_RRLC
          _RRPC               <- ct_RRPC
          _RRPF               <- ct_RRPF
          _RRMLT              <- ct_RRMLT
          _RRSP               <- ct_RRSP
          pure $ _STF_RR_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO

        stf_RRF_LAM  = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_RRF_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_RRNXT

        stf_SC_LAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          _SCEF               <- ct_SCEF
          _SCIED              <- ct_SCIED
          pure $ _STF_SC_LAM st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL _SCEF o_rf_SCMO _SCIED

        -- NAM

        stf_PR_NAM   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_PR_NAM st t pp_payoff y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_IPCB

        -- ANN

        stf_RR_ANN   = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          _RRLF               <- ct_RRLF
          _RRLC               <- ct_RRLC
          _RRPC               <- ct_RRPC
          _RRPF               <- ct_RRPF
          _RRMLT              <- ct_RRMLT
          _RRSP               <- ct_RRSP
          pure $ _STF_RR_ANN st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL _RRLF _RRLC _RRPC _RRPF _RRMLT _RRSP o_rf_RRMO

        stf_RRF_ANN  = getState $ do
          _FER                <- ct_FER
          y_sd_t'             <- y_sd_t
          y_tfpminus_t'       <- y_tfpminus_t
          y_tfpminus_tfpplus' <- y_tfpminus_tfpplus
          pure $ _STF_RRF_ANN st t y_sd_t' y_tfpminus_t' y_tfpminus_tfpplus' ct_FEB _FER ct_CNTRL ct_RRNXT
