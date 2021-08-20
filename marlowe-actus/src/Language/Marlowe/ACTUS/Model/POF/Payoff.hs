{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.Payoff where

import           Control.Applicative                               (liftA2)
import           Data.Maybe                                        (fromMaybe)
import           Data.Time                                         (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents (EventType (..), RiskFactors (..))
import           Language.Marlowe.ACTUS.Definitions.ContractState  (ContractState, ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms  (CT (..), ContractTerms (..))
import           Language.Marlowe.ACTUS.Model.POF.PayoffModel
import           Language.Marlowe.ACTUS.Ops                        (YearFractionOps (_y))

payoff :: EventType -> RiskFactors -> ContractTerms -> ContractState -> Day -> Double
payoff ev RiskFactors{..} ContractTerms{..} ContractStatePoly{..} t =
    case contractType of

        PAM ->
            case ev of
                IED -> pof_IED_PAM
                MD  -> pof_MD_PAM
                PP  -> pof_PP_PAM
                PY  -> pof_PY_PAM
                FP  -> pof_FP_PAM
                PRD -> pof_PRD_PAM
                TD  -> pof_TD_PAM
                IP  -> pof_IP_PAM
                _   -> 0.0

        LAM ->
            case ev of
                IED -> pof_IED_PAM
                PR  -> pof_PR_LAM
                MD  -> pof_MD_PAM
                PP  -> pof_PP_PAM
                PY  -> pof_PY_PAM
                FP  -> pof_FP_PAM
                PRD -> pof_PRD_LAM
                TD  -> pof_TD_LAM
                IP  -> pof_IP_LAM
                _   -> 0.0

        NAM ->
            case ev of
                IED -> pof_IED_PAM
                PR  -> pof_PR_NAM
                MD  -> pof_MD_PAM
                PP  -> pof_PP_PAM
                PY  -> pof_PY_PAM
                FP  -> pof_FP_PAM
                PRD -> pof_PRD_LAM
                TD  -> pof_TD_LAM
                IP  -> pof_IP_LAM
                _   -> 0.0

        ANN ->
            case ev of
                IED -> pof_IED_PAM
                PR  -> pof_PR_NAM
                MD  -> pof_MD_PAM
                PP  -> pof_PP_PAM
                PY  -> pof_PY_PAM
                FP  -> pof_FP_PAM
                PRD -> pof_PRD_LAM
                TD  -> pof_TD_LAM
                IP  -> pof_IP_LAM
                _   -> 0.0

  where
    y_sd_t = ct_DCC >>= \_DCC -> pure $ _y _DCC sd t ct_MD

    -- PAM

    pof_IED_PAM = getValue $ liftA2 (_POF_IED_PAM o_rf_CURS ct_CNTRL) ct_NT ct_PDIED

    pof_MD_PAM = _POF_MD_PAM o_rf_CURS nsc nt isc ipac (if isNaN feac then 0.0 else feac)

    pof_PP_PAM = _POF_PP_PAM o_rf_CURS pp_payoff

    pof_PY_PAM = getValue $ do
      _PYTP <- ct_PYTP
      _PYRT <- ct_PYRT
      y_sd_t' <- y_sd_t
      pure $ _POF_PY_PAM _PYTP o_rf_CURS o_rf_RRMO _PYRT ct_cPYRT ct_CNTRL nt ipnr y_sd_t'

    pof_FP_PAM = getValue $ do
      _FEB <- ct_FEB
      _FER <- ct_FER
      y_sd_t' <- y_sd_t
      pure $ _POF_FP_PAM _FEB _FER o_rf_CURS ct_CNTRL nt feac y_sd_t'

    pof_PRD_PAM = getValue $ ct_PPRD >>= \_PPRD -> y_sd_t >>= \y_sd_t' -> pure $ _POF_PRD_PAM o_rf_CURS ct_CNTRL _PPRD ipac ipnr nt y_sd_t'

    pof_TD_PAM = getValue $ ct_PTD >>= \_PTD -> y_sd_t >>= \y_sd_t' -> pure $ _POF_TD_PAM o_rf_CURS ct_CNTRL _PTD ipac ipnr nt y_sd_t'

    pof_IP_PAM = getValue $ y_sd_t >>= \y_sd_t' -> pure $ _POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t'

    -- LAM

    pof_PR_LAM = _POF_PR_LAM o_rf_CURS ct_CNTRL nsc prnxt

    pof_PRD_LAM = getValue $ ct_PPRD >>= \_PPRD -> y_sd_t >>= \y_sd_t' -> pure $_POF_PRD_LAM o_rf_CURS ct_CNTRL _PPRD ipac ipnr ipcb y_sd_t'

    pof_TD_LAM = getValue $ ct_PTD >>= \_PTD -> y_sd_t >>= \y_sd_t' -> pure $ _POF_TD_LAM o_rf_CURS ct_CNTRL _PTD ipac ipnr ipcb y_sd_t'

    pof_IP_LAM = getValue $ y_sd_t >>= \y_sd_t' -> pure $ _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t'

    -- NAN

    pof_PR_NAM = getValue $ y_sd_t >>= \y_sd_t' -> pure $ _POF_PR_NAM o_rf_CURS nsc prnxt ipac y_sd_t' ipnr ipcb

    getValue :: Maybe Double -> Double
    getValue = fromMaybe nan

    nan :: Double
    nan = 0/0
