{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.PayoffFs where

import           Data.Maybe                                        (fromJust)
import           Data.Time                                         (Day)
import           Language.Marlowe                                  (Observation, Value)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents (EventType (FP, IED, IP, MD, PP, PR, PRD, PY, TD))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms  (CT (..), ContractTerms (..))
import           Language.Marlowe.ACTUS.MarloweCompat              (constnt, enum, useval)
import           Language.Marlowe.ACTUS.Model.POF.PayoffModel
import           Language.Marlowe.ACTUS.Ops                        (ActusNum (..), YearFractionOps (_y),
                                                                    marloweFixedPoint)
import           Prelude                                           hiding (Fractional, Num, (*), (+), (-), (/))

payoffFs :: EventType -> ContractTerms -> Integer -> Integer -> Day -> Day -> Maybe (Value Observation)
payoffFs ev ContractTerms{..} t t_minus prevDate curDate =
    let _NT              = constnt (fromJust ct_NT)
        _PDIED           = constnt (fromJust ct_PDIED)
        _PYTP            = enum (fromJust ct_PYTP)
        _FEB             = enum (fromJust ct_FEB)
        _FER             = constnt (fromJust ct_FER)
        (_PPRD, _PTD  ) = (constnt (fromJust ct_PPRD), constnt (fromJust ct_PTD))
        (_PYRT, _cPYRT) = (constnt (fromJust ct_PYRT), constnt ct_cPYRT)


        _o_rf_CURS       = useval "o_rf_CURS" t
        _o_rf_RRMO       = useval "o_rf_RRMO" t
        _pp_payoff       = useval "pp_payoff" t
        _nsc             = useval "nsc" t_minus
        _nt              = useval "nt" t_minus
        _isc             = useval "isc" t_minus
        _ipac            = useval "ipac" t_minus
        _feac            = useval "feac" t_minus
        _ipnr            = useval "ipnr" t_minus
        _ipcb            = useval "ipcb" t_minus
        _prnxt           = useval "prnxt" t_minus

        y_sd_t            = constnt $ _y (fromJust ct_DCC) prevDate curDate ct_MD

        pof = case contractType of

            PAM -> case ev of
                IED -> Just $ _POF_IED_PAM _o_rf_CURS ct_CNTRL _NT _PDIED
                MD  -> Just $ _POF_MD_PAM _o_rf_CURS _nsc _nt _isc _ipac _feac
                PP  -> Just $ _POF_PP_PAM _o_rf_CURS _pp_payoff
                PY  -> Just $ _POF_PY_PAM _PYTP _o_rf_CURS _o_rf_RRMO _PYRT _cPYRT ct_CNTRL _nt _ipnr y_sd_t
                FP  -> Just $ _POF_FP_PAM _FEB _FER _o_rf_CURS ct_CNTRL _nt _feac y_sd_t
                PRD -> Just $ _POF_PRD_PAM _o_rf_CURS ct_CNTRL _PPRD _ipac _ipnr _nt y_sd_t
                TD  -> Just $ _POF_TD_PAM _o_rf_CURS ct_CNTRL _PTD _ipac _ipnr _nt y_sd_t
                IP  -> Just $ _POF_IP_PAM _o_rf_CURS _isc _ipac _ipnr _nt y_sd_t
                _   -> Nothing

            LAM -> case ev of
                IED -> Just $ _POF_IED_PAM _o_rf_CURS ct_CNTRL _NT _PDIED
                PR  -> Just $ _POF_PR_LAM _o_rf_CURS ct_CNTRL _nsc _prnxt
                MD  -> Just $ _POF_MD_PAM _o_rf_CURS _nsc _nt _isc _ipac _feac
                PP  -> Just $ _POF_PP_PAM _o_rf_CURS _pp_payoff
                PY  -> Just $ _POF_PY_PAM _PYTP _o_rf_CURS _o_rf_RRMO _PYRT _cPYRT ct_CNTRL _nt _ipnr y_sd_t
                FP  -> Just $ _POF_FP_PAM _FEB _FER _o_rf_CURS ct_CNTRL _nt _feac y_sd_t
                PRD -> Just $ _POF_PRD_LAM _o_rf_CURS ct_CNTRL _PPRD _ipac _ipnr _ipcb y_sd_t
                TD  -> Just $ _POF_TD_LAM _o_rf_CURS ct_CNTRL _PTD _ipac _ipnr _ipcb y_sd_t
                IP  -> Just $ _POF_IP_LAM _o_rf_CURS _isc _ipac _ipnr _ipcb y_sd_t
                _   -> Nothing

            NAM -> case ev of
                IED -> Just $ _POF_IED_PAM  _o_rf_CURS ct_CNTRL _NT _PDIED
                PR  -> Just $ _POF_PR_NAM _o_rf_CURS _nsc _prnxt _ipac y_sd_t _ipnr _ipcb
                MD  -> Just $ _POF_MD_PAM _o_rf_CURS _nsc _nt _isc _ipac _feac
                PP  -> Just $ _POF_PP_PAM _o_rf_CURS _pp_payoff
                PY  -> Just $ _POF_PY_PAM _PYTP _o_rf_CURS _o_rf_RRMO _PYRT _cPYRT ct_CNTRL _nt _ipnr y_sd_t
                FP  -> Just $ _POF_FP_PAM _FEB _FER _o_rf_CURS ct_CNTRL _nt _feac y_sd_t
                PRD -> Just $ _POF_PRD_LAM _o_rf_CURS ct_CNTRL _PPRD _ipac _ipnr _ipcb y_sd_t
                TD  -> Just $ _POF_TD_LAM _o_rf_CURS ct_CNTRL _PTD _ipac _ipnr _ipcb y_sd_t
                IP  -> Just $ _POF_IP_LAM _o_rf_CURS _isc _ipac _ipnr _ipcb y_sd_t
                _   -> Nothing

            ANN -> case ev of
                IED -> Just $ _POF_IED_PAM  _o_rf_CURS ct_CNTRL _NT _PDIED
                PR  -> Just $ _POF_PR_NAM _o_rf_CURS _nsc _prnxt _ipac y_sd_t _ipnr _ipcb
                MD  -> Just $ _POF_MD_PAM _o_rf_CURS _nsc _nt _isc _ipac _feac
                PP  -> Just $ _POF_PP_PAM _o_rf_CURS _pp_payoff
                PY  -> Just $ _POF_PY_PAM _PYTP _o_rf_CURS _o_rf_RRMO _PYRT _cPYRT ct_CNTRL _nt _ipnr y_sd_t
                FP  -> Just $ _POF_FP_PAM _FEB _FER _o_rf_CURS ct_CNTRL _nt _feac y_sd_t
                PRD -> Just $ _POF_PRD_LAM _o_rf_CURS ct_CNTRL _PPRD _ipac _ipnr _ipcb y_sd_t
                TD  -> Just $ _POF_TD_LAM _o_rf_CURS ct_CNTRL _PTD _ipac _ipnr _ipcb y_sd_t
                IP  -> Just $ _POF_IP_LAM _o_rf_CURS _isc _ipac _ipnr _ipcb y_sd_t
                _   -> Nothing

    in (\x -> x / (constnt $ fromIntegral marloweFixedPoint)) <$> pof
