FUNCTION ZFMRC_CHECKCHANGES_SINGLE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_RCDOC) TYPE  ZSC_FMRC_DOC
*"  CHANGING
*"     REFERENCE(C_CHANGED) TYPE  XFELD
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  CLEAR c_changed.
*--Check Document flag
  IF is_rcdoc-updkz IS NOT INITIAL.
    c_changed = c_true.
    EXIT.
  ENDIF.

*--Check X Data
  PERFORM rcdoc_check_changes USING is_rcdoc-x
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
*--Check Y Data
  PERFORM rcdoc_check_changes USING is_rcdoc-y
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
  RAISE no_change.

ENDFUNCTION.
