FUNCTION ZFMAC_CHECKCHANGES_SINGLE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_ACDOC) TYPE  ZSC_FMAC_DOC
*"  CHANGING
*"     REFERENCE(C_CHANGED) TYPE  XFELD
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  CLEAR c_changed.
*--Check Document flag
  IF is_acdoc-updkz IS NOT INITIAL.
    c_changed = c_true.
    EXIT.
  ENDIF.

*--Check X Data
  PERFORM acdoc_check_changes USING is_acdoc-x
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
*--Check Y Data
  PERFORM acdoc_check_changes USING is_acdoc-y
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
  RAISE no_change.

ENDFUNCTION.
