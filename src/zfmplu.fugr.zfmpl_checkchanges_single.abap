FUNCTION ZFMPL_CHECKCHANGES_SINGLE.
*"----------------------------------------------------------------------
*"*"Interfple local:
*"  IMPORTING
*"     REFERENCE(IS_PLDOC) TYPE  ZSC_FMPL_DOC
*"  CHANGING
*"     REFERENCE(C_CHANGED)
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  CLEAR c_changed.
*--Check Document flag
  IF is_pldoc-updkz IS NOT INITIAL.
    c_changed = c_true.
    EXIT.
  ENDIF.

*--Check X Data
  PERFORM pldoc_check_changes USING is_pldoc-x
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
*--Check Y Data
  PERFORM pldoc_check_changes USING is_pldoc-y
                           CHANGING c_changed.

  CHECK c_changed IS INITIAL.
  RAISE no_change.

ENDFUNCTION.
