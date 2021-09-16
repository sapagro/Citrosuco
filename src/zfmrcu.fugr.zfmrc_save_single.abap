FUNCTION zfmrc_save_single.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_COMMIT_WORK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(IREF_TEXT) TYPE REF TO  /AGRI/CL_GTEXT_PROCESS
*"       OPTIONAL
*"  CHANGING
*"     REFERENCE(CS_RCDOC) TYPE  ZSC_FMRC_DOC
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG OPTIONAL
*"  EXCEPTIONS
*"      NO_CHANGE
*"      ERROR_WHILE_SAVING
*"----------------------------------------------------------------------

  DATA: lv_changed,
        lv_subrc   TYPE sy-subrc,
        lv_failed.

  CALL FUNCTION 'ZFMRC_CHECKCHANGES_SINGLE'
    EXPORTING
      is_rcdoc  = cs_rcdoc
    CHANGING
      c_changed = lv_changed
    EXCEPTIONS
      no_change = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    RAISE no_change.
  ENDIF.

  IF i_set_update_task = c_true.
    SET UPDATE TASK LOCAL.
  ENDIF.

  PERFORM tables_complete USING iref_text
                       CHANGING cs_rcdoc
                                lv_failed.
  IF lv_failed IS NOT INITIAL.
    RAISE error_while_saving.
  ENDIF.

  PERFORM recipe_save USING i_set_update_task
                        cs_rcdoc
                        iref_text.

  IF NOT i_commit_work IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = i_commit_work.
    CALL FUNCTION 'DEQUEUE_ALL'.
  ENDIF.

ENDFUNCTION.
