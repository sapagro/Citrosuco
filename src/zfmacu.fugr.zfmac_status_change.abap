FUNCTION ZFMAC_STATUS_CHANGE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TPLNR) TYPE  TPLNR
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_COMMIT_WORK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_ACTIVITY) TYPE  IMAS_ACT_TYPE
*"     REFERENCE(IREF_TEXT) TYPE REF TO /AGRI/CL_GTEXT_PROCESS
*"         OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"      NO_CHANGE
*"      ERROR_WHILE_SAVING
*"--------------------------------------------------------------------

**  DATA: lv_changed,
**        lv_failed,
**        lv_subrc  TYPE sy-subrc,
**        lv_initiator TYPE /agri/gdescr,
**        lwa_tplnr TYPE /agri/s_gltplnr,
**        lwa_flhdr TYPE /agri/s_glflot,
**        lt_tplnr  TYPE /agri/t_gltplnr,
**        lt_flhdr  TYPE /agri/t_glflot,
**        lwa_fldoc TYPE /agri/s_glfl_doc.
**
**  lv_initiator = c_log_initiator-change.
**  messages_init.
**  messages_collect_all.
**  messages_initiator_set lv_initiator c_object-log
**                         c_log_subobject-change.
**
**  lwa_tplnr-tplnr_fl = i_tplnr.
**  APPEND lwa_tplnr TO lt_tplnr.
**
**  CALL FUNCTION '/AGRI/GLFL_READ'
**    EXPORTING
**      it_tplnr       = lt_tplnr
**    IMPORTING
**      et_flhdr       = lt_flhdr
***     ET_IFLOT       =
***     ET_IFLOTX      =
***     ET_ADRC        =
***     ET_ILOA        =
***     ET_IHPA        =
***     ET_FLATG       =
***     ET_FLATV       =
**    EXCEPTIONS
**      no_data_exists = 1
**      OTHERS         = 2.
**  IF sy-subrc <> 0.
**    RAISE no_data_exists.
**  ENDIF.
**
**  IF i_set_update_task = c_true.
**    SET UPDATE TASK LOCAL.
**  ENDIF.
**
**  READ TABLE lt_flhdr INTO lwa_flhdr INDEX 1.
**
**  IF lwa_flhdr-loevm EQ c_true AND
**     i_activity EQ c_activity-delete.
**    RAISE no_change.
**  ELSEIF lwa_flhdr-loevm NE c_true AND
**         i_activity EQ c_activity-undo_delete.
**    RAISE no_change.
**  ENDIF.
**
**  PERFORM change_funloc_status USING i_activity
**                                     lwa_flhdr
**                            CHANGING lv_subrc.
**  IF lv_subrc IS NOT INITIAL.
**    messages_get lv_initiator ct_messages.
**    RAISE error_while_saving.
**    EXIT.
**  ELSE.
**    IF i_activity EQ c_activity-delete.
**      lwa_flhdr-loevm = c_true.
**    ELSEIF i_activity EQ c_activity-undo_delete.
**      CLEAR lwa_flhdr-loevm.
**    ENDIF.
**  ENDIF.
**
**  CLEAR lwa_fldoc.
**  lwa_fldoc-x-flhdr = lwa_flhdr.
**
**  PERFORM tables_complete USING  iref_text
**                        CHANGING lwa_fldoc
**                                 lv_failed.
**  IF lv_failed IS NOT INITIAL.
**    RAISE error_while_saving.
**  ENDIF.
**
**  PERFORM fl_save_status USING i_set_update_task
**                               lwa_fldoc.
**
**  IF NOT i_commit_work IS INITIAL.
**    COMMIT WORK.
**    CALL FUNCTION 'DEQUEUE_ALL'.
**  ENDIF.

ENDFUNCTION.
