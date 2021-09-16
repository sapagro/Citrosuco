FUNCTION zabs_fm_wo_conf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_WOCONF) TYPE  ZABS_STR_OCNF
*"     REFERENCE(IT_WOCON) TYPE  ZABS_TTY_WO_CON
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

*--Refresh Global data
  PERFORM refresh_global_data.

  CLEAR gwa_wo_cnf.
  gwa_wo_cnf = is_woconf.
  gt_wocon_imp = it_wocon.

  PERFORM build_data.

  IF  gt_messages IS INITIAL.
    PERFORM process_data.
  ENDIF.

  IF gt_messages IS INITIAL.
*--Posting Orders
    PERFORM posting_orders.
  ENDIF.

  et_messages = gt_messages.

*--Refresh Global data
  PERFORM refresh_global_data.

ENDFUNCTION.
