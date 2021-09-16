FUNCTION zfmpl_order_schedule.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_AUFNR) TYPE  /AGRI/T_FMAUFNR
*"     VALUE(IT_TPLNR_FL) TYPE  /AGRI/T_GLTPLNR
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------


  DATA: lt_fmfp_doc  TYPE /agri/t_fmfp_doc,
        lwa_fmfp_doc TYPE /agri/s_fmfp_doc,
        lwa_gltplnr  TYPE /agri/s_gltplnr,
        lt_fmfphdr   TYPE /agri/t_fmfphdr.

  FIELD-SYMBOLS: <lwa_fmfphdr>   TYPE /agri/s_fmfphdr.

  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = it_aufnr
    IMPORTING
      et_fpdoc       = lt_fmfp_doc[]
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF lt_fmfp_doc[] IS NOT INITIAL.
    LOOP AT lt_fmfp_doc INTO lwa_fmfp_doc.
      READ TABLE it_tplnr_fl INTO lwa_gltplnr
                             WITH KEY = lwa_fmfp_doc-x-fphdr-tplnr_fl.
      IF sy-subrc EQ 0.
        MOVE c_true TO lwa_fmfp_doc-x-fphdr-schta.
        MOVE c_updkz_new TO lwa_fmfp_doc-x-fphdr-updkz.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION '/AGRI/FMFP_SAVE'
    EXPORTING
      i_set_update_task = 'X'
      i_commit_work     = 'X'
    CHANGING
      ct_fpdoc          = lt_fmfp_doc
      ct_messages       = ct_messages
    EXCEPTIONS
      no_change         = 1
      OTHERS            = 2.

ENDFUNCTION.
