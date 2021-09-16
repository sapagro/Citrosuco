FUNCTION zabs_fm_glmd_cancel.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CANCEL) TYPE  ABAP_BOOL DEFAULT 'X'
*"  CHANGING
*"     REFERENCE(CS_MDDOC) TYPE  /AGRI/S_GLMD_DOC OPTIONAL
*"----------------------------------------------------------------------

  DATA : lwa_mdatv   TYPE /agri/s_glmdatv,
         lwa_mditm   TYPE /agri/s_glmditm,
         lt_messages TYPE /agri/t_gprolog,
         lt_mddoc    TYPE /agri/t_glmd_doc,
         lv_object   TYPE tdobject,
         lv_objval   TYPE tdobname.

  IF i_cancel EQ abap_true.
    cs_mddoc-x-mdhdr-canceled = abap_true.
  ELSE.
    cs_mddoc-x-mdhdr-canceled = abap_false.
  ENDIF.

  cs_mddoc-x-mdhdr-updkz    = c_updkz_update.

  APPEND cs_mddoc TO lt_mddoc.

  CALL FUNCTION '/AGRI/GLMD_SAVE'
    CHANGING
      ct_mddoc    = lt_mddoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  IF sy-subrc EQ 0.
*-- Documento de medição &1 cancelado
    MESSAGE ID 'ZFMFP' TYPE 'S' NUMBER '082' WITH cs_mddoc-mdocm.
  ENDIF.

ENDFUNCTION.
