FUNCTION ZFMPL_CHECK_CHANGES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CT_PLDOC) TYPE  ZT_FMPL_DOC
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  DATA: lv_changed TYPE xfld.
  FIELD-SYMBOLS: <lwa_pldoc> TYPE zsc_fmpl_doc.

  LOOP AT ct_pldoc ASSIGNING <lwa_pldoc>.

    CALL FUNCTION 'ZFMPL_CHECKCHANGES_SINGLE'
      EXPORTING
        is_pldoc  = <lwa_pldoc>
      CHANGING
        c_changed = <lwa_pldoc>-updkz
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc NE 1.
      lv_changed = c_true.
      <lwa_pldoc>-updkz = c_true.
    ENDIF.

  ENDLOOP.

  IF lv_changed IS INITIAL.
    RAISE no_change.
  ENDIF.

ENDFUNCTION.
