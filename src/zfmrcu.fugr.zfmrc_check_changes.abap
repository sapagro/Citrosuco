FUNCTION zfmrc_check_changes.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CT_RCDOC) TYPE  ZT_FMRC_DOC
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  DATA: lv_changed TYPE xfld.
  FIELD-SYMBOLS: <lwa_rcdoc> TYPE zsc_fmrc_doc.

  LOOP AT ct_rcdoc ASSIGNING <lwa_rcdoc>.

    CALL FUNCTION 'ZFMRC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_rcdoc  = <lwa_rcdoc>
      CHANGING
        c_changed = <lwa_rcdoc>-updkz
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc NE 1.
      lv_changed = c_true.
      <lwa_rcdoc>-updkz = c_true.
    ENDIF.

  ENDLOOP.

  IF lv_changed IS INITIAL.
    RAISE no_change.
  ENDIF.

ENDFUNCTION.
