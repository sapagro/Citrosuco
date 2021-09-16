FUNCTION ZFMAC_CHECK_CHANGES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CT_ACDOC) TYPE  ZT_FMAC_DOC
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  DATA: lv_changed TYPE xfld.
  FIELD-SYMBOLS: <lwa_acdoc> TYPE zsc_fmac_doc.

  LOOP AT ct_acdoc ASSIGNING <lwa_acdoc>.

    CALL FUNCTION 'ZFMAC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_acdoc  = <lwa_acdoc>
      CHANGING
        c_changed = <lwa_acdoc>-updkz
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc NE 1.
      lv_changed = c_true.
      <lwa_acdoc>-updkz = c_true.
    ENDIF.

  ENDLOOP.

  IF lv_changed IS INITIAL.
    RAISE no_change.
  ENDIF.

ENDFUNCTION.
