FUNCTION ZFMAC_VIEW.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MODE) TYPE  RV56A-SELKZ DEFAULT 'V'
*"     REFERENCE(IT_ACNUM) TYPE  ZT_FM_ACNUM
*"  EXPORTING
*"     REFERENCE(ET_ACDOC) TYPE  ZT_FMAC_DOC
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------

  REFRESH et_acdoc[].
  IF it_acnum[] IS INITIAL.
    RAISE no_data_exists.
  ENDIF.

  CALL FUNCTION 'ZFMAC_READ'
    EXPORTING
      it_acnum       = it_acnum
    IMPORTING
      et_achdr       = gt_achdr
      et_acitm       = gt_acitm
      et_acvlc       = gt_acvlc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    RAISE no_data_exists.
  ENDIF.

  PERFORM data_tables_prepare USING i_mode
                              CHANGING et_acdoc.

ENDFUNCTION.
