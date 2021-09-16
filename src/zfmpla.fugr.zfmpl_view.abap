FUNCTION ZFMPL_VIEW.
*"----------------------------------------------------------------------
*"*"Interfple local:
*"  IMPORTING
*"     REFERENCE(I_MODE) TYPE  RV56A-SELKZ DEFAULT 'V'
*"     REFERENCE(IT_PLNUM) TYPE  ZT_FM_PLNUM
*"  EXPORTING
*"     REFERENCE(ET_PLDOC) TYPE  ZT_FMPL_DOC
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------

  REFRESH et_pldoc[].
  IF it_plnum[] IS INITIAL.
    RAISE no_data_exists.
  ENDIF.

  CALL FUNCTION 'ZFMPL_READ'
    EXPORTING
      it_plnum       = it_plnum
    IMPORTING
      et_plhdr       = gt_plhdr
      et_plitm       = gt_plitm
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    RAISE no_data_exists.
  ENDIF.

  PERFORM data_tables_prepare USING i_mode
                              CHANGING et_pldoc.

ENDFUNCTION.
