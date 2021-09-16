FUNCTION ZFMRC_VIEW .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MODE) TYPE  RV56A-SELKZ DEFAULT 'V'
*"     REFERENCE(IT_RCNUM) TYPE  ZT_FMRCNUM
*"  EXPORTING
*"     REFERENCE(ET_RCDOC) TYPE  ZT_FMRC_DOC
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------

  REFRESH et_rcdoc[].
  IF it_rcnum[] IS INITIAL.
    RAISE no_data_exists.
  ENDIF.

  CALL FUNCTION 'ZFMRC_READ'
    EXPORTING
      it_rcnum       = it_rcnum
    IMPORTING
      et_rchdr       = gt_rchdr
      et_rclst       = gt_rclst
      et_rcbom       = gt_rcbom
      et_rcvrs       = gt_rcvrs
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    RAISE no_data_exists.
  ENDIF.

  PERFORM data_tables_prepare USING i_mode
                              CHANGING et_rcdoc.

ENDFUNCTION.
