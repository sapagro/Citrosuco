*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_display .

  CLEAR: zsc_fmrchdr.
  MOVE-CORRESPONDING gs_rcdoc_infocus-x-rchdr TO zsc_fmrchdr.

ENDFORM.                    " HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_update .

  DATA: ls_fmrchdr     TYPE zsc_fmrchdr.


  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  MOVE-CORRESPONDING gs_rcdoc_infocus-x-rchdr TO ls_fmrchdr.

  IF ls_fmrchdr NE zsc_fmrchdr.
    IF gs_variables-document_mode NE c_mode_create.
      IF ls_fmrchdr-rctyp NE zsc_fmrchdr-rctyp.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING zsc_fmrchdr TO gs_rcdoc_infocus-x-rchdr.
    IF gs_rcdoc_infocus-x-rchdr-updkz NE c_updkz_new.
      gs_rcdoc_infocus-x-rchdr-updkz = c_updkz_update.
    ENDIF.

    gs_variables-data_changed = c_true.
    gs_rcdoc_infocus-rcnum = gs_rcdoc_infocus-x-rchdr-rcnum.
    gs_rcdoc_infocus-matnr = gs_rcdoc_infocus-x-rchdr-matnr.
    gs_rcdoc_infocus-werks = gs_rcdoc_infocus-x-rchdr-werks.

*-- BOC-T_T.KONNO-122719
    LOOP AT gs_rcdoc_infocus-x-rclst ASSIGNING FIELD-SYMBOL(<lwa_rclst>).
      <lwa_rclst>-matnr = gs_rcdoc_infocus-matnr.
    ENDLOOP.
*-- EOC-T_T.KONNO-122719
  ENDIF.

ENDFORM.                    " HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_check .
ENDFORM.                    " HEADER_DATA_CHECK
*--------------------------------------------------*
