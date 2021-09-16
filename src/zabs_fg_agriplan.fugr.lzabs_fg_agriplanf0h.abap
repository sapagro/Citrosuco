*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_display .

  CLEAR: zsc_fmachdr.
  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO zsc_fmachdr.

ENDFORM.                    " HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_update.

  DATA: ls_fmachdr TYPE zsc_fmachdr.

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_fmachdr.

  IF ls_fmachdr NE zsc_fmachdr.
    MOVE-CORRESPONDING zsc_fmachdr TO gs_acdoc_infocus-x-achdr.

    IF sy-dynnr EQ '0202'
    AND gs_variables-copy EQ abap_true
    AND gs_acdoc_infocus-x-achdr-acnum_ref IS NOT INITIAL
    AND gs_variables-document_mode EQ c_mode_create.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_new.
    ENDIF.

    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.

    gs_variables-data_changed = c_true.
    gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
  ENDIF.

ENDFORM.                    " HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_check .
ENDFORM.                    " HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*& Form HEADER_CROPAREA_FILL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM header_croparea_fill .


  DATA: lv_txtgr TYPE txtgr.

  PERFORM document_data_initialize USING c_true.

  gs_variables-document_mode = c_mode_create.
*  gs_variables-overview_mode
  gs_acdoc_infocus-acnum = TEXT-046.
  gs_acdoc_infocus-x-achdr-acnum = TEXT-046.

  MOVE p_acdes TO zsc_fmachdr-acdes.
  MOVE p_actyp TO zsc_fmachdr-actyp.
  MOVE p_ajahr TO zsc_fmachdr-ajahr.
  MOVE p_datab TO zsc_fmachdr-datab.
  MOVE p_datbi TO zsc_fmachdr-datbi.
*  MOVE p_werks TO zsc_fmachdr-werks.

*...BOC-T_T.KONNO
  READ TABLE so_werks INTO DATA(lwa_werks) INDEX 1.
  IF sy-subrc EQ 0.
    MOVE lwa_werks-low TO zsc_fmachdr-werks.
  ENDIF.
*...EOC-T_T.KONNO
  MOVE c_updkz_new  TO zsc_fmachdr-updkz.
  MOVE TEXT-046 TO zsc_fmachdr-acnum.

  MOVE-CORRESPONDING zsc_fmachdr TO gs_acdoc_infocus-x-achdr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIGH_DATE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM high_date_check.

  IF p_datab GT p_datbi.
    MESSAGE ID 'DB' TYPE c_msg_type-error NUMBER '650' INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

ENDFORM.
