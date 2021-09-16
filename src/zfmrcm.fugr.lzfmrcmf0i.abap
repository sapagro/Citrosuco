*&---------------------------------------------------------------------*
*&      Form  ICON_TEXT_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM icon_text_prepare  USING lv_icon_text TYPE val_text
                     CHANGING lv_actual_tab.
****Prepare tab button with icon, text and tooltip

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_actual_tab
      text                  = lv_icon_text
      info                  = lv_icon_text
      add_stdinf            = 'X'
    IMPORTING
      result                = lv_actual_tab
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ICON_TEXT_PREPARE

*&---------------------------------------------------------------------*
*& Form dose_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM dose_grid_data_prepare.

  DATA: lwa_rclst     TYPE zsc_fmrclst,
        lwa_dose_fcat LIKE LINE OF gt_fmrclst_fcat.

  CHECK ref_grid_rclst IS INITIAL OR
        gs_variables-refresh_dose_grid EQ c_true.

  REFRESH: gt_fmrclst_fcat.

  LOOP AT gs_rcdoc_infocus-x-rclst INTO lwa_rclst.
    MOVE-CORRESPONDING lwa_rclst TO lwa_dose_fcat.
    PERFORM assigned_dose_styles_prepare CHANGING lwa_dose_fcat.
    APPEND lwa_dose_fcat TO gt_fmrclst_fcat.
    CLEAR lwa_dose_fcat.
  ENDLOOP.

*...Vistex-11.06.2019/Begin
*  PERFORM initial_lines_set.
  PERFORM set_dose_grid_lines.
*...Vistex-11.06.2019/End

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIAL_LINES_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initial_lines_set.

  DATA: lv_lines      TYPE i,
        lwa_dose_fcat TYPE zsc_fmrclst_fcat.

  IF gs_variables-document_mode NE c_mode_display.
    DESCRIBE TABLE gt_fmrclst_fcat LINES lv_lines.
    IF lv_lines GE 10.
      lv_lines = lv_lines MOD 10.
      lv_lines = 10 - lv_lines.
    ELSE.
      lv_lines = 10 - lv_lines.
    ENDIF.
    DO lv_lines TIMES.
      lwa_dose_fcat-rcinp = icon_wd_radio_button_empty.
      APPEND lwa_dose_fcat TO gt_fmrclst_fcat.
    ENDDO.
  ENDIF.

  SORT gt_fmrclst_fcat DESCENDING BY posnr.

ENDFORM.

FORM item_lista_tecnica_get CHANGING lt_stpo TYPE tty_stpo.

  DATA: lt_stas TYPE  TABLE OF stas.

  SELECT * FROM stas
    INTO TABLE lt_stas BYPASSING BUFFER
   WHERE ( datuv EQ zsc_fmrchdr-datuv )
     AND ( stlal EQ zsc_fmrchdr-stlal )
     AND ( stlnr EQ zsc_fmrchdr-stlnr )
     AND ( stlty EQ 'M' ).

  IF lt_stas[] IS NOT INITIAL.
    SELECT * FROM stpo
      INTO CORRESPONDING FIELDS OF TABLE lt_stpo BYPASSING BUFFER
      FOR ALL ENTRIES IN lt_stas[]
     WHERE stlkn EQ lt_stas-stlkn
       AND ( datuv  EQ zsc_fmrchdr-datuv )
       AND ( stlnr EQ  zsc_fmrchdr-stlnr )
       AND ( stlty EQ 'M' ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIAL_DATA_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initial_data_check .

  IF zsc_fmrchdr-rctyp IS INITIAL.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '092'.
    SET CURSOR FIELD 'ZSC_FMRCHDR-RCTYP'.
    EXIT.
  ENDIF.

  IF zsc_fmrchdr-werks IS INITIAL.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '092'.
    SET CURSOR FIELD 'ZSC_FMRCHDR-WERKS'.
    EXIT.
  ENDIF.

  IF zsc_fmrchdr-datuv IS INITIAL.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '092'.
    SET CURSOR FIELD 'ZSC_FMRCHDR-DATUV'.
    EXIT.
  ENDIF.

  IF zsc_fmrchdr-datbi IS INITIAL.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '092'.
    SET CURSOR FIELD 'ZSC_FMRCHDR-DATBI'.
    EXIT.
  ENDIF.

  IF zsc_fmrchdr-matnr IS INITIAL.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '092'.
    SET CURSOR FIELD 'ZSC_FMRCHDR-MATNR'.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_DOSE_GRID_LINES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_dose_grid_lines .

  DATA: lt_empty_lines LIKE gt_fmrclst_fcat,
        lwa_dose_fcat  TYPE zsc_fmrclst_fcat.

  DELETE gt_fmrclst_fcat WHERE rcnum IS INITIAL
                           AND werks IS INITIAL
                           AND matnr IS INITIAL
                           AND posnr IS INITIAL
                           AND matnr_ins IS INITIAL
                           AND stlal IS INITIAL.

  SORT gt_fmrclst_fcat BY posnr.

  DATA(lv_lines) = lines( gt_fmrclst_fcat ).

  lt_empty_lines[] = gt_fmrclst_fcat[].
  LOOP AT lt_empty_lines INTO DATA(lwa_line).
    IF lwa_line IS NOT INITIAL.
      DELETE lt_empty_lines INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  IF lines( lt_empty_lines ) LT 10.
    DATA(lv_add_empty) = 10 - lines( lt_empty_lines ).
    DO lv_add_empty TIMES.
      INSERT INITIAL LINE INTO TABLE gt_fmrclst_fcat
        ASSIGNING FIELD-SYMBOL(<lwa_dose>).
    ENDDO.
  ENDIF.

ENDFORM.
