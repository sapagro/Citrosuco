*&---------------------------------------------------------------------*
*& Include          LZFMACAF0P
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form data_tables_prepare
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_MODE
*&      <-- ET_FLDOC
*&---------------------------------------------------------------------*
FORM data_tables_prepare USING    lv_mode
                         CHANGING et_rcdoc TYPE zt_fmrc_doc.

  DATA: lwa_fmrc_doc LIKE LINE OF et_rcdoc.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  LOOP AT gt_rchdr INTO DATA(lwa_rchdr).
    CLEAR lwa_fmrc_doc.

    lwa_fmrc_doc-x-rchdr = lwa_rchdr.
    MOVE-CORRESPONDING lwa_rchdr TO lwa_fmrc_doc.
*...Vistex-11.01.2019/Begin
*    READ TABLE gt_rclst TRANSPORTING NO FIELDS
*      WITH KEY rcnum = lwa_rchdr-rcnum BINARY SEARCH.
*    IF sy-subrc EQ 0.
*...Vistex-11.01.2019/End
      LOOP AT gt_rclst INTO DATA(lwa_rclst) FROM sy-tabix.
        IF lwa_rclst-rcnum NE lwa_rchdr-rcnum.
          EXIT.
        ELSE.
          APPEND lwa_rclst TO lwa_fmrc_doc-x-rclst[].
        ENDIF.
      ENDLOOP.
      LOOP AT gt_rcbom INTO DATA(lwa_rcbom) FROM sy-tabix.
        IF lwa_rcbom-rcnum NE lwa_rchdr-rcnum.
          EXIT.
        ELSE.
          APPEND lwa_rcbom TO lwa_fmrc_doc-x-rcbom[].
        ENDIF.
      ENDLOOP.
      LOOP AT gt_rcvrs INTO DATA(lwa_rcvrs) FROM sy-tabix.
        IF lwa_rcvrs-rcnum NE lwa_rchdr-rcnum.
          EXIT.
        ELSE.
          APPEND lwa_rcvrs TO lwa_fmrc_doc-x-rcvrs[].
        ENDIF.
      ENDLOOP.
*...Vistex-11.01.2019/Begin
*    ENDIF.
*...Vistex-11.01.2019/End
    APPEND lwa_fmrc_doc TO et_rcdoc.
  ENDLOOP.

ENDFORM.                    " DATA_TABLES_PREPARE
