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
                         CHANGING et_acdoc TYPE zt_fmac_doc.

  DATA: lwa_fmac_doc LIKE LINE OF et_acdoc.

  LOOP AT gt_achdr INTO DATA(lwa_achdr).
    CLEAR lwa_fmac_doc.

    lwa_fmac_doc-x-achdr = lwa_achdr.

    READ TABLE gt_acitm TRANSPORTING NO FIELDS
      WITH KEY acnum = lwa_achdr-acnum BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT gt_acitm INTO DATA(lwa_acitm) FROM sy-tabix.
        IF lwa_acitm-acnum NE lwa_achdr-acnum.
          EXIT.
        ELSE.
          APPEND lwa_acitm TO lwa_fmac_doc-x-acitm[].
        ENDIF.
      ENDLOOP.
      LOOP AT gt_acvlc INTO DATA(lwa_acvlc) FROM sy-tabix.
        IF lwa_acvlc-acnum NE lwa_achdr-acnum.
          EXIT.
        ELSE.
          APPEND lwa_acvlc TO lwa_fmac_doc-x-acvlc[].
        ENDIF.
      ENDLOOP.
    ENDIF.

    APPEND lwa_fmac_doc TO et_acdoc.
  ENDLOOP.

ENDFORM.                    " DATA_TABLES_PREPARE
