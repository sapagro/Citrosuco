*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0E
*&---------------------------------------------------------------------*
FORM exit_processing.

  DATA: lv_answer.

  fcode = ok_code.
  CLEAR ok_code.

  REFRESH: gr_aufnrtmp[],
           gr_aufnr[].

  CASE sy-dynnr.
    WHEN c_screen-main_screen.
****Check whether there are any changes
      PERFORM changes_confirm CHANGING lv_answer.

      IF lv_answer NE 'A'.
        IF gs_variables-document_mode NE c_mode_display.
          PERFORM document_infocus_unlock
            USING gs_acdoc_infocus-x-achdr-accom.
        ENDIF.
      ELSE.
        IF ok_code EQ c_fcode-save.
          IF fcode EQ c_fcode-exit.
            gs_variables-exit_after_save = c_true.
          ELSE.
            gs_variables-exit_after_save = 'C'.
          ENDIF.
          ok_code = c_fcode-back.
        ENDIF.
      ENDIF.
      CHECK lv_answer NE 'A'.

      IF fcode EQ c_fcode-exit.
        SET SCREEN 0.
        LEAVE SCREEN.
      ELSE.
        PERFORM document_data_initialize USING c_true.
        SET SCREEN 100.
        LEAVE SCREEN.
      ENDIF.
    WHEN c_screen-create_accom.
      CLEAR: gs_acdoc_infocus, /agri/s_fmachdr.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN c_screen-items.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN c_screen-multi_lang_desc.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " EXIT_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  EQUNR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_ACITM>_EQUNR  text
*      -->P_<LWA_ACITM>_AUFNR  text
*----------------------------------------------------------------------*
FORM equnr_check  USING    lv_equnr TYPE  /agri/fm_equnr
                           lv_aufnr TYPE aufnr.

  DATA:lwa_resource      TYPE /agri/fmacres,
       lwa_activity      TYPE /agri/fmacact,
       lt_activity       TYPE /agri/t_fmac_src_act,
       lwa_fp            TYPE /agri/s_fmac_fp,
       lv_minutes        TYPE i,
       lt_aufnr          TYPE /agri/t_fmaufnr,
       lt_fpdoc          TYPE /agri/t_fmfp_doc,
       lwa_aufnr         TYPE /agri/s_fmaufnr,
       lwa_fpdoc         TYPE /agri/s_fmfp_doc,
       ls_aufnr          TYPE /agri/s_fmaufnr,
       lt_fmwo_doc       TYPE /agri/t_fmwoc_doc,
       lwa_wodoc_infocus TYPE /agri/s_fmwoc_doc,
       lt_wonum          TYPE /agri/t_fmwonum,
       lwa_t006          TYPE t006,
       ls_afvc           TYPE /agri/s_fmac_src_ord,
       lv_aufpl          TYPE afko-aufpl,
       lv_idactv         TYPE /agri/fmacact-idactv,
       lv_auszt          TYPE auszt,
       ls_items_mod_rows TYPE lvc_s_modi,
       lt_arbpl          TYPE /agri/t_range_arbpl,
       lt_resource       TYPE /agri/t_fmac_src_res,
       lt_vgwts          TYPE /agri/t_vgwts,
       lwa_vgwts         TYPE /agri/s_vgwts,
       lt_parameters     TYPE /agri/t_parameters_tc21,
       lwa_parameters    TYPE /agri/s_parameters_tc21,
       lt_tc20           TYPE /agri/t_parameters_tc20,
       lwa_tc20          TYPE tc20,
       lwa_fmachdr       TYPE /agri/s_fmachdr,
       lt_fmacrsc        TYPE /agri/t_fmacrsc,
       ls_fmacrsc        TYPE /agri/s_fmacrsc.

  SELECT SINGLE * FROM /agri/fmacres
        INTO lwa_resource
        WHERE idresource EQ lv_equnr "#EC CI_NOORDER
          AND rstype     EQ c_rstype-equnr.

  IF sy-subrc NE 0.
    MESSAGE ID '/AGRI/FMAC'
            TYPE c_msg_type-error
            NUMBER '031'
            WITH lv_equnr
            INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ELSE.

    SELECT arbpl vgwts
      FROM crhd
      INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
*          FOR ALL ENTRIES IN lt_resource
      WHERE arbpl = lwa_resource-arbpl.

    DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED

    IF lt_vgwts IS NOT INITIAL.
      SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
      INTO CORRESPONDING FIELDS OF TABLE lt_parameters
      FROM tc21
      FOR ALL ENTRIES IN lt_vgwts
      WHERE vgwts = lt_vgwts-vgwts.
    ENDIF.

    IF lt_parameters IS NOT INITIAL.
      SELECT parid                            "#EC CI_FAE_LINES_ENSURED
      INTO CORRESPONDING FIELDS OF TABLE lt_tc20
      FROM tc20
      FOR ALL ENTRIES IN lt_parameters
      WHERE parid = lt_parameters-par01
        OR  parid = lt_parameters-par02
        OR  parid = lt_parameters-par03
        OR  parid = lt_parameters-par04
        OR  parid = lt_parameters-par05
        OR  parid = lt_parameters-par06.
    ENDIF.

*    READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.

    IF lt_tc20 IS NOT INITIAL.
      SELECT actyp parid rstyp txtlg          "#EC CI_FAE_LINES_ENSURED
      FROM /agri/tfmacrsc
      INTO TABLE lt_fmacrsc
      FOR ALL ENTRIES IN lt_tc20
      WHERE parid = lt_tc20-parid
*      AND actyp = lwa_fmachdr-actyp.
      AND actyp = gs_acdoc_infocus-x-achdr-actyp.
    ENDIF.

    IF lt_fmacrsc IS NOT INITIAL.
      LOOP AT lt_vgwts INTO lwa_vgwts.
        READ TABLE lt_parameters INTO lwa_parameters WITH KEY vgwts = lwa_vgwts-vgwts.
        IF sy-subrc EQ 0.
          LOOP AT lt_fmacrsc TRANSPORTING NO FIELDS WHERE ( parid = lwa_parameters-par01
                                                       OR parid = lwa_parameters-par02
                                                       OR parid = lwa_parameters-par03
                                                       OR parid = lwa_parameters-par04
                                                       OR parid = lwa_parameters-par05
                                                       OR parid = lwa_parameters-par06 )
                                                      AND rstyp EQ c_accom_id-equipment.
          ENDLOOP.

          IF sy-subrc NE 0.
            MESSAGE ID '/AGRI/FMAC'
            TYPE c_msg_type-error
            NUMBER '049'
            WITH lv_equnr
                 lv_aufnr
          INTO sy-msgli.
            gs_variables-errors = c_true.
            message_simple space.
            EXIT. "#EC CI_NOORDER
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE ID '/AGRI/FMAC'
            TYPE c_msg_type-error
            NUMBER '031'
            WITH lv_equnr
            INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
  ENDIF.


ENDFORM.                    " EQUNR_CHECK

*&---------------------------------------------------------------------*
*&      Form  EMPLOYEE_ID_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACITM  text
*      <--P_LT_FMACRES  text
*----------------------------------------------------------------------*
FORM employee_id_get  USING    lt_acitm TYPE /agri/t_fmacitm
                      CHANGING lt_fmacres TYPE /agri/t_fmacres.

  CHECK lt_acitm[] IS NOT INITIAL.
  SELECT * FROM /agri/fmacres
           INTO TABLE lt_fmacres
    FOR ALL ENTRIES IN lt_acitm
  WHERE idresource EQ lt_acitm-idresource.

ENDFORM.                    "employee_id_get
