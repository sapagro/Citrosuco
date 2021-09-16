*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0I
*&---------------------------------------------------------------------*
FORM items_prepare.

  DATA: lwa_items_layout TYPE /agri/s_fmacitm_layout,
        lv_subrc         TYPE subrc,
        lwa_details      TYPE ty_details.

  FIELD-SYMBOLS: <lwa_fmacitm> TYPE /agri/s_fmacitm.

  CHECK ref_grid_items IS INITIAL
     OR gs_variables-refresh_items_grid IS NOT INITIAL.

*  CLEAR: gt_items_layout.
  CHECK ref_grid_items IS NOT INITIAL.

  IF gs_variables-document_mode = c_mode_change
    AND gs_variables-refresh_items_grid IS INITIAL.
    PERFORM document_infocus_lock USING /agri/s_fmachdr-accom
                                  CHANGING lv_subrc.
    IF lv_subrc NE 0.
      gs_variables-document_mode =
      gs_variables-overview_mode = c_mode_display.
    ENDIF.
  ENDIF.

  IF lines( gs_acdoc_infocus-x-acitm ) < lines( gt_items_layout ) .
  ELSE.
    CLEAR: gt_items_layout.
    LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_fmacitm>.
      CLEAR lwa_items_layout.
*      READ TABLE gt_details INTO lwa_details WITH KEY aufnr = <lwa_fmacitm>-aufnr.
*      IF sy-subrc EQ 0.
*      <lwa_fmacitm>-tplnr = <lwa_fmacitm>-zztplnr.
      MOVE-CORRESPONDING <lwa_fmacitm> TO lwa_items_layout.
      PERFORM item_details_get USING space
                               CHANGING lwa_items_layout.
      PERFORM item_styles_prepare CHANGING lwa_items_layout.

      PERFORM catalog_line_style_change CHANGING lwa_items_layout.

      PERFORM description_get CHANGING lwa_items_layout.

      APPEND lwa_items_layout TO gt_items_layout.
*      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " ITEMS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ICON_TEXT_PREPARE
*&---------------------------------------------------------------------*
FORM icon_text_prepare  USING lv_icon_text TYPE val_text
                     CHANGING lv_actual_tab.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_actual_tab
      text                  = lv_icon_text
      info                  = lv_icon_text
      add_stdinf            = 'X'
    IMPORTING
      result                = lv_actual_tab
    EXCEPTIONS ##FM_SUBRC_OK
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " ICON_TEXT_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ITEMS_GRID_UPDATE
*&---------------------------------------------------------------------*
FORM items_grid_update .

  DATA: lwa_orders      TYPE /agri/s_aufnr,
        lt_orders       TYPE /agri/t_aufnr,
        lwa_acitm       TYPE /agri/s_fmacitm,
        lwa_item_layout TYPE /agri/s_fmacitm_layout,
        lwa_mod_row     TYPE lvc_s_modi,
        lv_tabix        TYPE sy-tabix,
        lv_answer       TYPE c,
        lv_modified,
        lv_subrc,
        lv_valid,
        lwa_row_id      TYPE lvc_s_row,
        lwa_col_id      TYPE lvc_s_col,
        lv_tplnr_fl     TYPE /agri/gltplnr_fl.
*        lt_arbpl        TYPE /agri/t_range_arbpl.

  FIELD-SYMBOLS: <lwa_acitm>       TYPE /agri/s_fmacitm,
                 <lwa_item_layout> TYPE /agri/s_fmacitm_layout.

  CHECK gs_variables-document_mode NE c_mode_display.

  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING  gs_variables-initiator
                                     c_log_subobject-check
                                     gs_acdoc_infocus-x-achdr.

  IF ok_code EQ c_fcode-item_delete.
    PERFORM fcode_item_delete CHANGING lv_answer.
    CLEAR ok_code.
  ENDIF.
  CHECK lv_answer IS INITIAL.

  lv_modified = ref_grid_items->data_modified_check( ).
  IF lv_modified EQ c_true OR
     gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_grid_items->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_items_grid = c_true.
    ENDIF.

  ENDIF.

  IF gt_details IS INITIAL.
*    commented on 25.09.19
*    PERFORM confirmations_maintain.
  ENDIF.

  LOOP AT gt_items_mod_rows INTO lwa_mod_row.

    lv_tabix = sy-tabix.
    READ TABLE gt_items_layout ASSIGNING <lwa_item_layout>
                                  INDEX lwa_mod_row-row_id.
    CHECK <lwa_item_layout> IS ASSIGNED.

    PERFORM item_data_check USING <lwa_item_layout>
                                  lwa_mod_row
                                  lv_subrc.

    IF lv_subrc NE 0.
      gs_variables-errors = c_true.
      CONTINUE.
    ENDIF.

*****22/09/2016
*    IF <lwa_item_layout>-status IS INITIAL.
*      <lwa_item_layout>-status = c_process_status-ctd.
*    ENDIF.
*****

    PERFORM item_details_get USING lwa_mod_row-fieldname
                             CHANGING <lwa_item_layout>.

    PERFORM item_styles_prepare CHANGING <lwa_item_layout>.

    PERFORM catalog_line_style_change CHANGING <lwa_item_layout>.

    PERFORM description_get CHANGING <lwa_item_layout>.

    MOVE-CORRESPONDING <lwa_item_layout> TO lwa_acitm.

    IF <lwa_item_layout>-tplnr IS NOT INITIAL AND
       <lwa_item_layout>-aufnr IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <lwa_item_layout>-tplnr
        IMPORTING
          output = lv_tplnr_fl.

      LOOP AT gt_details INTO DATA(lwa_details) WHERE tplnr_fl = lv_tplnr_fl
                                                  AND matnr    = <lwa_item_layout>-tmatnr.
        MOVE lwa_details-aufnr TO lwa_orders-aufnr.
        APPEND lwa_orders TO lt_orders.
      ENDLOOP.
      DESCRIBE TABLE lt_orders LINES DATA(lv_ord_lines).
      IF lv_ord_lines = 1.
        <lwa_item_layout>-aufnr = lt_orders[ 1 ]-aufnr.
      ENDIF.

    ENDIF.
****21/09/2016
    READ TABLE gt_items_layout INTO lwa_item_layout INDEX lwa_mod_row-row_id.

    IF sy-subrc EQ 0.

      READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                                         WITH KEY posnr = lwa_item_layout-posnr.
      IF sy-subrc = 0.
        IF lwa_acitm NE <lwa_acitm>.
          MOVE lwa_acitm TO <lwa_acitm>.
          IF <lwa_acitm>-updkz NE c_updkz_new.
            gs_variables-document_changed = c_true.
            <lwa_acitm>-updkz = c_updkz_update.
          ENDIF.
        ENDIF.
      ELSE.
        gs_variables-refresh_items_grid = c_true.
        gs_variables-document_changed   = c_true.
        <lwa_item_layout>-accom         = gs_acdoc_infocus-accom.
        <lwa_item_layout>-updkz         = c_updkz_new.
        MOVE-CORRESPONDING <lwa_item_layout> TO lwa_acitm.
        APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
      ENDIF.

    ENDIF.

*    READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
*                                        INDEX lwa_mod_row-row_id.
*    IF sy-subrc EQ 0.
*      IF lwa_acitm NE <lwa_acitm>.
*        MOVE lwa_acitm TO <lwa_acitm>.
*        IF <lwa_acitm>-updkz NE c_updkz_new.
*          gs_variables-document_changed = c_true.
*          <lwa_acitm>-updkz = c_updkz_update.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      gs_variables-refresh_items_grid = c_true.
*      gs_variables-document_changed   = c_true.
*      <lwa_item_layout>-accom         = gs_acdoc_infocus-accom.
*      <lwa_item_layout>-updkz         = c_updkz_new.
*      MOVE-CORRESPONDING <lwa_item_layout> TO lwa_acitm.
*      APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
*    ENDIF.
****

  ENDLOOP.

  DELETE gs_acdoc_infocus-x-acitm WHERE status IS INITIAL.
  DELETE gt_items_layout WHERE status IS INITIAL.

*  issue enable disable machinery or resource depending of workcenter


  REFRESH gt_items_mod_rows.

  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.

    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_items_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_items_grid.
      LOOP AT gt_items_layout ASSIGNING <lwa_item_layout>.
        CHECK <lwa_item_layout> IS NOT INITIAL.
        PERFORM item_details_get USING space CHANGING <lwa_item_layout>.
      ENDLOOP.
      PERFORM refresh_items_grid.
    ENDIF.

  ENDIF.
ENDFORM.                    " ITEMS_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  ITEM_DETAILS_GET
*&---------------------------------------------------------------------*
FORM item_details_get USING lv_fieldname
                      CHANGING lwa_item TYPE /agri/s_fmacitm_layout.

  DATA: lwa_wo            TYPE /agri/s_fmac_wo,
        lwa_ord           TYPE /agri/s_fmac_ord,
        lt_wo             TYPE /agri/t_fmac_wo,
        lt_ord            TYPE /agri/t_fmac_ord,
        lt_fp             TYPE /agri/t_fmac_fp,
        lwa_resource      TYPE /agri/fmacres,
        lwa_resource_aux  TYPE /agri/fmacres,
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
        lwa_resource_tmp  TYPE /agri/s_fmac_src_res,
        lt_vgwts          TYPE /agri/t_vgwts,
        lwa_vgwts         TYPE /agri/s_vgwts,
        ls_vgwts          TYPE /agri/s_vgwts,
        lt_parameters     TYPE /agri/t_parameters_tc21,
        lwa_parameters    TYPE /agri/s_parameters_tc21,
        ls_parameters     TYPE /agri/s_parameters_tc21,
        lt_tc20           TYPE /agri/t_parameters_tc20,
        lwa_tc20          TYPE tc20,
        lwa_fmachdr       TYPE /agri/s_fmachdr,
        lt_fmacrsc        TYPE /agri/t_fmacrsc,
        ls_fmacrsc        TYPE /agri/s_fmacrsc,
        lwa_details       TYPE ty_details,
        lv_duration       TYPE sytabix.

  FIELD-SYMBOLS: <lwa_fmacitm>  TYPE /agri/s_fmacitm_layout,
                 <lwa_activity> TYPE /agri/s_fmac_src_act.

  CASE lv_fieldname.
    WHEN 'TPLNR'.
      IF ( lwa_item-tplnr IS NOT INITIAL ).
        SELECT t1~wonum t1~wotyp t1~cmnum t1~matnr t1~stort
              t2~aufnr t2~tplnr_fl t2~contr
         INTO TABLE lt_wo
         FROM /agri/fmwochdr AS t1
          INNER JOIN /agri/fmwoitm AS t2
         ON t1~wonum = t2~wonum
         WHERE t2~tplnr_fl = lwa_item-tplnr.
        READ TABLE lt_wo INTO lwa_wo
                  WITH KEY wonum = /agri/s_fmachdr-wonum.
        IF sy-subrc EQ 0.
          lwa_item-tmatnr = lwa_wo-matnr.
*          lwa_item-aufnr  = lwa_wo-aufnr.
          IF lwa_item-aufnr IS NOT INITIAL.
            PERFORM yield_get USING lwa_item-aufnr
                                    lwa_item-tmatnr
                              CHANGING lwa_item-yield.
          ENDIF.
        ELSE.
          CHECK /agri/s_fmachdr-wonum IS NOT INITIAL.
***Extended Additional Syntax Check ATC  1709 PQ
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
             NUMBER '006'
             WITH gs_acdoc_infocus-x-achdr-wonum
                  lwa_item-tplnr "lwa_aufnr
                  INTO sy-msgli. "#EC*
*****
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR: lwa_item-tmatnr,
                 lwa_item-aufnr ,
                 lwa_item-tplnr.
        ENDIF.
      ENDIF.
    WHEN 'TMATNR'.
      IF lwa_item-tplnr IS INITIAL
        AND lwa_item-tmatnr IS NOT INITIAL.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '010'
                                INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
      ENDIF.
      SELECT t1~wonum t1~wotyp t1~cmnum t1~matnr t1~stort
            t2~aufnr t2~tplnr_fl t2~contr
       INTO TABLE lt_wo
       FROM /agri/fmwochdr AS t1
        INNER JOIN /agri/fmwoitm AS t2
       ON t1~wonum = t2~wonum
       WHERE t2~tplnr_fl = lwa_item-tplnr.
      CHECK sy-subrc EQ 0.
      IF /agri/s_fmachdr-wonum IS NOT INITIAL.
        READ TABLE lt_wo INTO lwa_wo
                        WITH KEY wonum    = /agri/s_fmachdr-wonum
                                 tplnr_fl = lwa_item-tplnr
                                 matnr    = lwa_item-tmatnr.
        IF sy-subrc NE 0.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                    NUMBER '013'
                    WITH lwa_item-tmatnr
                         lwa_item-tplnr
                         INTO sy-msgli.               "#EC CI_FLDEXT_OK
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-tmatnr.
        ENDIF.
      ELSE.
        SELECT aufnr tplnr_fl cmnum varia cpros matnr
          FROM /agri/fmfphdr
          INTO TABLE lt_fp
          FOR ALL ENTRIES IN lt_wo
          WHERE aufnr NE lt_wo-aufnr
            AND tplnr_fl EQ lwa_item-tplnr
            AND matnr    EQ lwa_item-tmatnr.
        CHECK sy-subrc EQ 0.
        SORT lt_fp BY aufnr DESCENDING.
        READ TABLE lt_fp INTO lwa_fp
                            INDEX 1.
        CHECK sy-subrc EQ 0.
*        lwa_item-aufnr = lwa_fp-aufnr.
      ENDIF.

      IF lwa_item-tmatnr IS NOT INITIAL.
        SELECT SINGLE meins INTO lwa_item-ymein
          FROM mara WHERE matnr = lwa_item-tmatnr.
      ENDIF.
    WHEN 'IDRESOURCE'.
      CHECK lwa_item-idresource IS NOT INITIAL.

      IF lwa_item-aufnr IS INITIAL.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '051'
                INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
        CLEAR lwa_item-idresource.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM /agri/fmacres INTO lwa_resource
       WHERE idresource = lwa_item-idresource           "#EC CI_NOORDER
       AND rstype = c_rstype-labor.

      IF sy-subrc NE 0.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '048'
                WITH lwa_item-idresource INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
        CLEAR lwa_item-idresource.
        EXIT.
      ELSE.
**** 13/09/2016

        SELECT arbpl vgwts
          FROM crhd
          INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
          WHERE arbpl = lwa_resource-arbpl.

        DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_vgwts IS NOT INITIAL.
          SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
          INTO CORRESPONDING FIELDS OF TABLE lt_parameters
          FROM tc21
          FOR ALL ENTRIES IN lt_vgwts
          WHERE vgwts = lt_vgwts-vgwts.
        ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_parameters IS NOT INITIAL.
          SELECT parid                        "#EC CI_FAE_LINES_ENSURED
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
*        READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_tc20 IS NOT INITIAL.
          SELECT actyp parid rstyp txtlg      "#EC CI_FAE_LINES_ENSURED
          FROM /agri/tfmacrsc
          INTO TABLE lt_fmacrsc
          FOR ALL ENTRIES IN lt_tc20
          WHERE parid = lt_tc20-parid
*          AND actyp = lwa_fmachdr-actyp.
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
                                                          AND rstyp EQ c_accom_id-employee.
              ENDLOOP.

              IF sy-subrc NE 0.
                MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '046'
                WITH lwa_item-idresource
                     lwa_item-aufnr
              INTO sy-msgli.
                gs_variables-errors = c_true.
                message_simple space.
                CLEAR lwa_item-idresource.
                EXIT.                                   "#EC CI_NOORDER
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '048'
                WITH lwa_item-idresource INTO sy-msgli.
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-idresource.
          EXIT.
        ENDIF.
****
      ENDIF.

      IF lwa_item-lifnr IS NOT INITIAL.
        SELECT SINGLE name1 INTO lwa_item-name1 FROM lfa1
          WHERE lifnr = lwa_item-lifnr.
      ENDIF.

      IF lwa_item-aufnr IS NOT INITIAL.
        PERFORM wonum_infocus_read
                            USING    /agri/s_fmachdr-wonum
                                     lwa_item-aufnr
                            CHANGING lt_arbpl.
        IF lt_arbpl[] IS NOT INITIAL.
          SELECT idresource description arbpl intext lifnr
             INTO TABLE lt_resource
             FROM /agri/fmacres
             WHERE rstype = c_rstype-labor
             AND arbpl IN lt_arbpl.
          IF lt_resource IS INITIAL.
            MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '047'
                WITH lwa_item-aufnr
                     INTO sy-msgli.
            gs_variables-errors = c_true.
            message_simple space.
            CLEAR lwa_item-idresource.
            EXIT.
          ELSE.
            IF lwa_item-idresource IS NOT INITIAL.

              READ TABLE lt_resource
                        TRANSPORTING NO FIELDS
                        WITH KEY idresource = lwa_item-idresource.
            ELSE.
              EXIT.
            ENDIF.
            IF sy-subrc NE 0.
              MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '046'
                WITH lwa_item-idresource
                     lwa_item-aufnr
              INTO sy-msgli.
              gs_variables-errors = c_true.
              message_simple space.
              CLEAR lwa_item-idresource.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
              NUMBER '047'
              WITH lwa_item-aufnr
                   INTO sy-msgli.
          gs_variables-errors = c_true.
          message_simple space.
          CLEAR lwa_item-idresource.
          EXIT.
        ENDIF.
      ENDIF.
      lwa_item-idresource  = lwa_resource-idresource.
      READ TABLE lt_resource INTO lwa_resource_tmp
                               WITH KEY idresource = lwa_item-idresource.
      IF sy-subrc EQ 0.
        lwa_item-arbpl       = lwa_resource_tmp-arbpl.
      ENDIF.
*      lwa_item-arbpl       = lwa_resource-arbpl.
      lwa_item-intext      = lwa_resource-intext.
      lwa_item-lrdesa      = lwa_resource-description.
      lwa_item-lifnr       = lwa_resource-lifnr.
      lwa_item-intext      = lwa_resource-intext.

      IF  lwa_item-idactvl IS NOT INITIAL.
        CLEAR: lwa_item-idactvl, lwa_item-empdes.
      ENDIF.

**** 16/09/2016
      IF lwa_item-equnr IS NOT INITIAL.
        SELECT SINGLE * FROM /agri/fmacres
        INTO lwa_resource
        WHERE idresource EQ lwa_item-equnr
          AND rstype     EQ c_rstype-equnr
          AND arbpl      EQ lwa_item-arbpl.
        IF sy-subrc NE 0.
          MESSAGE ID '/AGRI/FMAC'
                  TYPE c_msg_type-error
                  NUMBER '031'
                  WITH lwa_item-equnr
                  INTO sy-msgli.
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-equnr.
          EXIT.
        ELSE.

          SELECT arbpl vgwts
            FROM crhd
            INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
            WHERE arbpl = lwa_resource-arbpl.

          DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_vgwts IS NOT INITIAL.
            SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
            INTO CORRESPONDING FIELDS OF TABLE lt_parameters
            FROM tc21
            FOR ALL ENTRIES IN lt_vgwts
            WHERE vgwts = lt_vgwts-vgwts.
          ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_parameters IS NOT INITIAL.
            SELECT parid                      "#EC CI_FAE_LINES_ENSURED
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
*          READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_tc20 IS NOT INITIAL.
            SELECT actyp parid rstyp txtlg    "#EC CI_FAE_LINES_ENSURED
            FROM /agri/tfmacrsc
            INTO TABLE lt_fmacrsc
            FOR ALL ENTRIES IN lt_tc20
            WHERE parid = lt_tc20-parid
*            AND actyp = lwa_fmachdr-actyp.
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
                  NUMBER '031'
                  WITH lwa_item-equnr
                  INTO sy-msgli.
                  message_simple space.
                  gs_variables-errors = c_true.
                  CLEAR lwa_item-equnr.
                  EXIT.                                 "#EC CI_NOORDER
                ENDIF.
              ENDIF.
            ENDLOOP.
          ELSE.
            MESSAGE ID '/AGRI/FMAC'
                  TYPE c_msg_type-error
                  NUMBER '031'
                  WITH lwa_item-equnr
                  INTO sy-msgli.
            message_simple space.
            gs_variables-errors = c_true.
            CLEAR lwa_item-equnr.
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.
****

    WHEN 'EQUNR'.
      CHECK lwa_item-equnr IS NOT INITIAL.
      IF lwa_item-aufnr IS INITIAL.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '052'
                INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
        CLEAR lwa_item-equnr.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM /agri/fmacres
        INTO lwa_resource
        WHERE idresource EQ lwa_item-equnr              "#EC CI_NOORDER
          AND rstype     EQ c_rstype-equnr.

      IF sy-subrc NE 0.
        MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '031'
                WITH lwa_item-equnr
                INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
        CLEAR lwa_item-equnr.
        EXIT.
      ELSE.
**** 13/09/2016

        SELECT arbpl vgwts
          FROM crhd
          INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
*          FOR ALL ENTRIES IN lt_resource
          WHERE arbpl = lwa_resource-arbpl.

        DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_vgwts IS NOT INITIAL.
          SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
          INTO CORRESPONDING FIELDS OF TABLE lt_parameters
          FROM tc21
          FOR ALL ENTRIES IN lt_vgwts
          WHERE vgwts = lt_vgwts-vgwts.
        ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_parameters IS NOT INITIAL.
          SELECT parid                        "#EC CI_FAE_LINES_ENSURED
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
*        READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
        IF lt_tc20 IS NOT INITIAL.
          SELECT actyp parid rstyp txtlg      "#EC CI_FAE_LINES_ENSURED
          FROM /agri/tfmacrsc
          INTO TABLE lt_fmacrsc
          FOR ALL ENTRIES IN lt_tc20
          WHERE parid = lt_tc20-parid
*          AND actyp = lwa_fmachdr-actyp.
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
                WITH lwa_item-equnr
                     lwa_item-aufnr
              INTO sy-msgli.
                gs_variables-errors = c_true.
                message_simple space.
                CLEAR lwa_item-equnr.
                EXIT.                                   "#EC CI_NOORDER
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '031'
                WITH lwa_item-equnr
                INTO sy-msgli.
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-equnr.
          EXIT.
        ENDIF.

      ENDIF.
****
      IF lwa_item-lifnr IS NOT INITIAL.
        SELECT SINGLE name1 INTO lwa_item-name1 FROM lfa1
          WHERE lifnr = lwa_item-lifnr.
      ENDIF.

      IF lwa_item-aufnr IS NOT INITIAL.
        PERFORM wonum_infocus_read
                            USING    /agri/s_fmachdr-wonum
                                     lwa_item-aufnr
                            CHANGING lt_arbpl.
        IF lt_arbpl[] IS NOT INITIAL.
          SELECT idresource description arbpl intext lifnr
             INTO TABLE lt_resource
             FROM /agri/fmacres
             WHERE rstype = c_rstype-equnr
             AND arbpl IN lt_arbpl.
          IF lt_resource IS INITIAL.
            MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                NUMBER '047'
                WITH lwa_item-aufnr
                     INTO sy-msgli.
            gs_variables-errors = c_true.
            message_simple space.
            CLEAR lwa_item-idresource.
            EXIT.
          ELSE.
            IF lwa_item-equnr IS NOT INITIAL.

              READ TABLE lt_resource
                        TRANSPORTING NO FIELDS
                        WITH KEY idresource = lwa_item-equnr.
            ELSE.
              EXIT.
            ENDIF.

            IF sy-subrc NE 0.
              MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '049'
                WITH lwa_item-equnr
                     lwa_item-aufnr
              INTO sy-msgli.
              gs_variables-errors = c_true.
              message_simple space.
              CLEAR lwa_item-equnr.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
              NUMBER '047'
              WITH lwa_item-aufnr
                   INTO sy-msgli.
          gs_variables-errors = c_true.
          message_simple space.
          CLEAR lwa_item-idresource.
          EXIT.
        ENDIF.
      ENDIF.
      lwa_item-erdesa = lwa_resource-description.
      lwa_item-arbpl  = lwa_resource-arbpl.

      IF  lwa_item-idactve IS NOT INITIAL.
        CLEAR: lwa_item-idactve, lwa_item-eqmdes.
      ENDIF.

      IF lwa_item-idresource IS NOT INITIAL.

        SELECT SINGLE * FROM /agri/fmacres INTO lwa_resource
        WHERE idresource = lwa_item-idresource
        AND rstype = c_rstype-labor
        AND arbpl      EQ lwa_item-arbpl.

        IF sy-subrc NE 0.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                  NUMBER '048'
                  WITH lwa_item-idresource INTO sy-msgli.
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-idresource.
          EXIT.
        ELSE.
**** 13/09/2016
          SELECT arbpl vgwts
            FROM crhd
            INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
            WHERE arbpl = lwa_resource-arbpl.

          DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_vgwts IS NOT INITIAL.
            SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
            INTO CORRESPONDING FIELDS OF TABLE lt_parameters
            FROM tc21
            FOR ALL ENTRIES IN lt_vgwts
            WHERE vgwts = lt_vgwts-vgwts.
          ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_parameters IS NOT INITIAL.
            SELECT parid                      "#EC CI_FAE_LINES_ENSURED
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
*          READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
          IF lt_tc20 IS NOT INITIAL.
            SELECT actyp parid rstyp txtlg    "#EC CI_FAE_LINES_ENSURED
            FROM /agri/tfmacrsc
            INTO TABLE lt_fmacrsc
            FOR ALL ENTRIES IN lt_tc20
            WHERE parid = lt_tc20-parid
*            AND actyp = lwa_fmachdr-actyp.
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
                                                           AND rstyp EQ c_accom_id-employee.
                ENDLOOP.

                IF sy-subrc NE 0.
                  MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                  NUMBER '048'
                  WITH lwa_item-idresource INTO sy-msgli.
                  message_simple space.
                  gs_variables-errors = c_true.
                  CLEAR lwa_item-idresource.
                  EXIT.                                 "#EC CI_NOORDER
                ENDIF.
              ENDIF.
            ENDLOOP.
          ELSE.
            MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                  NUMBER '048'
                  WITH lwa_item-idresource INTO sy-msgli.
            message_simple space.
            gs_variables-errors = c_true.
            CLEAR lwa_item-idresource.
            EXIT.
          ENDIF.
****
        ENDIF.
      ENDIF.

    WHEN 'STRTDAT' OR 'STRTTIM' OR 'FINDAT' OR 'FINTIM'.
      IF lwa_item-strtdat IS NOT INITIAL
       AND lwa_item-findat IS NOT INITIAL.
*--- Replaced Unreleased Interface
*        CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
*          EXPORTING
*            t1      = lwa_item-strttim
*            t2      = lwa_item-fintim
*            d1      = lwa_item-strtdat
*            d2      = lwa_item-findat
*          IMPORTING
*            minutes = lv_minutes.
*        CALL FUNCTION '/AGRI/G_DELTA_TIME_DAY_HOUR'
*          EXPORTING
*            i_t1      = lwa_item-strttim
*            i_t2      = lwa_item-fintim
*            i_d1      = lwa_item-strtdat
*            i_d2      = lwa_item-findat
*          IMPORTING
*            e_minutes = lv_minutes.
*---

        CALL FUNCTION 'SWI_DURATION_DETERMINE'
          EXPORTING
            start_date       = lwa_item-strtdat
            end_date         = lwa_item-findat
            start_time       = lwa_item-strttim
            end_time         = lwa_item-fintim
         IMPORTING
           DURATION         = lv_duration.

*        lwa_item-duran = lv_minutes / 60.
        lwa_item-duran = lv_duration / 3600.
        lwa_item-meins = 'STD'.

        CHECK lwa_item-qmein IS NOT INITIAL
            AND lwa_item-meins IS NOT INITIAL
            AND lwa_item-duran IS NOT INITIAL.

        IF lwa_item-qmein EQ 'S'
          OR lwa_item-qmein EQ 'MIN'
          OR lwa_item-qmein EQ 'H'.

          PERFORM time_convert USING    lwa_item-qmein
                                        lwa_item-meins
                                        lwa_item-duran
                               CHANGING lv_auszt.
          IF lv_auszt IS NOT INITIAL.
            lwa_item-menge = lv_auszt.
          ENDIF.
        ELSE.
          IF lwa_item-menge IS NOT INITIAL.
            CLEAR lwa_item-menge.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'AUFNR'.
      CHECK lwa_item-aufnr IS NOT INITIAL.
      CLEAR: lwa_item-tplnr, lwa_item-tmatnr.
      APPEND lwa_item-aufnr TO lt_aufnr.
      PERFORM aufnr_infocus_read  USING lt_aufnr
                                  CHANGING lt_fpdoc.

      IF lt_fpdoc IS INITIAL.
        READ TABLE lt_aufnr INTO ls_aufnr INDEX 1.
        READ TABLE gt_items_layout ASSIGNING <lwa_fmacitm>
                          WITH KEY aufnr = ls_aufnr-aufnr.
        CLEAR <lwa_fmacitm>-aufnr.
        CALL METHOD ref_grid_items->refresh_table_display.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '023'
                                WITH ls_aufnr-aufnr
                                INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
      ENDIF.

      IF /agri/s_fmachdr-wonum IS NOT INITIAL.
        APPEND /agri/s_fmachdr-wonum TO lt_wonum.

        CALL FUNCTION '/AGRI/FMWO_VIEW'
          EXPORTING
            it_wonum       = lt_wonum
          IMPORTING
            et_wodoc       = lt_fmwo_doc
          EXCEPTIONS ##FM_SUBRC_OK
            no_data_exists = 1
            OTHERS         = 2.
        READ TABLE lt_fmwo_doc
                INTO lwa_wodoc_infocus INDEX 1.
        DELETE lwa_wodoc_infocus-x-woitm
            WHERE aufnr NE lwa_item-aufnr.
        IF lwa_wodoc_infocus-x-woitm[] IS INITIAL.
          MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '024'
                WITH lwa_aufnr gs_acdoc_infocus-x-achdr-wonum
                INTO sy-msgli.
          message_simple space.
          gs_variables-errors = c_true.
          CLEAR lwa_item-aufnr.
          EXIT.
        ENDIF.
      ENDIF.

      READ TABLE lt_fpdoc INTO lwa_fpdoc INDEX 1.
      lwa_item-tplnr  = lwa_fpdoc-x-fphdr-tplnr_fl.
      lwa_item-tmatnr = lwa_fpdoc-x-fphdr-matnr.
      PERFORM yield_get USING lwa_item-aufnr
                              lwa_item-tmatnr
                        CHANGING lwa_item-yield.

      IF lwa_item-yield IS INITIAL.
        LOOP AT gt_details INTO  lwa_details WHERE aufnr = lwa_item-aufnr.
          lwa_item-yield = lwa_item-yield + lwa_details-menge.
        ENDLOOP.
      ENDIF.

      IF lwa_item-tmatnr IS NOT INITIAL.
        SELECT SINGLE meins INTO lwa_item-ymein
          FROM mara WHERE matnr = lwa_item-tmatnr.
      ENDIF.

    WHEN 'IDACTVL'.
      CHECK lwa_item-idactvl IS NOT INITIAL.
      PERFORM activity_check USING c_rstype-labor

                          CHANGING lwa_item
                                   lt_activity
                                   lt_fmacrsc.
      READ TABLE lt_activity ASSIGNING <lwa_activity> WITH KEY idactv = lwa_item-idactvl
                                                                rstype = c_rstype-labor.
      IF sy-subrc EQ 0.

        PERFORM activity_mein_change  USING
                                            lt_fmacrsc
                                      CHANGING lwa_item
                                         <lwa_activity>.
      ELSE.

        MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '063'
                WITH lwa_item-idactvl
                     lwa_item-aufnr
              INTO sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
        CLEAR lwa_item-idactvl.
        EXIT.

      ENDIF.


    WHEN 'IDACTVE'.
      CHECK lwa_item-idactve IS NOT INITIAL.
      PERFORM activity_check USING c_rstype-equnr

                          CHANGING lwa_item
                                   lt_activity
                                   lt_fmacrsc.
      READ TABLE lt_activity ASSIGNING <lwa_activity> WITH KEY idactv = lwa_item-idactve
                                                               rstype = c_rstype-equnr.
      IF sy-subrc EQ 0.

        PERFORM activity_mein_change  USING
                                            lt_fmacrsc
                                      CHANGING lwa_item
                                               <lwa_activity>.

      ELSE.

        MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '062'
                WITH lwa_item-idactve
                     lwa_item-aufnr
              INTO sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
        CLEAR lwa_item-idactve.
        EXIT.
      ENDIF.

    WHEN space.
      CLEAR lwa_resource.
      SELECT SINGLE * FROM /agri/fmacres INTO lwa_resource
        WHERE idresource = lwa_item-idresource          "#EC CI_NOORDER
        AND rstype = c_rstype-labor.
      lwa_item-lrdesa      = lwa_resource-description.
      IF lwa_item-lifnr IS NOT INITIAL.
        SELECT SINGLE name1 INTO lwa_item-name1 FROM lfa1
          WHERE lifnr = lwa_item-lifnr.
      ENDIF.
      IF lwa_item-equnr IS NOT INITIAL.
        CLEAR lwa_resource.
        SELECT SINGLE * FROM /agri/fmacres INTO lwa_resource
          WHERE idresource = lwa_item-equnr             "#EC CI_NOORDER
          AND rstype = c_rstype-equnr.
        lwa_item-erdesa  = lwa_resource-description.
      ENDIF.
  ENDCASE.
ENDFORM.                    " ITEM_DETAILS_GET
*&---------------------------------------------------------------------*
*&      Form  ITEM_STYLES_PREPARE
*&---------------------------------------------------------------------*
FORM item_styles_prepare CHANGING lwa_item TYPE /agri/s_fmacitm_layout.
  DATA: lwa_style TYPE lvc_s_styl,
        lt_fcat   TYPE lvc_t_fcat.
  FIELD-SYMBOLS:  <lwa_fcat>  TYPE lvc_s_fcat.

  REFRESH: lwa_item-styles.
  IF ref_grid_items IS NOT INITIAL.

    IF lwa_item-status EQ c_process_status-ctd.
      READ TABLE gt_fcat ASSIGNING <lwa_fcat>
        WITH KEY fieldname = 'ZZCONFTYP'.
      IF sy-subrc EQ 0.
         lwa_style-fieldname = <lwa_fcat>-fieldname.
         lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
         INSERT lwa_style INTO TABLE lwa_item-styles.
      ENDIF.

      IF lwa_item-idresource IS INITIAL.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>
              WHERE fieldname = 'IDACTVL'
                 OR fieldname = 'IDRESOURCE'.
          CLEAR lwa_item-idactvl.
          lwa_style-fieldname = <lwa_fcat>-fieldname.
          lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lwa_style INTO TABLE lwa_item-styles.
        ENDLOOP.
      ELSE.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>
            WHERE fieldname = 'IDRESOURCE'.
          lwa_style-fieldname = <lwa_fcat>-fieldname.
          lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT lwa_style INTO TABLE lwa_item-styles.
        ENDLOOP.
      ENDIF.
      IF lwa_item-equnr IS INITIAL..
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>
                      WHERE fieldname = 'IDACTVE'
                         OR fieldname = 'EQUNR'.
          CLEAR lwa_item-idactve.
          lwa_style-fieldname = <lwa_fcat>-fieldname.
          lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lwa_style INTO TABLE lwa_item-styles.
        ENDLOOP.
      ELSE.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>
            WHERE fieldname = 'EQUNR'.
          lwa_style-fieldname = <lwa_fcat>-fieldname.
          lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT lwa_style INTO TABLE lwa_item-styles.
        ENDLOOP.
      ENDIF.
    ENDIF.

*    IF lwa_item-status EQ c_process_status-ctd
*       AND lwa_item-equnr IS INITIAL.
*      LOOP AT gt_fcat ASSIGNING <lwa_fcat>
*                      WHERE fieldname = 'IDACTVE'
*                         OR fieldname = 'EQUNR'.
*        CLEAR lwa_item-idactve.
*        lwa_style-fieldname = <lwa_fcat>-fieldname.
*        lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT lwa_style INTO TABLE lwa_item-styles.
*      ENDLOOP.
*
*    ENDIF.


    IF lwa_item-status NE c_process_status-ctd.
*      CALL METHOD ref_grid_items->get_frontend_fieldcatalog
*        IMPORTING
*          et_fieldcatalog = lt_fcat.
*      LOOP AT lt_fcat ASSIGNING <lwa_fcat> WHERE edit = c_true.
      LOOP AT gt_fcat ASSIGNING <lwa_fcat> WHERE edit = c_true.
        lwa_style-fieldname = <lwa_fcat>-fieldname.
        lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT lwa_style INTO TABLE lwa_item-styles.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " ITEM_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ITEM_DATA_CHECK
*&---------------------------------------------------------------------*
FORM item_data_check USING lwa_items_layout TYPE /agri/s_fmacitm_layout
                           lwa_mod_row      TYPE lvc_s_modi
                           lv_subrc.

  TYPES: BEGIN OF ty_shift.
      INCLUDE TYPE zfmacwork_shift.
  TYPES: stime TYPE tzonref-tstamps,
         etime TYPE tzonref-tstamps,
         stdur TYPE zabs_del_stdur,
         END OF ty_shift.

  DATA: lt_fmacact  TYPE TABLE OF /agri/fmacact,
        lt_fmacitm  TYPE /agri/t_fmacitm,
        lwa_fmacact TYPE /agri/fmacact,
        lv_tplnr    TYPE /agri/gltplnr_fl,
        lv_count    TYPE i,
*        lt_dayint   TYPE STANDARD TABLE OF pwsdayint,
*        lt_interval TYPE STANDARD TABLE OF ptws_time_duration,
        lv_stime    TYPE tzonref-tstamps,
        lv_etime    TYPE tzonref-tstamps,
        lt_fmacwork TYPE STANDARD TABLE OF ty_shift,
        lv_date     TYPE datum.

  FIELD-SYMBOLS: <lwa_fmacitml> TYPE /agri/s_fmacitm_layout,
                 <lwa_fmacact>  TYPE /agri/fmacact.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
    EXPORTING
      input  = lwa_items_layout-zztplnr
    IMPORTING
      output = lv_tplnr.

  CLEAR lv_subrc.
  CASE lwa_mod_row-fieldname.
    WHEN 'IDACTVL'.
      CHECK lwa_items_layout-idactvl IS NOT INITIAL.
      IF lt_fmacact IS INITIAL.
        SELECT * FROM /agri/fmacact                     "#EC CI_NOWHERE
          INTO TABLE lt_fmacact.
        SORT lt_fmacact BY idactv.
      ENDIF.
      READ TABLE lt_fmacact ASSIGNING <lwa_fmacact>
                            WITH KEY idactv = lwa_items_layout-idactvl
                            BINARY SEARCH.
      IF <lwa_fmacact> IS ASSIGNED.
        lwa_fmacact = <lwa_fmacact>.
      ENDIF.
      IF sy-subrc NE 0.
        MESSAGE ID c_transac
            TYPE c_msg_type-error NUMBER '029'
            INTO sy-msgli
            WITH lwa_items_layout-idactvl.
        message_simple space.
        CLEAR lwa_items_layout-idactvl.
      ELSEIF lwa_items_layout-idactve IS NOT INITIAL.
        READ TABLE lt_fmacact ASSIGNING <lwa_fmacact>
                          WITH KEY idactv = lwa_items_layout-idactve
                          BINARY SEARCH.
        IF <lwa_fmacact>-bill NE lwa_fmacact-bill.
          MESSAGE ID c_transac TYPE c_msg_type-error NUMBER '043'
                                                   INTO sy-msgli.
          CLEAR lwa_items_layout-idactvl.
          message_simple space.
        ENDIF.
      ENDIF.

      CHECK lwa_items_layout-idactvl IS NOT INITIAL.

      SELECT SINGLE bill
        FROM /agri/fmacact
        INTO lwa_items_layout-zzbill
       WHERE idactv EQ lwa_items_layout-idactvl.
      IF sy-subrc EQ 0 AND lwa_items_layout-zzbill NE 'NO'.
        CLEAR: lwa_items_layout-zzactrn,
               lwa_items_layout-zzacppg,
               lwa_items_layout-zzstdur.
      ENDIF.

    WHEN 'IDACTVE'.
      CHECK lwa_items_layout-idactve IS NOT INITIAL.
      IF lt_fmacact IS INITIAL.
        SELECT * FROM /agri/fmacact                     "#EC CI_NOWHERE
          INTO TABLE lt_fmacact.
        SORT lt_fmacact BY idactv.
      ENDIF.
      READ TABLE lt_fmacact ASSIGNING <lwa_fmacact>
                     WITH KEY idactv = lwa_items_layout-idactve
                     BINARY SEARCH.
      IF <lwa_fmacact> IS ASSIGNED.
        lwa_fmacact = <lwa_fmacact>.
      ENDIF.
      IF sy-subrc NE 0.
        MESSAGE ID c_transac TYPE c_msg_type-error
                          NUMBER '030' INTO sy-msgli
                          WITH lwa_mod_row-value.
        message_simple space.
        CLEAR lwa_items_layout-idactve.
      ELSEIF lwa_items_layout-idactvl IS NOT INITIAL.
        READ TABLE lt_fmacact ASSIGNING <lwa_fmacact>
                          WITH KEY idactv = lwa_items_layout-idactvl
                          BINARY SEARCH.
        IF <lwa_fmacact>-bill NE lwa_fmacact-bill.
          MESSAGE ID c_transac TYPE c_msg_type-error
                               NUMBER '044'
                               INTO sy-msgli.
          CLEAR lwa_items_layout-idactve.
          message_simple space.
        ENDIF.
      ENDIF.
    WHEN 'STRTDAT' OR 'STRTTIM' OR
         'FINDAT'  OR 'FINTIM'.

      IF lwa_mod_row-fieldname EQ 'FINDAT'
        AND lwa_items_layout-strtdat IS NOT INITIAL.
        IF lwa_items_layout-findat < lwa_items_layout-strtdat.
          lv_subrc = 4.
          MESSAGE ID c_transac TYPE c_msg_type-error
                              NUMBER '018' INTO sy-msgli
                              WITH lwa_items_layout-findat
                                   lwa_items_layout-strtdat.
          CLEAR lwa_items_layout-findat.
          message_simple space.
          EXIT.
        ENDIF.
      ELSEIF lwa_mod_row-fieldname EQ 'STRTDAT'
      AND lwa_items_layout-findat IS NOT INITIAL.
        IF lwa_items_layout-findat < lwa_items_layout-strtdat.
          lv_subrc = 4.
          MESSAGE ID c_transac TYPE c_msg_type-error
                              NUMBER '018' INTO sy-msgli
                              WITH lwa_items_layout-findat
                                   lwa_items_layout-strtdat.
          CLEAR lwa_items_layout-strtdat.
          message_simple space.
          EXIT.
        ENDIF.
      ENDIF.
      IF lwa_mod_row-fieldname EQ 'STRTTIM'
       AND lwa_items_layout-fintim IS NOT INITIAL
       AND lwa_items_layout-strtdat EQ lwa_items_layout-findat.
        IF lwa_items_layout-fintim LT lwa_items_layout-strttim.
          lv_subrc = 4.
***Extended Additional Syntax Check ATC  1709 PQ
          MESSAGE ID c_transac TYPE c_msg_type-error
                              NUMBER '050' INTO sy-msgli.
*                              WITH lwa_items_layout-findat
*                                   lwa_items_layout-strtdat. "#EC*
****
          CLEAR lwa_items_layout-strttim.
          message_simple space.
          EXIT.
        ENDIF.
      ELSEIF lwa_mod_row-fieldname EQ 'FINTIM'
       AND lwa_items_layout-strttim IS NOT INITIAL
       AND lwa_items_layout-strtdat EQ lwa_items_layout-findat.
        IF lwa_items_layout-fintim LT lwa_items_layout-strttim.
          lv_subrc = 4.
***Extended Additional Syntax Check ATC  1709 PQ
          MESSAGE ID c_transac TYPE c_msg_type-error
                              NUMBER '050' INTO sy-msgli.
*                              WITH lwa_items_layout-findat
*                                   lwa_items_layout-strtdat. "#EC*
****
          CLEAR lwa_items_layout-fintim.
          message_simple space.
          EXIT.
        ENDIF.
      ENDIF.

      IF lwa_items_layout-strtdat IS NOT INITIAL
        AND lwa_items_layout-findat IS NOT INITIAL.
        IF lwa_items_layout-strtdat < /agri/s_fmachdr-strtdat.
          lv_subrc = 4.
          MESSAGE ID c_transac TYPE c_msg_type-error
                              NUMBER '014' INTO sy-msgli
                              WITH lwa_items_layout-strtdat
                                   /agri/s_fmachdr-strtdat.
          message_simple space.
          EXIT.
        ENDIF.
        IF lwa_items_layout-findat > /agri/s_fmachdr-findat.
          lv_subrc = 4.
          MESSAGE ID c_transac TYPE c_msg_type-error
                               NUMBER '015' INTO sy-msgli
                               WITH lwa_items_layout-findat
                                    /agri/s_fmachdr-findat.
          message_simple space.
          EXIT.
        ENDIF.
      ENDIF.
      CHECK ok_code NE c_fcode-conf_reversed
        AND ok_code NE c_fcode-display_change_toggle.
      PERFORM items_time_validate USING    lwa_items_layout-idresource
                                           lwa_items_layout-strtdat
                                           lwa_items_layout-findat
                                           lwa_items_layout-equnr

                                  CHANGING lv_subrc
                                    lwa_mod_row-fieldname lwa_items_layout-strttim
                                    lwa_items_layout-fintim.
    WHEN 'AUFNR'.

      IF lwa_items_layout-tplnr IS INITIAL.
        lv_subrc = 4.
        MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                             NUMBER '079' INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.

      CHECK lwa_items_layout-aufnr IS NOT INITIAL.
      READ TABLE gt_details TRANSPORTING NO FIELDS
           WITH KEY tplnr_fl = lv_tplnr
                    matnr    = lwa_items_layout-tmatnr
                    aufnr    = lwa_items_layout-aufnr.
      IF sy-subrc NE 0.
        lv_subrc = 4.
        CLEAR: lwa_items_layout-aufnr.
*               lwa_items_layout-tmatnr,
*               lwa_items_layout-tplnr.
        MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                             NUMBER '081'
                             WITH lwa_items_layout-tplnr
                             INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.

    WHEN 'ZZTPLNR'.
      lwa_items_layout-tplnr = lwa_items_layout-zztplnr.
      CHECK lwa_items_layout-zztplnr IS NOT INITIAL.

      READ TABLE gt_details TRANSPORTING NO FIELDS
        WITH KEY tplnr_fl = lv_tplnr.
      IF sy-subrc NE 0.
        lv_subrc = 4.
        MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                             NUMBER '074' INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.

      lwa_items_layout-tplnr = lwa_items_layout-zztplnr.

*      CALL FUNCTION 'ZABS_FM_GET_COMPLEXITY'
*        EXPORTING
*          iv_tplnr   = lwa_items_layout-tplnr
*        IMPORTING
*          ev_complex = lwa_items_layout-zzcompl.

    WHEN 'ZZACTRN'.

      SELECT *
        FROM zfmacwork_shift
        INTO TABLE lt_fmacwork
        WHERE werks  EQ /agri/s_fmachdr-werks
          AND arbpl  EQ lwa_items_layout-arbpl
          AND actrn  EQ lwa_items_layout-zzactrn
          AND period EQ lwa_items_layout-strtdat(6).
      IF sy-subrc NE 0.
        lv_subrc = 4.
        MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                             NUMBER '082' INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.

      CLEAR: lv_stime, lv_etime.
      CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
        EXPORTING
          iv_sdate = lwa_items_layout-strtdat
          iv_edate = lwa_items_layout-findat
          iv_stime = lwa_items_layout-strttim
          iv_etime = lwa_items_layout-fintim
        IMPORTING
          ev_stime = lv_stime
          ev_etime = lv_etime.

      LOOP AT lt_fmacwork ASSIGNING FIELD-SYMBOL(<fs_fmacwork>).
        CLEAR lv_date.
        lv_date = lwa_items_layout-strtdat.
        IF <fs_fmacwork>-achit GT <fs_fmacwork>-achft.
          lv_date = lv_date + 1.
        ENDIF.

        CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
          EXPORTING
            iv_sdate = lwa_items_layout-strtdat
            iv_edate = lv_date
            iv_stime = <fs_fmacwork>-achit
            iv_etime = <fs_fmacwork>-achft
          IMPORTING
            ev_stime = <fs_fmacwork>-stime
            ev_etime = <fs_fmacwork>-etime
            ev_stdur = <fs_fmacwork>-stdur.
      ENDLOOP.

      LOOP AT lt_fmacwork INTO DATA(lwa_fmacwork)
                 WHERE stime LE lv_stime
                   AND etime GE lv_etime.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        lwa_items_layout-zzacppg = lwa_fmacwork-acppg.
        lwa_items_layout-zzacdtr = lwa_fmacwork-acdtr.
        lwa_items_layout-zzstdur = lwa_fmacwork-stdur.
      ELSE.
        lv_subrc = 4.
        MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                             NUMBER '082' INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.

  ENDCASE.

  IF lwa_items_layout-zzbill EQ 'NO' AND
     ( lwa_mod_row-fieldname EQ 'IDACTVL' OR
       lwa_mod_row-fieldname EQ 'STRTDAT' OR
       lwa_mod_row-fieldname EQ 'FINDAT' OR
       lwa_mod_row-fieldname EQ 'STRTTIM' OR
       lwa_mod_row-fieldname EQ 'FINTIM' ).

    REFRESH lt_fmacwork.

    SELECT *
      FROM zfmacwork_shift
      INTO TABLE lt_fmacwork
      WHERE werks  EQ /agri/s_fmachdr-werks
        AND arbpl  EQ lwa_items_layout-arbpl
        AND period EQ lwa_items_layout-strtdat(6).
    IF sy-subrc EQ 0.
      CLEAR: lv_stime, lv_etime.
      CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
        EXPORTING
          iv_sdate = lwa_items_layout-strtdat
          iv_edate = lwa_items_layout-findat
          iv_stime = lwa_items_layout-strttim
          iv_etime = lwa_items_layout-fintim
        IMPORTING
          ev_stime = lv_stime
          ev_etime = lv_etime.

      LOOP AT lt_fmacwork ASSIGNING <fs_fmacwork>.
        CLEAR lv_date.
        lv_date = lwa_items_layout-strtdat.
        IF <fs_fmacwork>-achit GT <fs_fmacwork>-achft.
          lv_date = lv_date + 1.
        ENDIF.

        CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
          EXPORTING
            iv_sdate = lwa_items_layout-strtdat
            iv_edate = lv_date
            iv_stime = <fs_fmacwork>-achit
            iv_etime = <fs_fmacwork>-achft
          IMPORTING
            ev_stime = <fs_fmacwork>-stime
            ev_etime = <fs_fmacwork>-etime
            ev_stdur = <fs_fmacwork>-stdur.
      ENDLOOP.

      CLEAR lv_count.
      LOOP AT lt_fmacwork INTO lwa_fmacwork
                 WHERE stime LE lv_stime
                   AND etime GE lv_etime.
        lv_count = lv_count + 1.
      ENDLOOP.

      IF lv_count EQ 1.
        lwa_items_layout-zzactrn = lwa_fmacwork-actrn.
        lwa_items_layout-zzacppg = lwa_fmacwork-acppg.
        lwa_items_layout-zzacdtr = lwa_fmacwork-acdtr.
        lwa_items_layout-zzstdur = lwa_fmacwork-stdur.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ITEM_DATA_CHECK

*&---------------------------------------------------------------------*
*&      Form  ITEMS_TIME_VALIDATE
*&---------------------------------------------------------------------*
FORM items_time_validate USING    lv_idresource TYPE /agri/fmidrsc
                                  lv_strtdat    TYPE /agri/glstrtdat
                                  lv_findat     TYPE /agri/glfindat
                                  lv_equnr      TYPE /agri/fm_equnr

                         CHANGING lv_subrc
                           lv_fieldname  TYPE lvc_fname  lv_strttim    TYPE /agri/glstrttim
                           lv_fintim     TYPE /agri/glfintim.

  DATA: lt_fmacitm  TYPE /agri/t_fmacitm,
        lt_fmacitm2 TYPE /agri/t_fmacitm,
        lt_daytim   TYPE /agri/t_fmac_dur_daytime,
        lv_tim      TYPE t,
        lv_tim2     TYPE t,
        lv_dattmp   TYPE d,
        lv_day      TYPE d,
        lv_timadd   TYPE c LENGTH 11,
        lv_timstr   TYPE c LENGTH 11,
        lv_timadd2  TYPE t,
        lv_timstr2  TYPE t,
        lv_target   TYPE c,
        lv_resource TYPE /agri/fmidrsc,
        lv_name     TYPE c LENGTH 9,
        lv_length   TYPE i,
        lr_budat    TYPE RANGE OF budat,
        lwa_budat   LIKE LINE OF lr_budat.

  FIELD-SYMBOLS: <lwa_fmacitml>  TYPE /agri/s_fmacitm_layout,
                 <lwa_fmacact>   TYPE /agri/fmacact,
                 <lwa_fmacitm>   TYPE /agri/s_fmacitm,
                 <lwa_daytim>    TYPE /agri/s_fmac_dur_daytime,
                 <lwa_items_tmp> TYPE /agri/s_fmacitm_layout.

  CHECK lv_idresource IS NOT INITIAL
     AND lv_strtdat   IS NOT INITIAL
     AND lv_findat    IS NOT INITIAL
      OR lv_equnr     IS NOT INITIAL.

  lwa_budat-option = c_operator_word-between.
  lwa_budat-sign   = c_sign-include.
  lwa_budat-low    = lv_strtdat.
  lwa_budat-high   = lv_findat.
  APPEND lwa_budat TO lr_budat.

  SELECT * FROM /agri/fmacitm                 "#EC CI_ALL_FIELDS_NEEDED
    INTO TABLE lt_fmacitm
    WHERE idresource EQ lv_idresource                   "#EC CI_NOORDER
       OR equnr      EQ lv_equnr
      AND strtdat    IN lr_budat
      AND findat     IN lr_budat.
  CHECK sy-subrc EQ 0.

  lwa_budat-option = c_operator_word-between.
  lwa_budat-sign   = c_sign-include.
  lwa_budat-low    = lv_strtdat.
  lwa_budat-high   = lv_findat.
  APPEND lwa_budat TO lr_budat.
  DELETE lt_fmacitm WHERE strtdat NOT IN lr_budat
                      AND findat  NOT IN lr_budat
                      OR status   NE c_process_status-cnf.
  DO 2 TIMES.
    IF lv_target IS INITIAL.
      lt_fmacitm2 = lt_fmacitm. "#EC CI_ALL_FIELDS_NEEDED   "#EC CI_NOORDER
      DELETE lt_fmacitm2 WHERE idresource IS INITIAL    "#EC CI_NOORDER
                            OR idresource
                            NE lv_idresource.
      lv_target = c_true.
      lv_name   = 'Employee'(016).
    ELSE.
      lt_fmacitm2 = lt_fmacitm. "#EC CI_ALL_FIELDS_NEEDED     "#EC CI_NOORDER
      DELETE lt_fmacitm2 WHERE equnr IS INITIAL         "#EC CI_NOORDER
                            OR equnr
                            NE lv_equnr.
      lv_name = 'Equipment'(015).
      CLEAR: lv_target, lv_tim, lv_subrc.
      REFRESH lt_daytim.
    ENDIF.
    LOOP AT lt_fmacitm2 ASSIGNING <lwa_fmacitm>. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_NOORDER
      CLEAR: lv_timadd, lv_tim2.
      IF lv_target IS NOT INITIAL.
        lv_resource = <lwa_fmacitm>-idresource. "#EC CI_ALL_FIELDS_NEEDED   "#EC CI_NOORDER
      ELSE.
        lv_resource = <lwa_fmacitm>-equnr. "#EC CI_ALL_FIELDS_NEEDED  "#EC CI_NOORDER
      ENDIF.
      CHECK <lwa_fmacitm> IS ASSIGNED.
      CALL FUNCTION '/AGRI/FMAC_CALC_HR_PR_DAY'   "#EC CI_ALL_FIELDS_NEEDED  "#EC CI_NOORDER
        EXPORTING
          i_date1          = <lwa_fmacitm>-strtdat
          i_time1          = <lwa_fmacitm>-strttim
          i_date2          = <lwa_fmacitm>-findat
          i_time2          = <lwa_fmacitm>-fintim
        IMPORTING
          e_tim_tab        = lt_daytim
*         E_DATE2_EARLY    =
        EXCEPTIONS
          invalid_datetime = 1
          days_over_limit  = 2
          OTHERS           = 3.
      IF sy-subrc EQ 0.
        READ TABLE lt_daytim ASSIGNING <lwa_daytim>
                        WITH KEY day = lv_strtdat.
        IF sy-subrc EQ 0.
          lv_day = <lwa_daytim>-day.
        ELSE.
          READ TABLE lt_daytim ASSIGNING <lwa_daytim>
                      WITH KEY day = lv_findat.
          IF sy-subrc EQ 0.
            lv_day = <lwa_daytim>-day.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <lwa_daytim>-hours IS ASSIGNED.
        WRITE <lwa_daytim>-hours TO lv_timadd.
      ENDIF.

      SHIFT lv_timadd LEFT DELETING LEADING space.
      IF lv_timadd EQ '24:00'.
        lv_timadd = '23:59'.
      ENDIF.
      lv_length = strlen( lv_timadd ).
      IF lv_length EQ 4.
        CONCATENATE '0' lv_timadd(1) lv_timadd+2(2) '00'
                                         INTO lv_timadd2.
      ELSEIF lv_length EQ 3.
        CONCATENATE '00' lv_timadd+1(2) '00'
                             INTO lv_timadd2.
      ELSE.
        CONCATENATE lv_timadd(2) lv_timadd+3(2) '00'
                                    INTO lv_timadd2.
      ENDIF.
*--- Replaced Unreleased Interface.
*        CALL FUNCTION 'DIMP_ADD_TIME'
*          EXPORTING
*            iv_starttime = lv_tim
*            iv_startdate = lv_day
*            iv_addtime   = lv_timadd2
*          IMPORTING
*            ev_endtime   = lv_tim2
*            ev_enddate   = lv_dattmp.
      CALL FUNCTION '/AGRI/G_DIMP_ADD_TIME'
        EXPORTING
          i_starttime = lv_tim
          i_startdate = lv_day
          i_addtime   = lv_timadd2
        IMPORTING
          e_endtime   = lv_tim2
          e_enddate   = lv_dattmp.
*---
      lv_tim = lv_tim2.
      IF lv_tim2 GT '235900'
        OR lv_dattmp NE lv_day.
        MESSAGE ID '/AGRI/FMAC'
                        TYPE c_msg_type-error
                        NUMBER '003'
                        WITH lv_day lv_name lv_resource
                        INTO sy-msgli.
        lv_subrc = 4.
        message_simple space.
        gs_variables-errors = c_true.
      ENDIF.
      REFRESH lt_daytim.
    ENDLOOP.
    CHECK lv_subrc EQ 0
       AND lt_fmacitm2 IS NOT INITIAL.
    CALL FUNCTION '/AGRI/FMAC_CALC_HR_PR_DAY'
      EXPORTING
        i_date1          = lv_strtdat
        i_time1          = lv_strttim
        i_date2          = lv_findat
        i_time2          = lv_fintim
      IMPORTING
        e_tim_tab        = lt_daytim
*       E_DATE2_EARLY    =
      EXCEPTIONS ##FM_SUBRC_OK
        invalid_datetime = 1
        days_over_limit  = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
    ENDIF.
    READ TABLE lt_daytim ASSIGNING <lwa_daytim>
                         WITH KEY day = lv_day.
    CHECK sy-subrc EQ 0.
    READ TABLE lt_fmacitm2 ASSIGNING <lwa_fmacitm> "#EC CI_ALL_FIELDS_NEEDED    "#EC CI_NOORDER
                           WITH KEY strtdat = <lwa_daytim>-day.
    IF <lwa_daytim>-hours IS ASSIGNED.
      WRITE <lwa_daytim>-hours TO lv_timadd.
    ELSE.
      CLEAR lv_timadd.
    ENDIF.
*    WRITE <lwa_daytim>-hours TO lv_timadd.
    SHIFT lv_timadd LEFT DELETING LEADING space.
    IF lv_timadd EQ '24:00'.
      lv_timadd = '23:59'.
    ENDIF.
    lv_length = strlen( lv_timadd ).
    IF lv_length EQ 4.
      CONCATENATE '0' lv_timadd(1) lv_timadd+2(2) '00'
                                      INTO lv_timadd2.
    ELSEIF lv_length EQ 3.
      CONCATENATE '00' lv_timadd+1(2) '00'
                          INTO lv_timadd2.
    ELSE.
      CONCATENATE lv_timadd(2) lv_timadd+3(2) '00'
                                   INTO lv_timadd2.
    ENDIF.
*---Replace Unreleased Interfaces
*    CALL FUNCTION 'DIMP_ADD_TIME'
*      EXPORTING
*        iv_starttime = lv_tim
*        iv_startdate = lv_day
*        iv_addtime   = lv_timadd2
*      IMPORTING
**       ev_endtime   = lv_etime
*        ev_enddate   = lv_dattmp.
    CALL FUNCTION '/AGRI/G_DIMP_ADD_TIME'
      EXPORTING
        i_starttime = lv_tim
        i_startdate = lv_day
        i_addtime   = lv_timadd2
      IMPORTING
*       E_ENDTIME   = lv_etime
        e_enddate   = lv_dattmp.
*---
    IF lv_dattmp NE lv_day.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                              NUMBER '005'
                              WITH <lwa_daytim>-day
                                   lv_name
                                   lv_resource
                              INTO sy-msgli.
      lv_subrc = 4.
      message_simple space.
      gs_variables-errors = c_true.
    ENDIF.
  ENDDO.
  CHECK gs_variables-errors = c_true.
  IF lv_fieldname   EQ 'STRTDAT'
    OR lv_fieldname EQ 'STRTTIM'.
    CLEAR: lv_strttim.
  ELSEIF lv_fieldname EQ 'FINDAT'
    OR lv_fieldname   EQ'FINTIM'.
    CLEAR: lv_fintim.
  ENDIF.

ENDFORM.                    " ITEMS_TIME_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  IDRESOURCE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_ACITM>_IDRESOURCE  text
*      -->P_<LWA_ACITM>_AUFNR  text
*----------------------------------------------------------------------*
FORM idresource_check  USING    lv_idresource TYPE /agri/fmidrsc
                                lv_aufnr      TYPE aufnr.

  DATA: lwa_resource      TYPE /agri/fmacres,
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

  SELECT SINGLE * FROM /agri/fmacres INTO lwa_resource
       WHERE idresource = lv_idresource                 "#EC CI_NOORDER
       AND rstype = c_rstype-labor.

  IF sy-subrc NE 0.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
            NUMBER '048'
            WITH lv_idresource INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ELSE.

    SELECT arbpl vgwts
      FROM crhd
      INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
      WHERE arbpl = lwa_resource-arbpl.

    DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_3 ATC 1709 PQ
    IF lt_vgwts IS NOT INITIAL.
      SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
      INTO CORRESPONDING FIELDS OF TABLE lt_parameters
      FROM tc21
      FOR ALL ENTRIES IN lt_vgwts
      WHERE vgwts = lt_vgwts-vgwts.
    ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
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
***Extended additional syntax check 1_3 ATC 1709 PQ
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
                                                      AND rstyp EQ c_accom_id-employee.
          ENDLOOP.

          IF sy-subrc NE 0.
            MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
            NUMBER '046'
            WITH lv_idresource
                lv_aufnr
          INTO sy-msgli.
            gs_variables-errors = c_true.
            message_simple space.
            EXIT.                                       "#EC CI_NOORDER
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
            NUMBER '048'
            WITH lv_idresource INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
****
  ENDIF.



ENDFORM.                    " IDRESOURCE_CHECK
