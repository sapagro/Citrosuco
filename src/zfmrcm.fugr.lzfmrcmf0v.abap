
*&---------------------------------------------------------------------*
*& Form rcbom_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM rcbom_grid_data_prepare .

  DATA:lwa_rcbom      TYPE zsc_fmrcbom,
       lwa_rcbom_fcat LIKE LINE OF gt_fmrcbom_fcat.

  CHECK ref_grid_rcbom IS INITIAL OR
        gs_variables-refresh_bom_grid EQ c_true.

  REFRESH: gt_fmrcbom_fcat.

  LOOP AT gs_rcdoc_infocus-x-rcbom INTO lwa_rcbom.
    MOVE-CORRESPONDING lwa_rcbom TO lwa_rcbom_fcat.
    APPEND lwa_rcbom_fcat TO gt_fmrcbom_fcat.
    CLEAR lwa_rcbom_fcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form rcbom_GRID_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM rcbom_grid_update .

  DATA: lwa_rcbom      TYPE zsc_fmrcbom,
        lwa_bom_layout LIKE LINE OF gt_fmrcbom_fcat,
        ls_fmrcbom     TYPE zsc_fmrcbom,
        lt_rcbom       TYPE zt_fmrcbom,
        lwa_mod_row    TYPE lvc_s_modi,
        lv_typ         TYPE char2 VALUE '02',
        lv_tabix       TYPE sy-tabix,
        lv_modified,
        lv_subrc       TYPE int4,
        lv_valid.

  DATA:
    lt_glmdhdr TYPE /agri/t_glmdhdr,
    lt_glmdatv TYPE /agri/t_glmdatv.

  FIELD-SYMBOLS: <lwa_rcbom>      TYPE zsc_fmrcbom,
                 <lwa_bom_layout> TYPE zsc_fmrcbom_fcat.

*...Vistex-11.06.2019/Begin
  IF gs_fmrchdr_previous-stlal NE zsc_fmrchdr-stlal.
    PERFORM alternative_change.
  ENDIF.
*...Vistex-11.06.2019/End

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_rcdoc_infocus-x-rchdr.
  CHECK zsc_fmrchdr-stlal IS NOT INITIAL
  AND zsc_fmrchdr-stlnr IS NOT INITIAL.
  LOOP AT gs_rcdoc_infocus-x-rclst INTO DATA(lwa_rclst).
    lv_tabix = sy-tabix.
    READ TABLE gs_rcdoc_infocus-x-rcbom ASSIGNING <lwa_rcbom>
                                      WITH KEY rcnum = lwa_rclst-rcnum
                                               werks = lwa_rclst-werks
                                               matnr = lwa_rclst-matnr
                                               stlnr = zsc_fmrchdr-stlnr
                                               stlal = zsc_fmrchdr-stlal
                                               matnr_ins = lwa_rclst-matnr_ins.
    IF sy-subrc EQ 0.
      gs_variables-document_changed = c_true.
      IF <lwa_rcbom>-updkz NE c_updkz_new.
        MOVE-CORRESPONDING lwa_rclst TO <lwa_rcbom>.
        <lwa_rcbom>-updkz = c_updkz_update.
        <lwa_rcbom>-stlnr = zsc_fmrchdr-stlnr.
        <lwa_rcbom>-stlal = zsc_fmrchdr-stlal.
      ELSE.
        MOVE-CORRESPONDING lwa_rclst TO <lwa_rcbom>.
        <lwa_rcbom>-updkz = c_updkz_new.
        <lwa_rcbom>-stlnr = zsc_fmrchdr-stlnr.
        <lwa_rcbom>-stlal = zsc_fmrchdr-stlal.
      ENDIF.
    ELSE.
      MOVE: zsc_fmrchdr-rcnum TO gs_rcdoc_infocus-rcnum,
            zsc_fmrchdr-matnr TO gs_rcdoc_infocus-matnr,
            zsc_fmrchdr-werks TO gs_rcdoc_infocus-werks.
      gs_variables-refresh_bom_grid = c_true.
      gs_variables-document_changed = c_true.
      lwa_rclst-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_rclst TO lwa_rcbom.
      lwa_rcbom-stlnr = zsc_fmrchdr-stlnr.
      lwa_rcbom-stlal = zsc_fmrchdr-stlal.
      APPEND lwa_rcbom TO gs_rcdoc_infocus-x-rcbom.
    ENDIF.
    gs_variables-refresh_bom_grid = c_true.
  ENDLOOP.

  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_bom_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_bom_grid.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VERSION_DATA_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM version_grid_update .

  DATA: lwa_rcversion      TYPE zsc_fmrcvrs,
        lwa_version_layout LIKE LINE OF gt_fmrcvrs_fcat,
        ls_fmrcnum         TYPE zsc_fmrcnum,
        lt_rcnum           TYPE zt_fmrcnum,
        lwa_mod_row        TYPE lvc_s_modi,
        lv_typ             TYPE char2 VALUE '02',
        lv_tabix           TYPE sy-tabix,
        lv_verid           TYPE verid,
        lv_subrc_vers      TYPE int4,
        lt_alter           TYPE zt_fmrcalter_tab,
        lv_alter_numc2     TYPE numc2,
        lt_rout            TYPE zt_fmrcrout_tab,
        lv_modified,
        lv_subrc           TYPE int4,
        lv_valid.

  FIELD-SYMBOLS: <lwa_rcvrs>          TYPE zsc_fmrcvrs,
                 <lwa_version_layout> TYPE zsc_fmrcvrs_fcat.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_rcdoc_infocus-x-rchdr.
  lv_modified = ref_grid_rcvrs->data_modified_check( ).
  IF lv_modified EQ c_true OR
  gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_grid_rcvrs->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_vrs_grid = c_true.
    ENDIF.
  ENDIF.

  PERFORM alter_read CHANGING lt_alter
                              lv_subrc.
  PERFORM rout_read CHANGING lt_rout
                             lv_subrc.

  LOOP AT gt_rcvrs_modi INTO lwa_mod_row.
    lv_tabix = sy-tabix.
    IF lwa_mod_row-value IS NOT INITIAL.
      CASE lwa_mod_row-fieldname.
        WHEN 'STLAL'.
          MOVE lwa_mod_row-value TO lv_alter_numc2.
          READ TABLE lt_alter INTO DATA(lwa_alter)
                              WITH KEY stlal = lv_alter_numc2.
          IF sy-subrc NE 0.
            MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '093'
                                                       WITH lwa_mod_row-value
                                                            zsc_fmrchdr-matnr
                                                            zsc_fmrchdr-werks
                                                       INTO sy-msgli.
            message_simple space.
            gs_variables-errors = c_true.
            EXIT.
          ENDIF.
        WHEN 'PLNNR'.
          READ TABLE lt_rout INTO DATA(lwa_rout)
                              WITH KEY plnnr = lwa_mod_row-value.
          IF sy-subrc NE 0.
            MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '094'
                                                       WITH lwa_mod_row-value
                                                       zsc_fmrchdr-matnr
                                                       zsc_fmrchdr-werks
                                                       INTO sy-msgli.
            message_simple space.
            gs_variables-errors = c_true.
            EXIT.
          ENDIF.
        WHEN 'VERID'.
          MOVE lwa_mod_row-value TO lv_verid.
          PERFORM code_vers_check USING lv_verid
                             CHANGING lv_subrc_vers.
          IF lv_subrc_vers EQ 0.
*           A versão & já existe
            MESSAGE e101(zfmrc) WITH lv_verid.
*...Vistex-11.06.2019/Begin
          ELSE.
            READ TABLE gs_rcdoc_infocus-x-rcvrs
              ASSIGNING <lwa_rcvrs> INDEX lwa_mod_row-row_id.
            IF sy-subrc EQ 0.
              <lwa_rcvrs>-verid = lv_verid.
            ENDIF.
*...Vistex-11.06.2019/End
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
    CLEAR: lv_alter_numc2.
    READ TABLE gt_fmrcvrs_fcat INTO lwa_version_layout INDEX lwa_mod_row-row_id.
    CHECK lwa_version_layout IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_version_layout TO lwa_rcversion.
*...Vistex-11.06.2019/Begin
*    READ TABLE gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
*                                        WITH KEY matnr = lwa_version_layout-matnr
*                                                 werks = lwa_version_layout-werks
*                                                 rcnum = lwa_version_layout-rcnum
*                                                 posnr = lwa_version_layout-posnr.
    READ TABLE gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
      WITH KEY matnr = lwa_version_layout-matnr
               werks = lwa_version_layout-werks
               rcnum = lwa_version_layout-rcnum
               verid = lwa_version_layout-verid.
*...Vistex-11.06.2019/End
    IF sy-subrc EQ 0.
      IF lwa_rcversion NE <lwa_rcvrs>.
        gs_variables-document_changed = c_true.
        IF <lwa_rcvrs>-updkz NE c_updkz_new.
          MOVE lwa_rcversion TO <lwa_rcvrs>.
          <lwa_rcvrs>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_rcversion TO <lwa_rcvrs>.
          <lwa_rcvrs>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
    ENDIF.
    DELETE gt_rcvrs_modi INDEX lv_tabix.
  ENDLOOP.

  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_vrs_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_vrs_grid.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VERSION_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM version_grid_data_prepare .

  DATA:
    lwa_version_fcat  LIKE LINE OF gt_fmrcvrs_fcat.

  CHECK ref_grid_rcvrs IS INITIAL OR
        gs_variables-refresh_vrs_grid EQ c_true.

  REFRESH: gt_fmrcvrs_fcat.
  LOOP AT gs_rcdoc_infocus-x-rcvrs INTO DATA(lwa_rcvrs).
    MOVE-CORRESPONDING lwa_rcvrs TO lwa_version_fcat.
    PERFORM assigned_vrs_styles_prepare CHANGING lwa_version_fcat.
    APPEND lwa_version_fcat TO gt_fmrcvrs_fcat.
    CLEAR lwa_version_fcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VERSION_DATA_PREPARED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM version_data_prepared .

  DATA: lwa_rcversion      TYPE zsc_fmrcvrs,
        lwa_version_layout LIKE LINE OF gt_fmrcvrs_fcat,
        lt_rcnum           TYPE zt_fmrcnum,
        lwa_mod_row        TYPE lvc_s_modi,
        lv_typ             TYPE char2 VALUE '02',
        lv_verid           TYPE verid,
        lv_tabix           TYPE sy-tabix,
        lt_mapl            TYPE TABLE OF mapl,
        lv_modified,
        lv_subrc           TYPE int4,
        lv_subrc_vers      TYPE int4,
        lv_valid.

  FIELD-SYMBOLS: <lwa_rcvrs>          TYPE zsc_fmrcvrs,
                 <lwa_version_layout> TYPE zsc_fmrcvrs_fcat,
                 <lwa_mapl>           TYPE mapl.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_rcdoc_infocus-x-rchdr.

  PERFORM group_routing_obtain CHANGING lt_mapl.
  CHECK zsc_fmrcvrs IS NOT INITIAL.
  DO gv_numocurren TIMES.
    MOVE-CORRESPONDING zsc_fmrcvrs TO lwa_rcversion.
*...Vistex-11.06.2019/Begin
*    READ TABLE gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
*                                        WITH KEY matnr = lwa_version_layout-matnr
*                                                 werks = lwa_version_layout-werks
*                                                 rcnum = lwa_version_layout-rcnum
*                                                 posnr = lwa_version_layout-posnr.
    READ TABLE gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
      WITH KEY matnr = lwa_version_layout-matnr
               werks = lwa_version_layout-werks
               rcnum = lwa_version_layout-rcnum
               verid = lwa_version_layout-verid.
*...Vistex-11.06.2019/End
    IF sy-subrc EQ 0.
      IF lwa_rcversion NE <lwa_rcvrs>.
        gs_variables-document_changed = c_true.
        IF <lwa_rcvrs>-updkz NE c_updkz_new.
          MOVE lwa_rcversion TO <lwa_rcvrs>.
          <lwa_rcvrs>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_rcversion TO <lwa_rcvrs>.
          <lwa_rcvrs>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE: zsc_fmrchdr-rcnum TO gs_rcdoc_infocus-rcnum,
            zsc_fmrchdr-matnr TO gs_rcdoc_infocus-matnr,
            zsc_fmrchdr-werks TO gs_rcdoc_infocus-werks,
            c_true            TO gs_variables-refresh_vrs_grid,
            c_true            TO gs_variables-document_changed .
      MOVE-CORRESPONDING zsc_fmrcvrs TO lwa_version_layout.
      READ TABLE lt_mapl ASSIGNING <lwa_mapl>
                         WITH KEY matnr = zsc_fmrchdr-matnr
                                  werks = zsc_fmrchdr-werks.
      IF sy-subrc EQ 0.
        MOVE <lwa_mapl>-plnnr TO lwa_version_layout-plnnr.
        MOVE <lwa_mapl>-plnal TO lwa_version_layout-plnal.
      ENDIF.
      MOVE:
     gs_rcdoc_infocus-rcnum TO lwa_version_layout-rcnum,
     gs_rcdoc_infocus-matnr TO lwa_version_layout-matnr,
     gs_rcdoc_infocus-werks TO lwa_version_layout-werks,
     '1'                    TO lwa_version_layout-stlan,
               c_updkz_new  TO lwa_version_layout-updkz.
*...Vistex-11.06.2019/Begin
*      PERFORM new_position_set USING 'RCVRS'
*                               CHANGING lwa_version_layout-posnr.
*...Vistex-11.06.2019/End
      MOVE-CORRESPONDING lwa_version_layout TO lwa_rcversion.
      APPEND lwa_rcversion TO gs_rcdoc_infocus-x-rcvrs.
    ENDIF.
    CLEAR: lwa_version_layout,
           lwa_rcversion.
  ENDDO.

  gs_variables-refresh_vrs_grid = c_true.
  CLEAR: gv_numocurren.
  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_vrs_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_vrs_grid.
    ENDIF.
  ENDIF.

ENDFORM.

FORM cm_fv_prod_vers_db_update.

  DATA: lt_mkal_i     TYPE TABLE OF mkal,
        ls_mkal_i     TYPE  mkal,
        lt_mkal_u     TYPE TABLE OF mkal,
        ls_mkal_u     TYPE  mkal,
        lt_mkal_d     TYPE TABLE OF mkal,
        ls_mkal_d     TYPE  mkal,
        lv_subrc_vers TYPE int4,
        lt_mkal_aend  TYPE TABLE OF  mkal_aend.

  FIELD-SYMBOLS: <lwa_rcvrs> TYPE zsc_fmrcvrs.

  DATA: lv         TYPE i,
        lv_message TYPE string,
        lr_exc     TYPE REF TO cx_sy_open_sql_error.

  LOOP AT gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
                                  WHERE updkz IS NOT INITIAL .
    CASE <lwa_rcvrs>-updkz.
      WHEN c_updkz_new.
        PERFORM code_vers_check USING <lwa_rcvrs>-verid
                               CHANGING lv_subrc_vers.
        IF lv_subrc_vers NE 0.
          MOVE-CORRESPONDING <lwa_rcvrs> TO ls_mkal_i.
*...Vistex-11.06.2019/Begin
          ls_mkal_i-alnal = <lwa_rcvrs>-plnal.
*...Vistex-11.06.2019/End
          MOVE sy-datum TO ls_mkal_i-prdat.
          APPEND ls_mkal_i TO lt_mkal_i.
          MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '098' WITH ls_mkal_i-verid
                                                                  INTO sy-msgli.
        ELSE.
          MESSAGE ID 'ZFMRC'
                     TYPE c_msg_type-error
                             NUMBER '101'
                             WITH  <lwa_rcvrs>-verid.
        ENDIF.
      WHEN c_updkz_update.
        MOVE-CORRESPONDING <lwa_rcvrs> TO ls_mkal_u.
        MOVE sy-datum TO ls_mkal_u-prdat.
        APPEND ls_mkal_u TO lt_mkal_u.
        MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '099' WITH ls_mkal_u-verid
                                                                INTO sy-msgli.
        message_simple space.
    ENDCASE.
  ENDLOOP.

  LOOP AT gs_rcdoc_infocus-y-rcvrs INTO DATA(lwa_rcvrs_d)
                                   WHERE updkz IS NOT INITIAL .
    CASE lwa_rcvrs_d-updkz.
      WHEN c_updkz_delete.
        MOVE-CORRESPONDING lwa_rcvrs_d TO ls_mkal_d.
        APPEND ls_mkal_d TO lt_mkal_d.
        MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '100' WITH ls_mkal_d-verid
                                                                INTO sy-msgli.
        message_simple space.
    ENDCASE.
  ENDLOOP.

  TRY.
      CALL FUNCTION 'CM_FV_PROD_VERS_DB_UPDATE'
        TABLES
          it_mkal_i    = lt_mkal_i
          it_mkal_u    = lt_mkal_u
          it_mkal_d    = lt_mkal_d
          it_mkal_aend = lt_mkal_aend.

    CATCH cx_sy_open_sql_error INTO lr_exc.
      lv_message = lr_exc->get_text( ).
  ENDTRY.

  REFRESH:
        lt_mkal_i,
        lt_mkal_u,
        lt_mkal_d,
        lt_mkal_aend.

  PERFORM bapi_commit.

ENDFORM.
