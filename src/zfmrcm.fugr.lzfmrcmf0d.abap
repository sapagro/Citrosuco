*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLacMNF0D .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock USING ls_keys TYPE zsc_fmrc_key.

  CHECK ls_keys IS NOT INITIAL.

  CALL FUNCTION 'DEQUEUE_EZ_FMRC'
    EXPORTING
      mandt = sy-mandt
      rcnum = ls_keys-rcnum
      werks = ls_keys-werks
      matnr = ls_keys-matnr.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_data_initialize  USING lv_refresh_messages.

  object_refresh_all.

  CLEAR: gs_rcdoc_infocus,
         gs_variables-document_mode,
         zsc_fmrchdr,
         zsc_fmrcvrs,
         zsc_fmrcbom,
         gs_rckey,
         gs_variables-data_changed,
         gs_variables-manual_changes,
         gs_variables-errors.

*...Vistex-11.07.2019/Begin
  REFRESH gt_similares.
*...Vistex-11.07.2019/End

  REFRESH: gt_rchdr,
           gt_fmrclst_fcat,
           gt_fmrcbom_fcat,
           gt_fmrcvrs_fcat.

  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  object_refresh_all.

  gs_variables-refresh_dose_grid = c_true.
  gs_variables-refresh_bom_grid  = c_true.
  gs_variables-refresh_vrs_grid  = c_true.


ENDFORM.                    " DOCUMENT_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_clear  CHANGING lv_continue.

  DATA: lv_answer(1).
  DATA: ls_keys TYPE zsc_fmrc_key.

  IF gs_variables-exit_after_save IS NOT INITIAL.

    PERFORM document_infocus_save USING space.

    IF gs_variables-exit_after_save EQ c_true.
      CLEAR gs_variables-exit_after_save.
      SET SCREEN 0.
      LEAVE SCREEN.
    ELSEIF gs_variables-exit_after_save EQ 'C'.
      CLEAR: gs_variables-exit_after_save.
      PERFORM document_data_initialize USING c_true.
      gs_variables-document_mode = c_mode_display.
    ENDIF.
  ELSE.
    PERFORM changes_confirm   CHANGING lv_answer.
    IF lv_answer EQ 'A'.
      IF ok_code = c_fcode-save.
        PERFORM document_infocus_save USING space.
        PERFORM document_data_initialize USING c_true.
        CLEAR ok_code.
      ELSE.
        CLEAR ok_code.
        lv_continue = c_false.
        EXIT.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING: gs_rcdoc_infocus TO ls_keys.
    PERFORM document_infocus_unlock  USING ls_keys.
    PERFORM document_data_initialize USING c_true.
    gs_variables-document_mode = c_mode_display.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_CLEAR
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_save USING lv_set_infocus.

  DATA: lt_rcdoc TYPE zt_fmrc_doc,
        lv_subrc TYPE sysubrc.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  CLEAR: gs_variables-errors.
  IF gs_rcdoc_infocus IS NOT INITIAL.
    APPEND gs_rcdoc_infocus TO lt_rcdoc.
  ENDIF.
  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    gs_rcdoc_infocus-x-rchdr.
  PERFORM rcdoc_infocus_save CHANGING lv_subrc
                                      lt_rcdoc.
  IF lv_subrc EQ 0.
*...Vistex-11.07.2019/Begin
    PERFORM save_similar USING lt_rcdoc.
*...Vistex-11.07.2019/End
    PERFORM messages_display USING gs_variables-initiator.
    PERFORM worklist_update USING gs_rcdoc_infocus.
    IF lv_set_infocus IS NOT INITIAL.
      PERFORM document_infocus_set USING gs_rckey
                                CHANGING lv_subrc.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_set  USING VALUE(ls_rckey) TYPE zsc_fmrc_key
                          CHANGING    lv_subrc.

  DATA: lv_activity(2) TYPE c,
**** ESP6 Task #30035 - Global Text Engine Integration
*        lv_txtgr  TYPE txtgr.
        lv_txtgr       TYPE /agri/gtxtgr.
****

  PERFORM document_data_initialize USING c_true.

  CHECK ls_rckey IS NOT INITIAL.

  PERFORM document_infocus_read USING ls_rckey-rcnum.
  CHECK gs_rcdoc_infocus IS NOT INITIAL.

  gs_variables-document_mode = gs_variables-overview_mode.

  IF gs_variables-overview_mode NE c_mode_display.
    MOVE c_authorization_activity-change TO lv_activity.
  ELSEIF gs_variables-overview_mode EQ c_mode_display.
    MOVE c_authorization_activity-display TO lv_activity.
  ENDIF.

  PERFORM authority_check USING gs_rcdoc_infocus-x-rchdr
                                lv_activity
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    IF lv_activity EQ c_authorization_activity-change.
      gs_variables-overview_mode = gs_variables-document_mode
                                 = c_mode_display.
    ELSE.
      gs_variables-errors = c_true.
      PERFORM document_data_initialize USING c_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_variables-document_mode = c_mode_change.
    PERFORM document_infocus_lock USING gs_rckey
                               CHANGING lv_subrc.
    IF lv_subrc NE 0.
      gs_variables-document_mode =
      gs_variables-overview_mode = c_mode_display.
    ENDIF.
  ENDIF.

  PERFORM rcdoc_type_control_read USING gs_rcdoc_infocus-x-rchdr-rctyp
                              CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_rcdoc_infocus, zsc_fmrchdr.
    EXIT.
  ENDIF.

  PERFORM tabstrip_build.
  IF gs_rcdoc_infocus-x-rchdr-txtgr IS NOT INITIAL.
    lv_txtgr = gs_rcdoc_infocus-x-rchdr-txtgr.
  ELSE.
    lv_txtgr = gs_fmrctyp-txtgr.
  ENDIF.

  object_refresh_all.
  object_publish c_object-bor gs_rcdoc_infocus-rcnum.

  PERFORM text_maintain USING lv_txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_rcdoc_infocus-x-rchdr-rcnum.
  gs_variables-refresh_dose_grid = c_true.

ENDFORM.                    " DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_lock  USING ls_keys TYPE zsc_fmrc_key
                         CHANGING lv_subrc.

  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT ls_keys IS INITIAL.

  CALL FUNCTION 'ENQUEUE_EZ_FMRC'
    EXPORTING
      mandt          = sy-mandt
      rcnum          = ls_keys-rcnum
      werks          = ls_keys-werks
      matnr          = ls_keys-matnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
    MESSAGE i005(/agri/glac) WITH ls_keys-rcnum lv_msgv1 INTO lv_msgli.
    sy-msgli = lv_msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_read  USING lv_rcnum.

  DATA: lt_rcnum  TYPE zt_fmrcnum,
        lt_ac_doc TYPE zt_fmrc_doc.

  APPEND lv_rcnum TO lt_rcnum.
  CALL FUNCTION 'ZFMRC_VIEW'
    EXPORTING
      it_rcnum       = lt_rcnum
    IMPORTING
      et_rcdoc       = lt_ac_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc EQ 0.
    READ TABLE lt_ac_doc INTO gs_rcdoc_infocus INDEX 1.
  ENDIF.

  MOVE-CORRESPONDING gs_rcdoc_infocus TO gs_rckey.

ENDFORM.                    " DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*&      Form  DROPDOWN_VALUES_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_values_fill .

  DATA: lt_dynpfields TYPE dynpread_tabtype,
        ls_dynpfield  LIKE LINE OF lt_dynpfields,
        lt_list       TYPE vrm_values,
        lv_name       TYPE vrm_id.

*-- Lista Técnica Alternativa
  lv_name = 'ZSC_FMRCHDR-STLAL'.
  REFRESH lt_list.

  IF gs_rcdoc_infocus-rcnum IS NOT INITIAL.
    DATA(lv_rcnum) = gs_rcdoc_infocus-rcnum.
  ELSE.
    lv_rcnum = zsc_fmrchdr-rcnum.
  ENDIF.

  IF lv_rcnum IS NOT INITIAL.
    SELECT * FROM zfmrclst
      INTO TABLE @DATA(lt_materiais)
     WHERE rcnum = @lv_rcnum.

    LOOP AT lt_materiais INTO DATA(lwa_material).
      INSERT INITIAL LINE INTO TABLE lt_list
        ASSIGNING FIELD-SYMBOL(<lwa_list>).
      IF sy-subrc EQ 0.
        <lwa_list>-key = lwa_material-stlal.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM lt_list COMPARING ALL FIELDS.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list.

*-- Base de Cálculo
  lv_name = 'ZSC_FMRCHDR-AUSME'.
  REFRESH lt_list.

  SELECT SINGLE ausme
    FROM marc
    INTO @DATA(lv_ausme) "zsc_fmrchdr-ausme
   WHERE matnr = @zsc_fmrchdr-matnr
     AND werks = @zsc_fmrchdr-werks.

  IF sy-subrc EQ 0.
    IF zsc_fmrchdr-ausme IS INITIAL.
      zsc_fmrchdr-ausme = lv_ausme.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE lt_list ASSIGNING <lwa_list>.
    IF sy-subrc EQ 0.
      <lwa_list>-key = lv_ausme.
      IF <lwa_list>-key = 'VC'.
        <lwa_list>-text = 'Volume de Calda'.
      ELSE.
        <lwa_list>-text = 'Hectare'.
      ENDIF.
    ENDIF.

    IF zsc_fmrchdr-rctyp EQ 'ZORC'.
      IF lv_ausme EQ 'HC'.
        INSERT INITIAL LINE INTO TABLE lt_list ASSIGNING <lwa_list>.
        IF sy-subrc EQ 0.
          <lwa_list>-key = 'VC'.
          <lwa_list>-text = 'Volume de Calda'.
        ENDIF.
      ENDIF.

      IF lv_ausme EQ 'VC'.
        INSERT INITIAL LINE INTO TABLE lt_list ASSIGNING <lwa_list>.
        IF sy-subrc EQ 0.
          <lwa_list>-key = 'HC'.
          <lwa_list>-text = 'Hectare'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT lt_list BY key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_name
      values          = lt_list[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  ls_dynpfield-fieldname = 'ZSC_FMRCHDR-AUSME'.
  APPEND ls_dynpfield TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

ENDFORM.                    " DROPDOWN_VALUES_FILL
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_prepare .

  DATA: lv_error,
        lv_subrc     TYPE sy-subrc,
        lv_subrc_tmp TYPE sy-subrc.

  CHECK gs_variables-document_mode EQ c_mode_create.

  gs_rcdoc_infocus-updkz = c_updkz_new.
  IF gs_rcdoc_infocus-x-rchdr-rcnum IS INITIAL.
    gs_rcdoc_infocus-rcnum = TEXT-046.
    gs_rcdoc_infocus-x-rchdr-rcnum = TEXT-046.
  ELSE.
    gs_rcdoc_infocus-rcnum = gs_rcdoc_infocus-x-rchdr-rcnum.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_rcdoc_infocus-x-rchdr
                                c_authorization_activity-create
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    CLEAR: gs_variables-document_mode,
           gs_rcdoc_infocus, zsc_fmrchdr.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

****Crop Type
  CLEAR gs_variables-rctyp_in_focus.
  PERFORM rcdoc_type_control_read USING gs_rcdoc_infocus-x-rchdr-rctyp
                              CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_rcdoc_infocus, zsc_fmrchdr.
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING gs_tfmrctyp TO gs_rcdoc_infocus-x-rchdr.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING gs_fmrctyp-txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_rcdoc_infocus-x-rchdr-rcnum.
  CLEAR: ts_items-activetab.

ENDFORM.                    " DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_check  USING    lv_before_save
                     CHANGING lv_subrc TYPE sy-subrc.
  PERFORM cm_fv_prod_vers_db_update.
ENDFORM.                    " DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_update_all  CHANGING lt_data TYPE STANDARD TABLE
                               lv_subrc TYPE sy-subrc.
*  PERFORM quantities_totalize.
ENDFORM.                    " DATA_UPDATE_ALL
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM descriptions_display .

  DATA: lv_subrc  TYPE sy-subrc,
        lwa_dd07v TYPE dd07v.

  STATICS: lt_dd07v TYPE STANDARD TABLE OF dd07v.

  CASE sy-dynnr.
    WHEN c_screen-quick_info.
      IF gs_fmrctyp IS INITIAL AND
         gs_rcdoc_infocus-x-rchdr-rctyp IS NOT INITIAL.
        PERFORM rcdoc_type_control_read USING gs_rcdoc_infocus-x-rchdr-rctyp
                                    CHANGING lv_subrc.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*& Form DESCRIPTION_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&---------------------------------------------------------------------*
FORM description_display  USING VALUE(lwa_mod_row_value)
                          CHANGING lv_maktx TYPE text40.

  PERFORM conver_material_set USING lwa_mod_row_value
                          CHANGING lwa_mod_row_value.
  SELECT SINGLE maktx FROM makt
                      INTO ( lv_maktx )
                          WHERE matnr = lwa_mod_row_value
                            AND spras = sy-langu.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATE_VALIDATE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&      --> LWA_GMCONDPRI_LAYOUT_DATAB
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM date_validate_check  USING    lv_datbi  TYPE datab
                                   lv_datab  TYPE datab
                          CHANGING lv_subrc.
  IF lv_datbi GE lv_datab.
    MESSAGE ID 'DB' TYPE 'E' NUMBER '650' INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    MOVE 4 TO lv_subrc.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM date_check .
  IF zsc_fmrcvrs-bdatu IS NOT INITIAL
    AND zsc_fmrcvrs-adatu IS NOT INITIAL.
    IF zsc_fmrcvrs-bdatu <  zsc_fmrcvrs-adatu.
      SET CURSOR FIELD 'ZSC_FMRCVRS-BDATU'.
      MESSAGE ID 'DB' TYPE 'E' NUMBER '650'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEFAULT_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM default_value .
  MOVE: 'N' TO zsc_fmrcvrs-plnty,
         1 TO gv_numocurren.
ENDFORM.

FORM dose_grid_update.

  DATA: lwa_rcdose      TYPE zsc_fmrclst,
        lwa_dose_layout LIKE LINE OF gt_fmrclst_fcat,
        ls_fmrcnum      TYPE zsc_fmrcnum,
        lt_fmrci_check  TYPE zt_fmrcnum,
        ls_fmrci_check  TYPE zsc_fmrcnum,
        lt_rcnum        TYPE zt_fmrcnum,
        lwa_mod_row     TYPE lvc_s_modi,
        lv_typ          TYPE char2 VALUE '02',
        lv_tabix        TYPE sy-tabix,
        lv_modified,
        lv_subrc        TYPE int4,
        lt_matnr_tab    TYPE zt_fmrcmatnr_tab,
        lv_valid.

  FIELD-SYMBOLS: <lwa_rclst>       TYPE zsc_fmrclst,
                 <lwa_dose_layout> TYPE zsc_fmrclst_fcat.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_rcdoc_infocus-x-rchdr.
  lv_modified = ref_grid_rclst->data_modified_check( ).
  IF lv_modified EQ c_true OR
  gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_grid_rclst->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_dose_grid = c_true.
    ENDIF.
  ENDIF.
**  IF sy-uname EQ 'T_T.KONNO'.
**    BREAK-POINT.
**  ENDIF.
  PERFORM material_read CHANGING lt_matnr_tab
                                 lv_subrc.
  LOOP AT gt_dose_modi INTO lwa_mod_row.
    lv_tabix = sy-tabix.
    READ TABLE gt_fmrclst_fcat INTO lwa_dose_layout INDEX lwa_mod_row-row_id.
*...Vistex-11.01.2019/Begin
*    CHECK lwa_dose_layout IS NOT INITIAL.
*    CHECK lwa_mod_row-value IS NOT INITIAL
*       OR lwa_mod_row-fieldname EQ 'RCINP'.
    IF ( lwa_dose_layout IS INITIAL )
    OR ( lwa_mod_row-value IS INITIAL AND lwa_mod_row-fieldname NE 'RCINP' ).
      CONTINUE.
    ENDIF.
*...Vistex-11.01.2019/End
    IF lwa_mod_row-fieldname EQ 'MATNR_INS'.
      material_covert lwa_mod_row-value lwa_mod_row-value.
*...Vistex-11.01.2019/Begin
*      READ TABLE lt_matnr_tab INTO DATA(lwa_matnr_tab)
*        WITH KEY matnr = lwa_mod_row-value.
      READ TABLE lt_matnr_tab INTO DATA(lwa_matnr_tab)
        WITH KEY matnr = lwa_mod_row-value BINARY SEARCH.
*...Vistex-11.01.2019/End
      IF sy-subrc NE 0.
*...Vistex-11.01.2019/Begin
*        MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '097'
*                                                   WITH lwa_mod_row-value
*                                                        zsc_fmrchdr-werks
*                                                        gs_tfmrctyp-mtart
*                                                   INTO sy-msgli.
*...Material &1 inválido! Utilize insumos existentes no centro &2!
        MESSAGE e105(zfmrc) WITH lwa_mod_row-value zsc_fmrchdr-werks
          DISPLAY LIKE 'I'.
*...Vistex-11.01.2019/End
        message_simple space.
        gs_variables-errors = c_true.
        EXIT.
      ENDIF.
      PERFORM description_display USING lwa_mod_row-value
                                  CHANGING lwa_dose_layout-maktx.
      PERFORM unit_management_display USING lwa_mod_row-value
                                      CHANGING lwa_dose_layout-units.
      PERFORM material_duplicate_check USING lwa_mod_row-value
                                       CHANGING lv_subrc.
      IF lv_subrc NE 0.
        EXIT.
      ENDIF.
    ENDIF.
*--- Material Principal update
    IF lwa_mod_row-fieldname EQ 'RCINP'.
      IF lwa_dose_layout-matnr IS NOT INITIAL.
        PERFORM material_principal_update USING lwa_mod_row-row_id
                                          CHANGING lwa_dose_layout.
      ELSE.
        MESSAGE ID 'ZFMRC' TYPE 'E' NUMBER '089' INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
        EXIT.
      ENDIF.

    ENDIF.
    MOVE-CORRESPONDING lwa_dose_layout TO lwa_rcdose.
    READ TABLE gs_rcdoc_infocus-x-rclst ASSIGNING <lwa_rclst>
                                        WITH KEY rcnum = lwa_dose_layout-rcnum
                                                 werks = lwa_dose_layout-werks
                                                 matnr = lwa_dose_layout-matnr
                                                 matnr_ins = lwa_dose_layout-matnr_ins
                                                 stlal     = lwa_dose_layout-stlal.
    IF sy-subrc EQ 0.
      IF lwa_rcdose NE <lwa_rclst>.
        gs_variables-document_changed = c_true.
        IF <lwa_rclst>-updkz NE c_updkz_new.
          MOVE lwa_rcdose TO <lwa_rclst>.
          <lwa_rclst>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_rcdose TO <lwa_rclst>.
          <lwa_rclst>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE: zsc_fmrchdr-rcnum TO gs_rcdoc_infocus-rcnum,
            zsc_fmrchdr-matnr TO gs_rcdoc_infocus-matnr,
            zsc_fmrchdr-werks TO gs_rcdoc_infocus-werks,
            c_true            TO gs_variables-refresh_dose_grid,
            c_true            TO gs_variables-document_changed ,
       gs_rcdoc_infocus-rcnum TO lwa_dose_layout-rcnum,
       gs_rcdoc_infocus-matnr TO lwa_dose_layout-matnr,
       gs_rcdoc_infocus-werks TO lwa_dose_layout-werks,
       zsc_fmrchdr-stlal TO lwa_dose_layout-stlal,
                 c_updkz_new  TO lwa_dose_layout-updkz .
      PERFORM new_position_set USING 'RCLST'
                               CHANGING lwa_dose_layout-posnr.
      MOVE-CORRESPONDING lwa_dose_layout TO lwa_rcdose.
*...Vistex-11.06.2019/Begin
      lwa_rcdose-rcinp = icon_wd_radio_button_empty.
*...Vistex-11.06.2019/End
      APPEND lwa_rcdose TO gs_rcdoc_infocus-x-rclst.
    ENDIF.
    DELETE gt_dose_modi INDEX lv_tabix.
  ENDLOOP.
  IF zsc_fmrchdr-stlal IS NOT INITIAL
  AND zsc_fmrchdr-stlnr IS NOT INITIAL.
    PERFORM bom_update.
  ENDIF.
  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_dose_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_dose_grid.
    ENDIF.
  ENDIF.

ENDFORM.

FORM duplicatekey_check.

  SELECT * UP TO 1 ROWS
    FROM zfmrchdr
    INTO @DATA(ls_zfmrchdr)
   WHERE matnr = @zsc_fmrchdr-matnr
     AND werks = @zsc_fmrchdr-werks
     AND datuv = @zsc_fmrchdr-datuv.
  ENDSELECT.

  IF sy-subrc EQ 0.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '096'
     WITH ls_zfmrchdr-rcnum zsc_fmrchdr-matnr zsc_fmrchdr-werks.
    SET CURSOR  FIELD 'ZSC_FMRCHDR-MATNR'.
    EXIT.
  ENDIF.

ENDFORM.

FORM delete_all.
  PERFORM all_alternatives_delete.
  PERFORM all_versions_delete.
ENDFORM.
