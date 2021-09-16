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
*& Form ITEMS_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM items_grid_data_prepare.

  TYPES: BEGIN OF ly_terrain,
           input  TYPE /agri/gltplnr_fl,
           output TYPE /agri/gltplnr_fl,
           season TYPE /agri/gl_season,
         END OF ly_terrain.

  DATA: lt_terrain     TYPE STANDARD TABLE OF ly_terrain INITIAL SIZE 0,
        lt_sorted      LIKE gs_acdoc_infocus-x-acitm,
        lwa_acitm      TYPE zsc_fmacitm,
        lwa_items_fcat LIKE LINE OF gt_fmacitm_fcat.

  CHECK ref_grid_items IS INITIAL OR
        gs_variables-refresh_items_grid EQ c_true.

  REFRESH: gt_fmacitm_fcat,
*-- BOC T_T.KONNO 04.07.21
           gt_glflcma.
*-- EOC T_T.KONNO 04.07.21

*...BOC-T_T.KONNO-12.23.19
*  SORT gs_acdoc_infocus-x-acitm BY acpos ASCENDING.
*
*  LOOP AT gs_acdoc_infocus-x-acitm INTO lwa_acitm.
*    INSERT INITIAL LINE INTO TABLE lt_terrain
*      ASSIGNING FIELD-SYMBOL(<lwa_terrain>).
*    IF sy-subrc EQ 0.
*      <lwa_terrain>-input = lwa_acitm-tplnr_fl.
*      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
*        EXPORTING
*          input  = lwa_acitm-tplnr_fl
*        IMPORTING
*          output = <lwa_terrain>-output.
*      <lwa_terrain>-season = lwa_acitm-season.
*    ENDIF.
*  ENDLOOP.
*
*  SORT: lt_terrain BY output season.
*
*  LOOP AT lt_terrain INTO DATA(lwa_terrain).
*    READ TABLE gs_acdoc_infocus-x-acitm INTO lwa_acitm
*      WITH KEY tplnr_fl = lwa_terrain-input
*               season   = lwa_terrain-season.
*    IF sy-subrc EQ 0.
*      INSERT INITIAL LINE INTO TABLE lt_sorted
*        ASSIGNING FIELD-SYMBOL(<lwa_sorted>).
*      IF sy-subrc EQ 0.
*        <lwa_sorted> = lwa_acitm.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  gs_acdoc_infocus-x-acitm[] = lt_sorted.
*...EOC-T_T.KONNO-12.23.19

  LOOP AT gs_acdoc_infocus-x-acitm INTO lwa_acitm.
    MOVE-CORRESPONDING lwa_acitm TO lwa_items_fcat.
    PERFORM assigned_items_styles_prepare CHANGING lwa_items_fcat.
    APPEND lwa_items_fcat TO gt_fmacitm_fcat.
    CLEAR lwa_items_fcat.
  ENDLOOP.

  PERFORM initial_lines_set.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEMS_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM items_get USING lv_ajahr TYPE ajahr
                     lv_typq  TYPE char1
            CHANGING lt_acitm TYPE zt_fmacitm.

  DATA: lt_acitm_old TYPE zt_fmacitm,
*...BOC-T_T.KONNO
        lt_safras    TYPE type_safras_tab,
*...EOC-T_T.KONNO
        lv_subrc     TYPE sy-subrc.

*...BOC-T_T.KONNO
*  PERFORM terrains_anabled_set USING lv_ajahr
*                                     lv_typq
*                            CHANGING lt_acitm.
  PERFORM terrains_anabled_set USING lv_typq
                            CHANGING lt_acitm.
*...EOC-T_T.KONNO

  IF lt_acitm[] IS NOT INITIAL.
*...BOC-T_T.KONNO
*    PERFORM crop_area_validate CHANGING lt_acitm
*                                        lv_subrc
*                                        lv_ajahr.
    PERFORM crop_area_validate CHANGING lt_acitm
                                        lt_acitm_old
                                        lv_subrc
                                        lv_ajahr.
*...EOC-T_T.KONNO
    IF lv_subrc EQ 0.
*...BOC-T_T.KONNO
*        PERFORM terrain_attribute_get CHANGING lt_acitm.
      PERFORM terrain_attribute_get CHANGING lt_acitm
                                             lt_safras.
*...EOC-T_T.KONNO
    ELSE.
      MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '074' INTO sy-msgli.
      message_simple c_false.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '072' INTO sy-msgli.
    message_simple c_false.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ITEMS_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE items_grid_update INPUT.

  DATA: lwa_acitm       TYPE zsc_fmacitm,
        lwa_item_layout LIKE LINE OF gt_fmacitm_fcat,
        ls_fmacitm      TYPE zsc_fmacitm,
        lt_acitm_old    TYPE zt_fmacitm,
        lt_fmaci_check  TYPE zt_fmacitm,
*...BOC-T_T.KONNO
        lt_new_line     LIKE lt_fmaci_check,
        lt_safras       TYPE type_safras_tab,
        lt_seasons      TYPE type_seasons_tab,
        lv_season       TYPE /agri/glflcma-season,
*...EOC-T_T.KONNO
        ls_fmaci_check  TYPE zsc_fmacitm,
        lt_acitm        TYPE zt_fmacitm,
        lwa_mod_row     TYPE lvc_s_modi,
        lv_typ          TYPE char2 VALUE '02',
        lv_tabix        TYPE sy-tabix,
        lv_modified,
        lv_subrc        TYPE int4,
        lt_glflcma      TYPE /agri/t_glflcma,
        lt_glcsprs      TYPE /agri/t_glcsprs,
        lt_glcsprso     TYPE /agri/t_glcsprso,
        lt_mast         TYPE /agri/t_gmast_so,
        lt_stpo         TYPE tt_ccm_bomitem,
        lv_valid.

  FIELD-SYMBOLS: <lwa_acitm>       TYPE zsc_fmacitm,
                 <lwa_item_layout> TYPE zsc_fmacitm_fcat.

  CHECK gs_variables-document_mode NE c_mode_display.

  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_acdoc_infocus-x-achdr.

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

  LOOP AT gt_items_modi INTO lwa_mod_row.
    lv_tabix = sy-tabix.

    READ TABLE gt_fmacitm_fcat INTO lwa_item_layout INDEX lwa_mod_row-row_id.
    CHECK lwa_item_layout IS NOT INITIAL.
    IF lwa_mod_row-fieldname EQ 'TPLNR_FL'.
      MOVE-CORRESPONDING lwa_item_layout TO ls_fmaci_check.
      APPEND ls_fmaci_check TO lt_fmaci_check.
*...BOC-T_T.KONNO
*      PERFORM crop_area_validate CHANGING lt_fmaci_check
*                                          lv_subrc
*                                          zsc_fmachdr-ajahr.

      lt_new_line[] = lt_fmaci_check[].
      PERFORM crop_area_validate CHANGING lt_fmaci_check
                                          lt_acitm_old
                                          lv_subrc
                                          zsc_fmachdr-ajahr.

      IF lv_subrc NE 0.
        PERFORM check_valid_crop_season USING lwa_mod_row-value
                                     CHANGING lt_new_line
                                              lt_seasons
                                              lv_season
                                              lv_subrc.
      ENDIF.

*...EOC-T_T.KONNO
      IF lv_subrc EQ 0.
*...BOC-T_T.KONNO
*        PERFORM check_terrain_plant USING lwa_mod_row-value
*                                 CHANGING lv_subrc.
        PERFORM check_terrain_plant USING lwa_mod_row-value
                                 CHANGING lt_safras
                                          lv_subrc.
*...EOC-T_T.KONNO

        IF lv_subrc NE 0.
          EXIT.
        ENDIF.
        PERFORM terrain_manual_get USING lwa_mod_row-value
                                CHANGING lwa_item_layout.
        MOVE-CORRESPONDING lwa_item_layout TO ls_fmacitm.
*...BOC-T_T.KONNO
        IF lv_season IS NOT INITIAL.
          lwa_item_layout-season = lv_season.
        ENDIF.
*...EOC-T_T.KONNO
        APPEND ls_fmacitm TO lt_acitm.
*...BOC-T_T.KONNO
*        PERFORM terrain_attribute_get CHANGING lt_acitm.
        PERFORM terrain_attribute_get CHANGING lt_acitm
                                               lt_safras.
*...EOC-T_T.KONNO
        READ TABLE lt_acitm INTO ls_fmacitm INDEX 1.
        IF sy-subrc EQ 0.
          MOVE ls_fmacitm-adqpl TO lwa_item_layout-adqpl.
*...BOC-T_T.KONNO
*          MOVE c_unit_of_measurement-hectare TO lwa_item_layout-unqpl.
          MOVE: ls_fmacitm-advlc TO lwa_item_layout-advlc,
                ls_fmacitm-unqpl TO lwa_item_layout-unqpl.
*...EOC-T_T.KONNO
        ENDIF.
        PERFORM individual_task_material_get USING ls_fmacitm-tplnr_fl
                                                   ls_fmacitm-contr
                                          CHANGING lt_glflcma
                                                   lt_glcsprs
                                                   lt_glcsprso
                                                   lt_mast
                                                   lt_stpo.
        PERFORM volumen_calda_fill USING lt_glflcma
                                         lt_glcsprs
                                         lt_glcsprso
                                         lt_mast
                                         lt_stpo.
        CLEAR:ls_fmacitm.
        REFRESH: lt_acitm.
      ELSE.
*...BOC-T_T.KONNO
*        MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '077'.
        READ TABLE lt_acitm_old INTO DATA(lwa_acitm_old) INDEX 1.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lwa_acitm_old-tplnr_fl
            IMPORTING
              output = lwa_acitm_old-tplnr_fl.
**........O Terreno &1 já foi atribuído à Área de Cultivo &2!
          MESSAGE e092(zfmac) WITH lwa_acitm_old-tplnr_fl lwa_acitm_old-acnum.
        ENDIF.
*...EOC-T_T.KONNO

      ENDIF.
    ENDIF.
    REFRESH: lt_fmaci_check.
    PERFORM terrains_duplicate_chek USING lwa_item_layout
                                    CHANGING lv_subrc.
    IF lv_subrc NE 0.
      EXIT.
    ENDIF.
    MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
    READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                                        WITH KEY acnum = lwa_item_layout-acnum
                                                 acpos = lwa_item_layout-acpos.
    IF sy-subrc EQ 0.
      IF lwa_acitm NE <lwa_acitm>.
        gs_variables-document_changed = c_true.
        IF <lwa_acitm>-updkz NE c_updkz_new.
          MOVE lwa_acitm TO <lwa_acitm>.
          <lwa_acitm>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_acitm TO <lwa_acitm>.
          <lwa_acitm>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE zsc_fmachdr-acnum TO gs_acdoc_infocus-acnum.
      gs_variables-refresh_items_grid = c_true.
      gs_variables-document_changed = c_true.
      PERFORM new_position_set USING lwa_item_layout-acpos.
      lwa_item_layout-acnum = gs_acdoc_infocus-acnum.
      lwa_item_layout-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
      APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
    ENDIF.
    DELETE gt_items_modi INDEX lv_tabix.
  ENDLOOP.

  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_items_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_items_grid.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form INITIAL_LINES_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initial_lines_set .

  DATA: lv_lines       TYPE i,
        lwa_items_fcat TYPE zsc_fmacitm_fcat.

*...BOC-T_T.KONNO
*  IF gs_variables-document_mode NE c_mode_display.
*    DESCRIBE TABLE gt_fmacitm_fcat LINES lv_lines.
*    IF lv_lines GE 10.
*      lv_lines = lv_lines MOD 10.
*      lv_lines = 10 - lv_lines.
*    ELSE.
*      lv_lines = 10 - lv_lines.
*    ENDIF.
*    DO lv_lines TIMES.
*      APPEND INITIAL LINE TO gt_fmacitm_fcat.
*    ENDDO.
*  ENDIF.
  IF gs_variables-document_mode NE c_mode_display.
    DATA(lt_entries) = gt_fmacitm_fcat[].
    DELETE lt_entries WHERE tplnr_fl IS NOT INITIAL.
    DATA(lv_blank_lines) = lines( lt_entries ).
    DATA(lv_add_lines) = 10 - lv_blank_lines.
    DO lv_add_lines TIMES.
      APPEND INITIAL LINE TO gt_fmacitm_fcat.
    ENDDO.
  ENDIF.
*...EOC-T_T.KONNO

ENDFORM.
