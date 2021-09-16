
*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display .

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program_rcm) IF FOUND.

ENDFORM.                    " CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  controls_display_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0100.

  DATA: lv_view.

  IF ref_worklist_container IS INITIAL.

    CREATE OBJECT ref_worklist_container
      EXPORTING
        repid                       = c_program_rcm
        dynnr                       = sy-dynnr
        side                        = cl_gui_docking_container=>dock_at_left
        extension                   = 300
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM worklist_build.

  ENDIF.

  IF gs_variables-refresh_worklist EQ c_true.

    PERFORM worklist_refresh USING lv_view.

    CLEAR gs_variables-refresh_worklist.

  ENDIF.

ENDFORM.                    "controls_display_0100
*&---------------------------------------------------------------------*
*&      Form  CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register .

  DATA : lv_routine(28) VALUE 'CONTROL_EVENTS_REGISTER_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

  PERFORM (lv_routine) IN PROGRAM (c_program_rcm) IF FOUND.

ENDFORM.                    " CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0100 .

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  SET HANDLER: ref_event_handler->on_toolbar_wl
               FOR ref_worklist,
               ref_event_handler->on_user_command_wl
               FOR ref_worklist,
               ref_event_handler->on_hotspot_click_wl
               FOR ref_worklist,
               ref_event_handler->on_view_changed
               FOR ref_worklist.

ENDFORM.                    "control_events_register_0100
*&---------------------------------------------------------------------*
*&      Form  CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM changes_confirm  CHANGING lv_answer.

  DATA: lv_data_changed.

  CHECK gs_variables-document_mode NE c_mode_display
    AND NOT gs_rcdoc_infocus IS INITIAL.

  IF gs_variables-manual_changes   EQ c_true
  OR gs_variables-data_changed EQ c_true.
    lv_data_changed = c_true.
  ENDIF.

  IF lv_data_changed IS INITIAL.
    CALL FUNCTION 'ZFMRC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_rcdoc  = gs_rcdoc_infocus
      CHANGING
        c_changed = lv_data_changed
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  CHECK lv_data_changed EQ c_true OR sy-datar EQ c_true.

  popup_to_confirm TEXT-010 TEXT-011 c_true lv_answer.

  CASE lv_answer.
    WHEN '1'.
      lv_answer = 'A'.
      ok_code   = c_fcode-save.
  ENDCASE.

ENDFORM.                    " CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  controls_display_0202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  controls_display_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0301.

  DATA: ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo,
        lv_input   TYPE i,
        lv_title   TYPE lvc_title.

  DATA: ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_rclst IS INITIAL.

    CREATE OBJECT ref_container_rclst
      EXPORTING
        container_name              = 'SAPLFMRCM_0301_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_rclst IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_rclst
      EXPORTING
        i_parent           = ref_container_rclst
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.


    PERFORM field_catalog_prepare USING c_structure_name-doce
                               CHANGING lt_fcat.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-dose.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_rclst->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_fmrclst_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR gs_variables-refresh_dose_grid.

  ELSEIF gs_variables-refresh_dose_grid EQ c_true.

    CLEAR gs_variables-refresh_dose_grid.

    PERFORM grid_data_get USING ref_grid_rclst
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set  USING ref_grid_rclst
                        CHANGING gt_fmrclst_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.
  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_rclst->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

*...Vistex-11.06.2019/Begin
  gs_fmrchdr_previous = zsc_fmrchdr.
*...Vistex-11.06.2019/End

ENDFORM.                    "controls_display_0301

FORM controls_display_0203.

  DATA: ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo,
        lv_input   TYPE i,
        lv_title   TYPE lvc_title.

  DATA: ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_rcbom IS INITIAL.

    CREATE OBJECT ref_container_rcbom
      EXPORTING
        container_name              = 'SAPLFMRCM_0203_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_rcbom IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_rcbom
      EXPORTING
        i_parent           = ref_container_rcbom
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.


    PERFORM field_catalog_prepare USING c_structure_name-bom
                               CHANGING lt_fcat.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-bom.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_rcbom->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_fmrcbom_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR: gs_variables-refresh_bom_grid.

  ELSEIF gs_variables-refresh_bom_grid EQ c_true.

    CLEAR gs_variables-refresh_bom_grid.

    PERFORM grid_data_get_vlc USING ref_grid_rcbom
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set_vlc  USING ref_grid_rcbom
                        CHANGING gt_fmrcbom_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_rcbom->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0203

*&---------------------------------------------------------------------*
*&      Form  control_events_register_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0301.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4     TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4    TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_rclst->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

***Register the F4 event for grid
  lwa_f4-fieldname = 'MATNR_INS'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_grid_rclst->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

**  IF sy-uname EQ 'T_T.KONNO'.
**    BREAK-POINT.
**  ENDIF.

  SET HANDLER: ref_event_handler->on_f4_request_items
               FOR ref_grid_rclst,
               ref_event_handler->on_hotspot_click_items
               FOR ref_grid_rclst,
               ref_event_handler->on_data_changed_items
               FOR ref_grid_rclst,
               ref_event_handler->on_toolbar_grid
               FOR ref_grid_rclst,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_rclst.

ENDFORM.                    "control_events_register_0301
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0203.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4     TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4    TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_rcbom->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CALL METHOD ref_grid_rcbom->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_f4_request_rcbom
               FOR ref_grid_rcbom,
*               ref_event_handler->on_hotspot_click_items
*               FOR ref_grid_rcbom,
               ref_event_handler->on_data_changed_rcbom
               FOR ref_grid_rcbom,
               ref_event_handler->on_toolbar_grid_rcbom
               FOR ref_grid_rcbom,
               ref_event_handler->on_user_command_grid_rcbom
               FOR ref_grid_rcbom.

ENDFORM.                    "control_events_register_0203
*&---------------------------------------------------------------------*
*& Form CROP_AREA_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM receita_check .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVER_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&---------------------------------------------------------------------*
FORM conver_material_set  USING VALUE(lv_material_in)
                            CHANGING VALUE(lv_material_out).

  material_covert lv_material_in lv_material_out.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form controls_display_0205
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM controls_display_0205 .

  DATA: ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo,
        lv_input   TYPE i,
        lv_title   TYPE lvc_title.

  DATA: ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_rcvrs IS INITIAL.

    CREATE OBJECT ref_container_rcvrs
      EXPORTING
        container_name              = 'SAPLFMRCM_0205_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_rcvrs IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_rcvrs
      EXPORTING
        i_parent           = ref_container_rcvrs
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.


    PERFORM field_catalog_prepare USING c_structure_name-ver
                               CHANGING lt_fcat.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-dose.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_rcvrs->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_fmrcvrs_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR gs_variables-refresh_vrs_grid.

  ELSEIF gs_variables-refresh_vrs_grid EQ c_true.

    CLEAR gs_variables-refresh_vrs_grid.

    PERFORM grid_data_get USING ref_grid_rcvrs
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set  USING ref_grid_rcvrs
                        CHANGING gt_fmrcvrs_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.
  ENDIF.


  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_rcvrs->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.

FORM control_events_register_0205.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4     TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4    TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_rcvrs->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

***Register the F4 event for grid
  lwa_f4-fieldname = 'STLAL'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.
  CLEAR:lwa_f4.

  lwa_f4-fieldname = 'PLNNR'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.
  CLEAR:lwa_f4.

  CALL METHOD ref_grid_rcvrs->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_f4_request_rcvrs
               FOR ref_grid_rcvrs,
               ref_event_handler->on_hotspot_click_rcvrs
               FOR ref_grid_rcvrs,
               ref_event_handler->on_data_changed_rcvrs
               FOR ref_grid_rcvrs,
               ref_event_handler->on_toolbar_grid_rcvrs
               FOR ref_grid_rcvrs,
               ref_event_handler->on_user_command_grid_rcvrs
               FOR ref_grid_rcvrs.

ENDFORM.                    "control_events
*&---------------------------------------------------------------------*
*& Form CALCULATE_BASE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM calculate_base.

  IF zsc_fmrchdr-ausme IS INITIAL.
    SELECT SINGLE ausme
      FROM marc
      INTO zsc_fmrchdr-ausme
     WHERE matnr = zsc_fmrchdr-matnr
       AND werks = zsc_fmrchdr-werks.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CODE_VERS_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_VERSION_LAYOUT_VERID
*&---------------------------------------------------------------------*
FORM code_vers_create  CHANGING VALUE(lv_verid).

  DATA: lv_length TYPE i.
  CONDENSE lv_verid NO-GAPS.
  lv_length = strlen( lv_verid ). "ld_length would return the value 8

  DO ( 4 - lv_length )  TIMES.
    CONCATENATE  '0' lv_verid INTO lv_verid.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CODE_VERS_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_VERID
*&      <-- LV_SUBRC_VERS
*&---------------------------------------------------------------------*
FORM code_vers_check  USING    lv_verid TYPE verid
                      CHANGING lv_subrc TYPE int4.
  DATA: lt_mkal TYPE TABLE OF mkal.
  SELECT * FROM mkal INTO TABLE lt_mkal
                     WHERE matnr = zsc_fmrchdr-matnr
                       AND  werks = zsc_fmrchdr-werks
                  AND  verid = lv_verid.
  MOVE sy-subrc TO lv_subrc.
ENDFORM.
