
*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display .

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program_acm) IF FOUND.

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
        repid                       = c_program_acm
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

  PERFORM (lv_routine) IN PROGRAM (c_program_acm) IF FOUND.

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
    AND NOT gs_acdoc_infocus IS INITIAL.

  IF gs_variables-manual_changes   EQ c_true
  OR gs_variables-data_changed EQ c_true.
    lv_data_changed = c_true.
  ENDIF.

  IF lv_data_changed IS INITIAL.
    CALL FUNCTION 'ZFMAC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_acdoc  = gs_acdoc_infocus
      IMPORTING
        c_changed = lv_data_changed
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  CHECK lv_data_changed EQ c_true OR sy-datar EQ c_true.

  popup_to_confirm text-010 text-011 c_true lv_answer.

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
        ls_layout TYPE lvc_s_layo,
        lv_input TYPE i,
        lv_title TYPE lvc_title.

  DATA: ls_row_no   TYPE lvc_s_roid,
        ls_col_no   TYPE lvc_s_col,
        ls_row_info TYPE lvc_s_row,
        ls_col_info TYPE lvc_s_col,
        ls_environment TYPE /agri/s_glvc_environment,
        lt_fcat        TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_items IS INITIAL.

    CREATE OBJECT ref_container_items
      EXPORTING
        container_name              = 'SAPLFMACM_0301_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_items IS INITIAL.

    ls_environment-switchoff_performance = c_true.

    CREATE OBJECT ref_grid_items
      EXPORTING
        i_parent           = ref_container_items
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
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.
    ls_layout-sel_mode = 'A'.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM field_catalog_prepare USING c_structure_name-items
                               CHANGING lt_fcat.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_items->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = gt_fmacitm_fcat[]
        it_fieldcatalog               = lt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    PERFORM control_events_register.
    CLEAR gs_variables-refresh_items_grid.

  ELSEIF gs_variables-refresh_items_grid EQ c_true.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_items
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

ENDIF.

  IF gs_variables-document_mode NE c_mode_create.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_items->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0301
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0301.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4 TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4 TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_items->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CALL METHOD ref_grid_items->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_f4_request_items
               FOR ref_grid_items,
               ref_event_handler->on_hotspot_click_items
               FOR ref_grid_items,
               ref_event_handler->on_data_changed_items
               FOR ref_grid_items.

ENDFORM.                    "control_events_register_0301
