*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0C
*&---------------------------------------------------------------------*
FORM controls_display .
  DATA: lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program) IF FOUND.
ENDFORM.                    " CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ANSWER  text
*----------------------------------------------------------------------*
FORM changes_confirm  CHANGING lv_answer.

  DATA: lv_document_changed.

  CHECK gs_variables-document_mode NE c_mode_display
    AND NOT gs_acdoc_infocus IS INITIAL.

  IF gs_variables-manual_changes EQ c_true
  OR gs_variables-document_changed EQ c_true.
    lv_document_changed = c_true.
  ENDIF.

  IF lv_document_changed IS INITIAL.
    CALL FUNCTION '/AGRI/FMAC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_acdoc  = gs_acdoc_infocus
      CHANGING
        c_changed = lv_document_changed
      EXCEPTIONS ##FM_SUBRC_OK
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  CHECK lv_document_changed EQ c_true OR sy-datar EQ c_true.

  popup_to_confirm TEXT-013 TEXT-014 c_true lv_answer.

  CASE lv_answer.
    WHEN '1'.
      lv_answer = 'A'.
      ok_code = c_fcode-save.
    WHEN '2'.
*      CLEAR gs_variables-document_changed.
    WHEN 'A'.
      CLEAR gs_variables-document_changed.
      CLEAR lv_answer.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    " CHANGES_CONFIRM
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
        repid                       = c_program
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_events_register .
  DATA: lv_routine(28) VALUE 'CONTROL_EVENTS_REGISTER_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

  PERFORM (lv_routine) IN PROGRAM (c_program) IF FOUND.
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
*&      Form  controls_display_0203
*&---------------------------------------------------------------------*
FORM controls_display_0203.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i.

  IF ref_multi_lang_desc_container IS INITIAL.
    CREATE OBJECT ref_multi_lang_desc_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLFMACM_0203_CC'
*       STYLE                       =
*       LIFETIME                    = lifetime_default
*       REPID                       =
*       DYNNR                       =
*       NO_AUTODEF_PROGID_DYNNR     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  IF ref_multi_lang_desc_grid IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_environment-nocwidth_opt          = c_true.
    ls_layout-sel_mode                   = 'A'.
    ls_layout-stylefname                 = 'STYLES'.

    CREATE OBJECT ref_multi_lang_desc_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_multi_lang_desc_container
*       I_APPL_EVENTS      = space
*       I_PARENTDBG        =
*       I_APPLOGPARENT     =
*       I_GRAPHICSPARENT   =
*       I_USE_VARIANT_CLASS = SPACE
*       I_NAME             =
        is_lvc_environment = ls_environment
*       I_NO_AUTO_DETAILS  =
*       I_NOOUT_FIELDS_HIDE = 'X'
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-ac_multi_lang_desc.
*                                 CHANGING lt_fcat.

    PERFORM control_events_register.

    PERFORM ac_desc_prepare.

    CALL METHOD ref_multi_lang_desc_grid->set_table_for_first_display
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_BYPASSING_BUFFER            =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
*       is_variant                    = ls_variant
*       i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = ls_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
*       it_toolbar_excluding          = lt_toolbar_excludes
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
*       IT_EXCEPT_QINFO               =
      CHANGING
        it_outtab                     = gt_ac_desc_layout
        it_fieldcatalog               = gt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF gs_variables-refresh_grid_desc = c_true.

    PERFORM ac_desc_prepare.
    CLEAR: gs_variables-refresh_grid_desc.

    CALL METHOD ref_multi_lang_desc_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
  ENDIF.

  CALL METHOD ref_multi_lang_desc_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

  gs_variables-refresh_items_grid = c_true.

ENDFORM.                    "controls_display_0301
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0203.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_multi_lang_desc_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  SET HANDLER : ref_event_handler->on_toolbar_grid
                FOR ref_multi_lang_desc_grid,
                ref_event_handler->on_data_changed_grid
                FOR ref_multi_lang_desc_grid,
                ref_event_handler->on_user_command_grid
                FOR ref_multi_lang_desc_grid.

ENDFORM.                    "control_events_register_0203
*&---------------------------------------------------------------------*
*&      Form  controls_display_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0301.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        ls_variant          TYPE disvariant,
        lv_input            TYPE i,
        lc_xfeld            TYPE xfeld.

*  gs_stbl-row = c_true.
  IF ref_container_items IS INITIAL.
    CREATE OBJECT ref_container_items
      EXPORTING
        container_name              = '/AGRI/SAPLFMACM_0301_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  IF ref_grid_items IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_items
      EXPORTING
        i_parent           = ref_container_items
        is_lvc_environment = ls_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-ac_items.
*                                  CHANGING lt_fcat.

    ls_variant-report = c_program.
    ls_variant-handle = c_variant_handle-items.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

    PERFORM control_events_register.
    CALL METHOD ref_grid_items->check_changed_data
      IMPORTING
        e_valid = lc_xfeld.

    CALL METHOD ref_grid_items->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_items_layout
        it_fieldcatalog               = gt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
    ENDIF.

    IF gt_items_layout IS INITIAL.
      PERFORM items_prepare.
      PERFORM refresh_items_grid.
    ENDIF.

    CALL METHOD ref_grid_items->set_toolbar_interactive.

  ELSEIF gs_variables-refresh_items_grid = c_true.
    PERFORM field_catalog_prepare USING c_structure_name-ac_items.
    PERFORM refresh_items_grid.
  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
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

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_grid_items->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CALL METHOD ref_grid_items->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

*  CALL METHOD ref_grid_items->register_delayed_event
*    EXPORTING
*      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_delayed_move_curr_cell.

  REFRESH gt_f4_fields.
  gs_f4_fields-fieldname = 'AUFNR'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.
  gs_f4_fields-fieldname = 'IDRESOURCE'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.
  gs_f4_fields-fieldname = 'EQUNR'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.
  gs_f4_fields-fieldname = 'IDACTVL'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.
  gs_f4_fields-fieldname = 'IDACTVE'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.

  gs_f4_fields-fieldname = 'ZZTPLNR'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.
  gs_f4_fields-fieldname = 'ZZACTRN'.
  gs_f4_fields-register = 'X'.
  INSERT gs_f4_fields INTO TABLE gt_f4_fields.

  CALL METHOD ref_grid_items->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_fields.

  SET HANDLER: ref_event_handler->on_toolbar_grid
               FOR ref_grid_items,
               ref_event_handler->on_data_changed_grid
               FOR ref_grid_items,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_items,
               ref_event_handler->on_handle_f4
               FOR ref_grid_items,
               ref_event_handler->on_hotspot_click
               FOR ref_grid_items.

ENDFORM.                    "control_events_register_0301
*&---------------------------------------------------------------------*
*&      Form  confirmation_infocus_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_POST    text
*      -->LV_SUBRC   text
*----------------------------------------------------------------------*
FORM confirmation_infocus_save  USING lv_post
                                CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.
    PERFORM postings_confirmations CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      ct_acdoc    = lt_acdoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                      INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '053'
                            WITH gs_acdoc_infocus-x-achdr-accom INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " CONFIRMATION_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  closed_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_SUBRC   text
*----------------------------------------------------------------------*
FORM closed_process  CHANGING lv_subrc.
  FIELD-SYMBOLS: <lwa_acitm> TYPE /agri/s_fmacitm.

  gs_acdoc_infocus-x-achdr-status = c_process_status-cls .
  IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
    gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
  ENDIF.
  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>.
    <lwa_acitm>-status = c_process_status-cls.
    IF <lwa_acitm>-updkz NE c_updkz_new.
      <lwa_acitm>-updkz = c_updkz_update.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "closed_process
*&---------------------------------------------------------------------*
*&      Form  deleted_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_SUBRC   text
*----------------------------------------------------------------------*
FORM deleted_process  CHANGING lv_subrc.
  FIELD-SYMBOLS: <lwa_acitm> TYPE /agri/s_fmacitm.

  IF gs_acdoc_infocus-x-achdr-status NE c_process_status-ctd.

    lv_subrc = 4.

  ENDIF.

  CHECK lv_subrc IS INITIAL.
*  CASE gs_acdoc_infocus-x-achdr-status.
*    WHEN  c_process_status-ctd.
*      READ TABLE gs_acdoc_infocus-x-acitm
*                        WITH KEY status = c_process_status-ctd
*                        TRANSPORTING NO FIELDS.
*
*      CHECK sy-subrc NE 0.
*      lv_subrc = 0.
*
*    WHEN c_process_status-cnf.
*      READ TABLE gs_acdoc_infocus-x-acitm
*                        WITH KEY status = c_process_status-cnf
*                        TRANSPORTING NO FIELDS.
*
*      lv_subrc = sy-subrc.
*      CHECK lv_subrc NE 0.
*      lv_subrc = 0.
*
*    WHEN c_process_status-dis.
*
*      LOOP AT gs_acdoc_infocus-x-acitm TRANSPORTING NO FIELDS
*                                       WHERE status = c_process_status-ctd
*                                          OR status = c_process_status-cnf.
*        EXIT.
*      ENDLOOP.
*
*      CHECK sy-subrc NE 0.
*      lv_subrc = 0.
*  ENDCASE.

  gs_acdoc_infocus-x-achdr-status = c_process_status-del .
  IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
    gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
  ENDIF.
  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>.
    <lwa_acitm>-status = c_process_status-del.
    IF <lwa_acitm>-updkz NE c_updkz_new.
      <lwa_acitm>-updkz = c_updkz_update.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "deleted_process
*&---------------------------------------------------------------------*
*&      Form  closed_infocus_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_POST    text
*      -->LV_SUBRC   text
*----------------------------------------------------------------------*
FORM closed_infocus_save   USING lv_post
                           CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.
    PERFORM closed_process CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      ct_acdoc    = lt_acdoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                      INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '058'
                            WITH gs_acdoc_infocus-x-achdr-accom INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    "closed_infocus_save
*&---------------------------------------------------------------------*
*&      Form  delete_infocus_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_POST    text
*      -->LV_SUBRC   text
*----------------------------------------------------------------------*
FORM deleted_infocus_save   USING lv_post
                           CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.
    PERFORM deleted_process CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                     INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      ct_acdoc    = lt_acdoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                      INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '057'
                            WITH gs_acdoc_infocus-x-achdr-accom INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    "deleted_infocus_save
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0307
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0307.

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF NOT ref_grid_additional_data IS INITIAL.
    CALL METHOD ref_grid_additional_data->register_edit_event
      EXPORTING
        i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

****Register f4 For Fields.
    lwa_f4-fieldname = 'FIELDVAL'.
    lwa_f4-register = c_true.
****ESP6 Task#34370 - ATC correction (Post 1709 Corrections)
*    APPEND lwa_f4 TO lt_f4.
    INSERT  lwa_f4 INTO TABLE lt_f4.
****
    CALL METHOD ref_grid_additional_data->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: ref_event_handler->on_data_changed_grid
                 FOR ref_grid_additional_data,
                 ref_event_handler->on_value_request
                 FOR ref_grid_additional_data.
  ENDIF.

ENDFORM.                    "control_events_register_0307
*&---------------------------------------------------------------------*
*&      Form  CONFIRMATIONS_MAINTAIN
*&---------------------------------------------------------------------*
FORM confirmations_maintain.

  DATA : lt_terrain      TYPE STANDARD TABLE OF /agri/glflcma,
         lwa_terrain     TYPE /agri/glflcma,
         lt_order        TYPE STANDARD TABLE OF /agri/fmfphdr,
         lwa_order       TYPE /agri/fmfphdr,
         lwa_details_new TYPE ty_details,
         lwa_acm_ord     TYPE zabs_acm_ord,
         lt_acm_ord      TYPE STANDARD TABLE OF zabs_acm_ord.

*get the terrain & counter details.

* while creation only.

  IF gs_acdoc_infocus-updkz EQ 'I'.

    REFRESH gt_details.

    IF p_strdat IS INITIAL.
      p_strdat = /agri/s_fmachdr-strtdat.
    ENDIF.

    IF p_findat IS  INITIAL.
      p_findat = /agri/s_fmachdr-findat.
    ENDIF.

    SELECT tplnr_fl contr
      FROM /agri/glflcma
      INTO CORRESPONDING FIELDS OF TABLE lt_terrain
     WHERE tplnr_fl IN so_tplnr
       AND datab <= p_strdat
       AND datbi >= p_findat
       AND astat = 'A'
       AND bukrs = p_bukrs
       AND iwerk = p_werks
       AND loevm = space.
    IF sy-subrc = 0.
      SELECT aufnr auart tplnr_fl contr matnr gamng gmein
        FROM /agri/fmfphdr INTO CORRESPONDING FIELDS OF TABLE lt_order
         FOR ALL ENTRIES IN lt_terrain
       WHERE aufnr IN so_aufnr
         AND autyp EQ 'TO'
         AND tplnr_fl = lt_terrain-tplnr_fl
         AND contr    = lt_terrain-contr
         AND matnr    = p_mat
         AND tecom    = space.
    ENDIF.

    LOOP AT lt_order INTO lwa_order.
      CLEAR lwa_details_new.
      MOVE-CORRESPONDING lwa_order TO lwa_details_new.
      lwa_details_new-meins = lwa_order-gmein.
      lwa_details_new-menge = lwa_order-gamng.

      CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_OUTPUT'
        EXPORTING
          i_input  = lwa_order-tplnr_fl
        IMPORTING
          o_output = lwa_details_new-tplnr_fl.

      APPEND lwa_details_new TO gt_details.
      CLEAR : lwa_details_new.
    ENDLOOP.

*  ELSE.
*
*    CALL FUNCTION 'ZABS_FM_ORD_GET'
*      EXPORTING
*        iv_accom  = gs_acdoc_infocus-accom
*      TABLES
*        t_acm_ord = lt_acm_ord.
*
*    LOOP AT lt_acm_ord INTO lwa_acm_ord.
*      CLEAR lwa_details_new.
*      MOVE-CORRESPONDING lwa_acm_ord TO lwa_details_new.
*      APPEND lwa_details_new TO gt_details.
*    ENDLOOP.

  ENDIF.

  gs_variables-refresh_postings_grid = c_true.

  RETURN.

  DATA: lt_mseg     TYPE TABLE OF /agri/s_fmac_mseg,
        lt_fmprdoc  TYPE /agri/t_fmac_pr,
        lwa_fmprdoc TYPE /agri/s_fmac_pr,
        lt_fp       TYPE /agri/t_fmac_fp,
        lwa_fp      TYPE /agri/s_fmac_fp,
        lv_mode     LIKE t180-trtyp,
        lwa_woitm   TYPE /agri/s_fmwoitm,
        lt_ocnum    TYPE /agri/t_fmocnum,
        lwa_details TYPE ty_details,
        lv_details  TYPE xfld,
        lr_bwart    TYPE RANGE OF bwart,
        lwa_bwart   LIKE LINE OF lr_bwart,
        lr_budat    TYPE RANGE OF budat,
        lwa_budat   LIKE LINE OF lr_budat,
        lt_actar    TYPE TABLE OF /agri/tfmactar,
        lt_mara     TYPE TABLE OF mara,
        lwa_mara    TYPE mara.

  FIELD-SYMBOLS: <lwa_details_tmp> TYPE /agri/s_fmoc_details,
                 <lwa_mseg>        TYPE /agri/s_fmac_mseg.

*  CHECK gs_acdoc_infocus-x-achdr-wonum IS NOT INITIAL.

  lwa_bwart-option = c_operator_word-between.
  lwa_bwart-sign   = c_sign-include.
  lwa_bwart-low    = c_movements-mov_101.
  lwa_bwart-high   = c_movements-mov_102.
  APPEND lwa_bwart TO lr_bwart.

  lwa_budat-option = c_operator_word-between.
  lwa_budat-sign   = c_sign-include.
  lwa_budat-low    = gs_acdoc_infocus-x-achdr-strtdat.
  lwa_budat-high   = gs_acdoc_infocus-x-achdr-findat.
  APPEND lwa_budat TO lr_budat.

  PERFORM ocnum_get CHANGING lt_ocnum
                             lt_fmprdoc.
  CHECK lt_ocnum IS NOT INITIAL.

  FREE: gt_details,
        gt_details_fcat.
  lv_details = c_true.
  PERFORM confirmations_data_read IN PROGRAM /agri/saplfmocm
                                USING lv_details
                                      lt_ocnum
                               CHANGING gt_details_tmp.

  DELETE gt_details_tmp WHERE bwart      NOT IN lr_bwart.
*                           OR budat_mkpf NOT IN lr_budat .

  IF gt_details_tmp IS NOT INITIAL.
    SELECT mblnr aufnr bwart smbln matbf
      FROM mseg
      INTO TABLE lt_mseg
      FOR ALL ENTRIES IN gt_details_tmp
      WHERE aufnr EQ gt_details_tmp-aufnr
        AND bwart EQ '102'.

    IF lt_mseg IS NOT INITIAL.
      LOOP AT lt_mseg ASSIGNING <lwa_mseg>.
        DELETE gt_details_tmp WHERE aufnr EQ <lwa_mseg>-aufnr
                                AND mblnr EQ <lwa_mseg>-smbln.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF gt_details_tmp IS NOT INITIAL.

    SELECT aufnr tplnr_fl cmnum varia cpros matnr
    FROM /agri/fmfphdr
    INTO TABLE lt_fp
    FOR ALL ENTRIES IN gt_details_tmp
    WHERE aufnr = gt_details_tmp-aufnr.

***validate the configuration material
***15/07/2016
    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN gt_details_tmp
    WHERE matnr = gt_details_tmp-matnr.                 "#EC CI_NOORDER

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_actar
    FROM /agri/tfmactar
    WHERE actyp =  gs_acdoc_infocus-x-achdr-actyp.

    IF lt_actar IS NOT INITIAL.

      LOOP AT gt_details_tmp ASSIGNING <lwa_details_tmp>.

        READ TABLE lt_mara INTO lwa_mara WITH KEY matnr = <lwa_details_tmp>-matnr. "#EC CI_ALL_FIELDS_NEEDED
        IF sy-subrc EQ 0.
          READ TABLE lt_actar TRANSPORTING NO FIELDS WITH KEY mtart = lwa_mara-mtart. "#EC CI_ALL_FIELDS_NEEDED
          IF sy-subrc EQ 0.
            READ TABLE lt_fp INTO lwa_fp
                       WITH KEY aufnr = <lwa_details_tmp>-aufnr.
            CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_OUTPUT'
              EXPORTING
                i_input  = lwa_fp-tplnr_fl
              IMPORTING
                o_output = lwa_details-tplnr_fl.
            MOVE-CORRESPONDING <lwa_details_tmp> TO lwa_details.
            IF gs_tfmactyp-acapp EQ c_accom_appli-prnum.
              READ TABLE lt_fmprdoc INTO lwa_fmprdoc
                        WITH KEY pdokar1 = <lwa_details_tmp>-mblnr
                        BINARY SEARCH.
              IF sy-subrc EQ 0.
****16/08/2016
                lwa_details-budat_mkpf = lwa_fmprdoc-budat.
                lwa_details-prnum = lwa_fmprdoc-prnum.
                APPEND lwa_details TO gt_details.
              ENDIF.
            ELSE.
              APPEND lwa_details TO gt_details.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
****
  gs_variables-refresh_postings_grid = c_true.
ENDFORM.                    " CONFIRMATIONS_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  controls_display_0302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0302.

  DATA : lt_fcat             TYPE lvc_t_fcat,
         ls_variant          TYPE disvariant,
         ls_layout           TYPE lvc_s_layo,
         lt_toolbar_excludes TYPE ui_functions,
         lv_input            TYPE i,
         ls_environment      TYPE /agri/s_glvc_environment.

  IF  ref_details_container IS INITIAL.
    CREATE OBJECT ref_details_container
      EXPORTING
        container_name              = '/AGRI/SAPLFMOCM_0101_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.


  IF ref_grid_details IS INITIAL.

    ls_environment-switchoff_performance = c_true.

    CREATE OBJECT ref_grid_details
      EXPORTING
        i_parent           = ref_details_container
        is_lvc_environment = ls_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-details.
*                             CHANGING lt_fcat.

    ls_layout-sel_mode   = 'A'.
*    ls_layout-stylefname = 'STYLES'.
*    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-cwidth_opt = c_true.
    ls_variant-report    = c_program.
    ls_variant-handle = c_variant_handle-details.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_details->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_details_fcat
        it_fieldcatalog               = gt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
    ENDIF.

    PERFORM control_events_register.

    CALL METHOD ref_grid_details->set_toolbar_interactive.
    CLEAR gs_variables-refresh_postings_grid.
****Once Grid is refreshed
  ELSEIF gs_variables-refresh_postings_grid = c_true.
    CLEAR gs_variables-refresh_postings_grid.

    PERFORM refresh_details_grid.

  ENDIF.

ENDFORM.                    "controls_display_0302
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0302.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  SET HANDLER: ref_event_handler->on_toolbar_grid
               FOR ref_grid_details,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_details,
               ref_event_handler->on_data_changed_grid
               FOR ref_grid_details,
               ref_event_handler->on_hotspot_click
               FOR ref_grid_details.

ENDFORM.                    "control_events_register_0302
*&---------------------------------------------------------------------*
*&      Form  controls_display_0307
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0307.

  DATA: ls_layout         TYPE lvc_s_layo,
        ls_variant        TYPE disvariant,
        lv_structure_name LIKE dd03l-fieldname.
  DATA: lv_row_no TYPE lvc_s_row,
        lv_col_id TYPE lvc_s_col.

  CHECK NOT gt_additional_data[] IS INITIAL.

  IF ref_grid_additional_data IS INITIAL.

    CREATE OBJECT ref_additional_data_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLFMACM_0307_CC'
*       STYLE                       =
*       LIFETIME                    = lifetime_default
*       REPID                       =
*       DYNNR                       =
*       NO_AUTODEF_PROGID_DYNNR     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

    CREATE OBJECT ref_grid_additional_data
      EXPORTING
*       I_SHELLSTYLE      = 0
*       I_LIFETIME        =
        i_parent          = ref_additional_data_container
*       I_APPL_EVENTS     = space
*       I_PARENTDBG       =
*       I_APPLOGPARENT    =
*       I_GRAPHICSPARENT  =
*       I_USE_VARIANT_CLASS = SPACE
*       I_NAME            =
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CHECK sy-subrc EQ 0.

    PERFORM field_catalog_prepare USING '/AGRI/S_ABGL_USER_SCRFIELDS'.
*                               CHANGING lt_fcat.
    PERFORM field_value_conversions USING '1'.

****Prepare ALV Grid Layout
    ls_layout-cwidth_opt = c_true.
    ls_layout-no_toolbar = c_true.
    ls_layout-no_rowmark = c_true.

    ls_variant-report = c_program.
    ls_variant-handle = c_variant_handle-ac_additional_data.

    CALL METHOD ref_grid_additional_data->set_table_for_first_display
      EXPORTING
*       I_BYPASSING_BUFFER            =
*       I_BUFFER_ACTIVE               =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
        is_variant                    = ls_variant
        i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = ls_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
*       IT_TOOLBAR_EXCLUDING          =
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
*       IT_EXCEPT_QINFO               =
      CHANGING
        it_outtab                     = gt_additional_data[]
        it_fieldcatalog               = gt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    PERFORM control_events_register.

    CLEAR gs_variables-refresh_additional_data_grid.
  ELSEIF NOT gs_variables-refresh_additional_data_grid IS INITIAL.

    CLEAR gs_variables-refresh_additional_data_grid.
    PERFORM field_value_conversions USING '1'.

    CALL METHOD ref_grid_additional_data->get_current_cell
      IMPORTING
        es_row_id = lv_row_no
        es_col_id = lv_col_id.
    CALL METHOD ref_grid_additional_data->refresh_table_display.
    CALL METHOD ref_grid_additional_data->set_current_cell_via_id
      EXPORTING
        is_row_id    = lv_row_no
        is_column_id = lv_col_id.
  ENDIF.

  IF gs_variables-overview_mode EQ c_mode_display OR
     gs_acdoc_infocus IS INITIAL.

    CALL METHOD ref_grid_additional_data->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ELSE.
    CALL METHOD ref_grid_additional_data->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ENDIF.

ENDFORM.                    "controls_display_0307
*&---------------------------------------------------------------------*
*&      Form  CONF_TIME_VALIDATE
*&---------------------------------------------------------------------*
FORM conf_time_validate CHANGING lv_subrc.

  DATA: lt_fmacitm    TYPE /agri/t_fmacitm,
        lt_fmacitm2   TYPE /agri/t_fmacitm,
        lt_fmacitmtmp TYPE /agri/t_fmacitm,
        lt_daytim     TYPE /agri/t_fmac_dur_daytime,
        lv_tim        TYPE t,
        lv_tim2       TYPE t,
        lv_dattmp     TYPE d,
        lv_day        TYPE d,
        lv_timadd     TYPE c LENGTH 11,
        lv_timstr     TYPE c LENGTH 11,
        lv_timadd2    TYPE t,
        lv_timstr2    TYPE t,
        lv_target     TYPE c,
        lv_resource   TYPE /agri/fmidrsc,
        lv_name       TYPE c LENGTH 9,
        lv_length     TYPE i,
        lv_subrctmp   TYPE sysubrc,
        lr_budat      TYPE RANGE OF budat,
        lwa_budat     LIKE LINE OF lr_budat.

  FIELD-SYMBOLS: <lwa_fmacitml>   TYPE /agri/s_fmacitm_layout,
                 <lwa_fmacact>    TYPE /agri/fmacact,
                 <lwa_fmacitm>    TYPE /agri/s_fmacitm,
                 <lwa_fmacitmtmp> TYPE /agri/s_fmacitm,
                 <lwa_daytim>     TYPE /agri/s_fmac_dur_daytime,
                 <lwa_items_tmp>  TYPE /agri/s_fmacitm_layout.

  lt_fmacitmtmp = gs_acdoc_infocus-x-acitm.

  LOOP AT lt_fmacitmtmp ASSIGNING <lwa_fmacitmtmp> WHERE zzconfm IS INITIAL.
    DATA(lv_tabix1) = sy-tabix.
    lt_fmacitm = lt_fmacitmtmp.
    lwa_budat-option = c_operator_word-between.
    lwa_budat-sign   = c_sign-include.
    lwa_budat-low    = <lwa_fmacitmtmp>-strtdat.
    lwa_budat-high   = <lwa_fmacitmtmp>-findat.
    APPEND lwa_budat TO lr_budat.

    DELETE lt_fmacitm WHERE strtdat NOT IN lr_budat
                        AND findat  NOT IN lr_budat.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      IF lv_target IS INITIAL.
        lt_fmacitm2 = lt_fmacitm.
        DELETE lt_fmacitm2 WHERE idresource IS INITIAL
                              OR idresource
                              NE <lwa_fmacitmtmp>-idresource.
        lv_target = c_true.
        lv_name   = 'Employee'(016).
        CLEAR: lv_tim.
        REFRESH lt_daytim.
      ELSE.
        lt_fmacitm2 = lt_fmacitm.
        DELETE lt_fmacitm2 WHERE equnr IS INITIAL
                              OR equnr
                              NE <lwa_fmacitmtmp>-equnr.
        lv_name = 'Equipment'(017).
        CLEAR: lv_target, lv_tim, lv_subrctmp.
        REFRESH lt_daytim.
      ENDIF.
      LOOP AT lt_fmacitm2 ASSIGNING <lwa_fmacitm>.
        DATA(lv_tabix2) = sy-tabix.
        CLEAR: lv_timadd, lv_tim2.
        IF lv_target IS NOT INITIAL.
          lv_resource = <lwa_fmacitm>-idresource.
        ELSE.
          lv_resource = <lwa_fmacitm>-equnr.
        ENDIF.
        CHECK <lwa_fmacitm> IS ASSIGNED.
        CALL FUNCTION '/AGRI/FMAC_CALC_HR_PR_DAY'
          EXPORTING
            i_date1          = <lwa_fmacitm>-strtdat
            i_time1          = <lwa_fmacitm>-strttim
            i_date2          = <lwa_fmacitm>-findat
            i_time2          = <lwa_fmacitm>-fintim
          IMPORTING
            e_tim_tab        = lt_daytim
          EXCEPTIONS
            invalid_datetime = 1
            days_over_limit  = 2
            OTHERS           = 3.
        IF sy-subrc EQ 0.
          READ TABLE lt_daytim ASSIGNING <lwa_daytim>
                          WITH KEY day = <lwa_fmacitmtmp>-strtdat.
          IF sy-subrc EQ 0.
            lv_day = <lwa_daytim>-day.
          ELSE.
            READ TABLE lt_daytim ASSIGNING <lwa_daytim>
                        WITH KEY day = <lwa_fmacitmtmp>-findat.
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
          CONCATENATE '0' lv_timadd(1) lv_timadd+2(2) '00' INTO lv_timadd2.
        ELSEIF lv_length EQ 3.
          CONCATENATE '00' lv_timadd+1(2) '00' INTO lv_timadd2.
        ELSE.
          CONCATENATE lv_timadd(2) lv_timadd+3(2) '00' INTO lv_timadd2.
        ENDIF.
        CLEAR lv_dattmp.
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
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '003'
                          WITH lv_day lv_name lv_resource
                          INTO sy-msgli.
          lv_subrctmp = lv_subrc = 4.
          message_simple space.
          gs_variables-errors = c_true.
        ENDIF.
        REFRESH lt_daytim.
      ENDLOOP.
    ENDDO.
    DELETE lt_fmacitmtmp WHERE idresource = <lwa_fmacitmtmp>-idresource
                                OR equnr   = <lwa_fmacitmtmp>-equnr
                               AND strtdat NOT IN lr_budat
                               AND findat  NOT IN lr_budat.
  ENDLOOP.

ENDFORM.                    " CONF_TIME_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  CATALOG_LINE_STYLE_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ARBPL  text
*      -->P_LWA_MOD_ROW  text
*----------------------------------------------------------------------*
FORM catalog_line_style_change  CHANGING lwa_item_layout  TYPE /agri/s_fmacitm_layout.


  DATA: lt_vgwts      TYPE /agri/t_vgwts,
        lwa_vgwts     TYPE /agri/s_vgwts,
        lt_parameters TYPE /agri/t_parameters_tc21,
        lt_resource   TYPE /agri/t_fmac_src_res,
        lwa_resource  TYPE /agri/s_fmac_src_res,
        lwa_tc20      TYPE tc20,
        lwa_fmacrsc   TYPE /agri/s_fmacrsc,
        lt_tc20       TYPE /agri/t_parameters_tc20,
        lt_fmacrsc    TYPE /agri/t_fmacrsc,
        ls_fmacrsc    TYPE /agri/s_fmacrsc,
        lwa_fmachdr   TYPE /agri/s_fmachdr,
        lwa_style     TYPE lvc_s_styl,
        lv_data       TYPE i,
        lt_arbpl      TYPE /agri/t_range_arbpl.

  FIELD-SYMBOLS: <lwa_fmacitm> TYPE /agri/s_fmacitm_layout,
                 <lwa_style>   TYPE lvc_s_styl.

  IF lwa_item_layout-status = c_process_status-ctd.
    PERFORM wonum_infocus_read USING /agri/s_fmachdr-wonum
                                     lwa_item_layout-aufnr
                               CHANGING lt_arbpl.
  ENDIF.

  CHECK lt_arbpl IS NOT INITIAL.

  SELECT arbpl vgwts
     FROM crhd
     INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
     WHERE arbpl IN lt_arbpl.

  DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED "#EC CI_FAE_LINES_ENSURED

  IF lt_vgwts IS NOT INITIAL.

    SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
    INTO CORRESPONDING FIELDS OF TABLE lt_parameters
    FROM tc21
    FOR ALL ENTRIES IN lt_vgwts
    WHERE vgwts = lt_vgwts-vgwts.             "#EC CI_FAE_LINES_ENSURED

  ENDIF.

  IF  lt_parameters IS NOT INITIAL.
    SELECT parid                              "#EC CI_FAE_LINES_ENSURED
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

  READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom. "#EC CI_FAE_LINES_ENSURED

  IF lt_tc20 IS NOT INITIAL .

    SELECT actyp parid rstyp txtlg            "#EC CI_FAE_LINES_ENSURED
    FROM /agri/tfmacrsc
    INTO TABLE lt_fmacrsc
    FOR ALL ENTRIES IN lt_tc20
    WHERE parid = lt_tc20-parid                         "#EC CI_NOORDER
    AND actyp = lwa_fmachdr-actyp.

  ENDIF.
  IF sy-subrc = 0.
    IF lwa_item_layout-status = c_process_status-ctd.

      IF lwa_item_layout-equnr IS INITIAL AND lwa_item_layout-idresource IS INITIAL.

        IF lwa_item_layout-arbpl IS NOT INITIAL.
          CLEAR: lwa_item_layout-arbpl.
        ENDIF.

        DESCRIBE TABLE lt_fmacrsc LINES lv_data.
        IF lv_data > 1.

          LOOP AT lt_fmacrsc INTO ls_fmacrsc.

            CASE ls_fmacrsc-rstyp.
              WHEN  c_accom_id-equipment.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'EQUNR'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_enabled.
                ELSE.
                  lwa_style-fieldname = 'EQUNR'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.


              WHEN  c_accom_id-employee.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'IDRESOURCE'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_enabled.
                ELSE.
                  lwa_style-fieldname = 'IDRESOURCE'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.

              WHEN OTHERS.
            ENDCASE.

          ENDLOOP.

        ELSE.

          READ TABLE lt_fmacrsc INTO ls_fmacrsc INDEX 1. "#EC CI_NOORDER
          IF sy-subrc EQ 0.

            CASE ls_fmacrsc-rstyp.
              WHEN  c_accom_id-equipment.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'EQUNR'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_enabled.
                ELSE.
                  lwa_style-fieldname = 'EQUNR'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'IDRESOURCE'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_disabled.
                ELSE.
                  lwa_style-fieldname = 'IDRESOURCE'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.


              WHEN  c_accom_id-employee.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'IDRESOURCE'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_enabled.
                ELSE.
                  lwa_style-fieldname = 'IDRESOURCE'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.
                READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = 'EQUNR'.
                IF sy-subrc EQ 0.
                  <lwa_style>-style = cl_gui_alv_grid=>mc_style_disabled.
                ELSE.
                  lwa_style-fieldname = 'EQUNR'.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                  INSERT lwa_style INTO TABLE lwa_item_layout-styles.
                ENDIF.

              WHEN OTHERS.
            ENDCASE.

          ENDIF.



        ENDIF.

      ELSEIF lwa_item_layout-equnr IS NOT INITIAL
        AND lwa_item_layout-idresource IS INITIAL.

        PERFORM camp_validate_change  USING  c_rstype-equnr
                                             lt_arbpl
                                             lt_vgwts
                                             lt_parameters
                                             lt_tc20
                                             lt_fmacrsc
                                    CHANGING lwa_item_layout.


      ELSEIF lwa_item_layout-equnr IS INITIAL
        AND lwa_item_layout-idresource IS NOT INITIAL.

        PERFORM camp_validate_change  USING  c_rstype-labor
                                             lt_arbpl
                                             lt_vgwts
                                             lt_parameters
                                             lt_tc20
                                             lt_fmacrsc
                                    CHANGING lwa_item_layout.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " CATALOG_LINE_STYLE_CHANGE
*&---------------------------------------------------------------------*
*&      Form  CAMP_VALIDATE_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_RSTYPE_LABOR  text
*----------------------------------------------------------------------*
FORM camp_validate_change  USING  lv_rstype
                                  lt_arbpl      TYPE /agri/t_range_arbpl
                                  lt_vgwts      TYPE /agri/t_vgwts
                                  lt_parameters TYPE /agri/t_parameters_tc21
                                  lt_tc20       TYPE /agri/t_parameters_tc20
                                  lt_fmacrsc    TYPE /agri/t_fmacrsc
                         CHANGING lwa_item_layout TYPE /agri/s_fmacitm_layout.

  DATA: lv_rstyp       TYPE /agri/fmacrstyp,
        lwa_style      TYPE lvc_s_styl,
        lwa_vgwts      TYPE /agri/s_vgwts,
        lv_fname1      TYPE lvc_fname,
        lt_resource    TYPE /agri/t_fmac_src_res,
        lwa_parameters TYPE /agri/s_parameters_tc21,
        lwa_resource   TYPE /agri/s_fmac_src_res,
        lwa_tc20       TYPE /agri/s_parameters_tc20,
        lwa_fmacrsc    TYPE /agri/s_fmacrsc.
  FIELD-SYMBOLS: <lwa_style>       TYPE lvc_s_styl.

  SELECT idresource description arbpl intext lifnr
    INTO TABLE lt_resource
    FROM /agri/fmacres
    WHERE rstype = lv_rstype
    AND arbpl IN lt_arbpl.

  CASE lv_rstype.
    WHEN c_rstype-labor.
      READ TABLE lt_resource INTO lwa_resource WITH KEY idresource = lwa_item_layout-idresource.
      CHECK sy-subrc EQ 0.
      lv_rstyp =  c_accom_id-equipment.
      lv_fname1 = 'EQUNR'.
    WHEN c_rstype-equnr.
      READ TABLE lt_resource INTO lwa_resource WITH KEY idresource  = lwa_item_layout-equnr.
      CHECK sy-subrc EQ 0.
      lv_rstyp =  c_accom_id-employee.
      lv_fname1 = 'IDRESOURCE'.
    WHEN OTHERS.
  ENDCASE.

  READ TABLE lt_vgwts INTO lwa_vgwts WITH KEY arbpl = lwa_resource-arbpl.
  CHECK sy-subrc EQ 0.
  READ TABLE lt_parameters INTO lwa_parameters WITH KEY vgwts = lwa_vgwts-vgwts.

*  LOOP AT lt_tc20 INTO lwa_tc20 WHERE parid = lwa_parameters-par01 OR
*                                      parid = lwa_parameters-par02 OR
*                                      parid = lwa_parameters-par03 OR
*                                      parid = lwa_parameters-par04 OR
*                                      parid = lwa_parameters-par05 OR
*                                      parid = lwa_parameters-par06.
*
*    READ TABLE lt_fmacrsc TRANSPORTING NO FIELDS WITH KEY parid = lwa_tc20-parid
*                                                          rstyp = lv_rstyp.
*    IF sy-subrc EQ 0.
*      lwa_style-fieldname = lv_fname1.
*      lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*      INSERT lwa_style INTO TABLE lwa_item_layout-styles.
*      DELETE ADJACENT DUPLICATES FROM lwa_item_layout-styles COMPARING fieldname.
*    ELSE.
*      lwa_style-fieldname = lv_fname1.
*      lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*      INSERT lwa_style INTO TABLE lwa_item_layout-styles.
*      DELETE ADJACENT DUPLICATES FROM lwa_item_layout-styles COMPARING fieldname.
*    ENDIF.
*
*  ENDLOOP.
  LOOP AT lt_fmacrsc TRANSPORTING NO FIELDS WHERE ( parid = lwa_parameters-par01 OR
                                        parid = lwa_parameters-par02 OR
                                        parid = lwa_parameters-par03 OR
                                        parid = lwa_parameters-par04 OR
                                        parid = lwa_parameters-par05 OR
                                        parid = lwa_parameters-par06 ) AND
                                        rstyp = lv_rstyp.


  ENDLOOP.
  IF sy-subrc EQ 0.
    READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = lv_fname1.
    IF sy-subrc EQ 0.
      <lwa_style>-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      lwa_style-fieldname = lv_fname1.
      lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT lwa_style INTO TABLE lwa_item_layout-styles.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM lwa_item_layout-styles COMPARING fieldname.
  ELSE.
    READ TABLE lwa_item_layout-styles ASSIGNING <lwa_style> WITH KEY fieldname = lv_fname1.
    IF sy-subrc EQ 0.
      <lwa_style>-style = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      lwa_style-fieldname = lv_fname1.
      lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT lwa_style INTO TABLE lwa_item_layout-styles.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM lwa_item_layout-styles COMPARING fieldname.

  ENDIF.

ENDFORM.                    " CAMP_VALIDATE_CHANGE
*&---------------------------------------------------------------------*
*&      Form  CONFIRMATION_PARAMETER_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LWA_ACITM>  text
*----------------------------------------------------------------------*
FORM confirmation_parameter_change     USING ls_acitm    TYPE /agri/s_fmacitm
                                    CHANGING lwa_fmfpcnf TYPE /agri/s_fmfp_cnf.

  DATA: ls_activity    TYPE /agri/s_fmac_src_act,
        lt_activity    TYPE /agri/t_fmac_src_act,
        ls_resource    TYPE /agri/s_fmac_src_res,
        ls_equnr       TYPE /agri/s_fmac_src_res,
        ls_afvc        TYPE /agri/s_fmac_src_ord,
        lv_aufpl       TYPE co_aufpl,
        lt_vgwts       TYPE /agri/t_vgwts,
        ls_vgwts       TYPE /agri/s_vgwts,
        lwa_vgwts      TYPE /agri/s_vgwts,
        lt_parameters  TYPE /agri/t_parameters_tc21,
        ls_parameters  TYPE /agri/s_parameters_tc21,
        lwa_parameters TYPE /agri/s_parameters_tc21,
        lt_tc20        TYPE /agri/t_parameters_tc20,
        lwa_tc20       TYPE tc20,
        lwa_fmachdr    TYPE /agri/s_fmachdr,
        lt_fmacrsc     TYPE /agri/t_fmacrsc,
        lwa_fmacrsc    TYPE /agri/s_fmacrsc.

  SELECT SINGLE arbpl vgwts
       FROM crhd
       INTO ls_vgwts
       WHERE arbpl = ls_acitm-arbpl.                    "#EC CI_NOORDER

  SELECT SINGLE aufpl
  FROM afko
  INTO lv_aufpl
  WHERE aufnr = ls_acitm-aufnr.

  SELECT SINGLE vgwts lar01 lar02 lar03 lar04 lar05 lar06
    INTO ls_afvc
    FROM afvc
    WHERE aufpl = lv_aufpl                              "#EC CI_NOORDER
    AND vgwts = ls_vgwts-vgwts.

  SELECT SINGLE vgwts par01 par02 par03 par04 par05 par06
    INTO ls_parameters
    FROM tc21
    WHERE vgwts = ls_vgwts-vgwts.

  SELECT parid                                "#EC CI_FAE_LINES_ENSURED
  INTO CORRESPONDING FIELDS OF TABLE lt_tc20
  FROM tc20
  WHERE parid = ls_parameters-par01
    OR  parid = ls_parameters-par02
    OR  parid = ls_parameters-par03
    OR  parid = ls_parameters-par04
    OR  parid = ls_parameters-par05
    OR  parid = ls_parameters-par06.          "#EC CI_FAE_LINES_ENSURED

  IF lt_tc20 IS NOT INITIAL.

    SELECT actyp parid rstyp txtlg            "#EC CI_FAE_LINES_ENSURED
      FROM /agri/tfmacrsc
      INTO TABLE lt_fmacrsc
       FOR ALL ENTRIES IN lt_tc20
     WHERE parid = lt_tc20-parid.

  ENDIF.
  IF ls_acitm-idactvl IS NOT INITIAL.

    SELECT SINGLE t1~idactv t1~rstype t1~bill t1~actype t2~description
      INTO ls_activity
      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
      ON t1~idactv = t2~idactv
      WHERE t1~rstype =  c_rstype-labor                 "#EC CI_NOORDER
      AND t1~idactv = ls_acitm-idactvl
      AND t2~spras = sy-langu.

    LOOP AT lt_fmacrsc INTO lwa_fmacrsc WHERE rstyp = c_accom_id-employee.

      IF lwa_fmacrsc-parid = ls_parameters-par01.
        IF ls_afvc-lar01 = ls_activity-actype.
          lwa_fmfpcnf-ism01 = ls_acitm-menge.
          lwa_fmfpcnf-leinh1 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par02.
        IF ls_afvc-lar02 = ls_activity-actype.
          lwa_fmfpcnf-ism02 = ls_acitm-menge.
          lwa_fmfpcnf-leinh2 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par03.
        IF ls_afvc-lar03 = ls_activity-actype.
          lwa_fmfpcnf-ism03 = ls_acitm-menge.
          lwa_fmfpcnf-leinh3 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par04.
        IF ls_afvc-lar04 = ls_activity-actype.
          lwa_fmfpcnf-ism04 = ls_acitm-menge.
          lwa_fmfpcnf-leinh4 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par05.
        IF ls_afvc-lar05 = ls_activity-actype.
          lwa_fmfpcnf-ism05 = ls_acitm-menge.
          lwa_fmfpcnf-leinh5 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par06.
        IF ls_afvc-lar06 = ls_activity-actype.
          lwa_fmfpcnf-ism06 = ls_acitm-menge.
          lwa_fmfpcnf-leinh6 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ls_acitm-idactve IS NOT INITIAL.

    SELECT SINGLE t1~idactv t1~rstype t1~bill t1~actype t2~description
      INTO ls_activity
      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
      ON t1~idactv = t2~idactv
      WHERE t1~rstype =  c_rstype-equnr                 "#EC CI_NOORDER
      AND t1~idactv = ls_acitm-idactve
      AND t2~spras = sy-langu.

    LOOP AT lt_fmacrsc INTO lwa_fmacrsc WHERE rstyp = c_accom_id-equipment.

      IF lwa_fmacrsc-parid = ls_parameters-par01.
        IF ls_afvc-lar01 = ls_activity-actype.
          lwa_fmfpcnf-ism01 = ls_acitm-menge.
          lwa_fmfpcnf-leinh1 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par02.
        IF ls_afvc-lar02 = ls_activity-actype.
          lwa_fmfpcnf-ism02 = ls_acitm-menge.
          lwa_fmfpcnf-leinh2 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par03.
        IF ls_afvc-lar03 = ls_activity-actype.
          lwa_fmfpcnf-ism03 = ls_acitm-menge.
          lwa_fmfpcnf-leinh3 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par04.
        IF ls_afvc-lar04 = ls_activity-actype.
          lwa_fmfpcnf-ism04 = ls_acitm-menge.
          lwa_fmfpcnf-leinh4 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par05.
        IF ls_afvc-lar05 = ls_activity-actype.
          lwa_fmfpcnf-ism05 = ls_acitm-menge.
          lwa_fmfpcnf-leinh5 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par06.
        IF ls_afvc-lar06 = ls_activity-actype.
          lwa_fmfpcnf-ism06 = ls_acitm-menge.
          lwa_fmfpcnf-leinh6 = ls_acitm-qmein.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-- Confirm the hours for activities in the work center
*-- for Accomplishment type 'TAST'
  IF gs_acdoc_infocus-x-achdr-actyp EQ 'TAST'.
    IF ls_acitm-idactve IS NOT INITIAL.
      CLEAR: lwa_fmfpcnf-ism01, lwa_fmfpcnf-leinh1,
             lwa_fmfpcnf-ism02, lwa_fmfpcnf-leinh2,
             lwa_fmfpcnf-ism03, lwa_fmfpcnf-leinh3,
             lwa_fmfpcnf-ism04, lwa_fmfpcnf-leinh4,
             lwa_fmfpcnf-ism05, lwa_fmfpcnf-leinh5,
             lwa_fmfpcnf-ism06, lwa_fmfpcnf-leinh6.

      LOOP AT lt_fmacrsc INTO lwa_fmacrsc WHERE rstyp = c_accom_id-equipment.
        IF ls_parameters-par01 IS NOT INITIAL.
          lwa_fmfpcnf-ism01  = ls_acitm-menge.
          lwa_fmfpcnf-leinh1 = ls_acitm-qmein.
        ENDIF.
        IF ls_parameters-par02 IS NOT INITIAL.
          lwa_fmfpcnf-ism02  = ls_acitm-menge.
          lwa_fmfpcnf-leinh2 = ls_acitm-qmein.
        ENDIF.
        IF ls_parameters-par03 IS NOT INITIAL.
          lwa_fmfpcnf-ism03  = ls_acitm-menge.
          lwa_fmfpcnf-leinh3 = ls_acitm-qmein.
        ENDIF.
        IF ls_parameters-par04 IS NOT INITIAL.
          lwa_fmfpcnf-ism04  = ls_acitm-menge.
          lwa_fmfpcnf-leinh4 = ls_acitm-qmein.
        ENDIF.
        IF ls_parameters-par05 IS NOT INITIAL.
          lwa_fmfpcnf-ism05  = ls_acitm-menge.
          lwa_fmfpcnf-leinh5 = ls_acitm-qmein.
        ENDIF.
        IF ls_parameters-par06 IS NOT INITIAL.
          lwa_fmfpcnf-ism06  = ls_acitm-menge.
          lwa_fmfpcnf-leinh6 = ls_acitm-qmein.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ls_acitm-zzlmnga IS NOT INITIAL AND
       ls_acitm-zzmeinh IS NOT INITIAL.
      lwa_fmfpcnf-gicre = 'X'.
      lwa_fmfpcnf-lmnga = ls_acitm-zzlmnga.
      lwa_fmfpcnf-meinh = ls_acitm-zzmeinh.
      lwa_fmfpcnf-umren = 1.
    ENDIF.
  ENDIF.

ENDFORM.                    " CONFIRMATION_PARAMETER_CHANGE
*&---------------------------------------------------------------------*
*&      Form  CONFIRMATION_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM confirmation_data_check  CHANGING lv_subrc TYPE sy-subrc.

  DATA:lwa_wo            TYPE /agri/s_fmac_wo,
       lwa_ord           TYPE /agri/s_fmac_ord,
       lt_wo             TYPE /agri/t_fmac_wo,
       lt_ord            TYPE /agri/t_fmac_ord,
       lt_fp             TYPE /agri/t_fmac_fp,
       lwa_resource      TYPE /agri/fmacres,
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

  DATA: lwa_items_layout       TYPE /agri/s_fmacitm_layout.

  FIELD-SYMBOLS: <lwa_acitm>    TYPE /agri/s_fmacitm,
                 <lwa_activity> TYPE /agri/s_fmac_src_act.

  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                                   WHERE status = c_process_status-ctd
                                     AND zzconfm IS INITIAL.


    IF <lwa_acitm>-idactvl IS INITIAL AND <lwa_acitm>-idactve IS INITIAL..
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '067' INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.

    IF <lwa_acitm>-aufnr IS INITIAL.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
              NUMBER '051'
              INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.

    IF <lwa_acitm>-idresource IS NOT INITIAL.

      PERFORM idresource_check USING <lwa_acitm>-idresource
                                     <lwa_acitm>-aufnr.

    ENDIF.

    IF <lwa_acitm>-equnr IS NOT INITIAL.

      PERFORM equnr_check USING <lwa_acitm>-equnr
                                <lwa_acitm>-aufnr.

    ENDIF.

    IF <lwa_acitm>-idactvl IS NOT INITIAL.

      MOVE-CORRESPONDING <lwa_acitm> TO lwa_items_layout.

      PERFORM activity_check USING c_rstype-labor

                           CHANGING lwa_items_layout
                                    lt_activity
                                     lt_fmacrsc.
      READ TABLE lt_activity ASSIGNING <lwa_activity> WITH KEY idactv = <lwa_acitm>-idactvl
                                                                rstype = c_rstype-labor.
      IF sy-subrc NE 0.

        MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '063'
                WITH <lwa_acitm>-idactvl
                     <lwa_acitm>-aufnr
              INTO sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
        EXIT.

      ENDIF.
    ENDIF.

    IF <lwa_acitm>-idactve IS NOT INITIAL.

      MOVE-CORRESPONDING <lwa_acitm> TO lwa_items_layout.

      PERFORM activity_check USING c_rstype-equnr

                           CHANGING lwa_items_layout
                                    lt_activity
                                     lt_fmacrsc.
      READ TABLE lt_activity ASSIGNING <lwa_activity> WITH KEY idactv = <lwa_acitm>-idactve
                                                                rstype = c_rstype-equnr.
      IF sy-subrc NE 0.

        MESSAGE ID '/AGRI/FMAC'
                TYPE c_msg_type-error
                NUMBER '063'
                WITH <lwa_acitm>-idactve
                     <lwa_acitm>-aufnr
              INTO sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
        EXIT.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CONFIRMATION_DATA_CHECK
