*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0C .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display .

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program-funloc) IF FOUND.

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
*       parent                      =
        repid                       = c_program-funloc
        dynnr                       = sy-dynnr
        side                        = cl_gui_docking_container=>dock_at_left
        extension                   = 300
*       style                       =
*       lifetime                    = lifetime_default
*       caption                     =
*       metric                      = 0
*       ratio                       =
*       no_autodef_progid_dynnr     =
*       name                        =
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

  IF gs_variables-worklist_refresh EQ c_true.
    PERFORM worklist_refresh USING lv_view.
    CLEAR gs_variables-worklist_refresh.
  ENDIF.

ENDFORM.                    "controls_display_0100
*&---------------------------------------------------------------------*
*&      Form  controls_display_0309
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0309.

  DATA: lv_input              TYPE i,
        ls_environment        TYPE /agri/s_glvc_environment,
        ls_layout             TYPE lvc_s_layo,
        ls_variant            TYPE disvariant,
        lt_sort               TYPE lvc_t_sort,
        lt_group_level        TYPE lvc_t_fimg,
        lt_buttons_to_exclude TYPE ui_functions,
        lt_fcat               TYPE lvc_t_fcat.

  IF ref_attributes_container IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    CREATE OBJECT ref_attributes_container
      EXPORTING
*       parent                      =
        container_name              = '/AGRI/SAPLGLFLM_0309_CC'
*       style                       =
*       lifetime                    = lifetime_default
*       repid                       =
*       dynnr                       =
*       no_autodef_progid_dynnr     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CHECK sy-subrc EQ 0.

    CREATE OBJECT ref_attributes_grid
      EXPORTING
*       i_shellstyle      = 0
*       i_lifetime        =
        i_parent          = ref_attributes_container
*       i_appl_events     = space
*       i_parentdbg       =
*       i_applogparent    =
*       i_graphicsparent  =
*       i_use_variant_class = SPACE
*       i_name            =
*       is_lvc_environment =
*       i_no_auto_details =
*       i_noout_fields_hide = 'X'
*       i_hide_dummy_fields = 'X'
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CHECK sy-subrc = 0.

    PERFORM field_catalog_prepare USING c_structure_name-attributes
                               CHANGING lt_fcat[].
    ls_layout-cwidth_opt = c_true.
    ls_layout-smalltitle = c_true.

    ls_variant-report = c_program-funloc.
    ls_variant-handle = c_variant_handle-attributes.

    PERFORM edit_functions_exclude CHANGING lt_buttons_to_exclude.

    PERFORM sort_group_tables_prepare CHANGING lt_sort
                                               lt_group_level.

    PERFORM control_events_register.

    CALL METHOD ref_attributes_grid->set_table_for_first_display
      EXPORTING
*       i_buffer_active               =
*       i_bypassing_buffer            =
*       i_consistency_check           =
*       i_structure_name              =
        is_variant                    = ls_variant
*       i_save                        =
*       i_default                     = 'X'
        is_layout                     = ls_layout
*       is_print                      =
*       it_special_groups             =
        it_toolbar_excluding          = lt_buttons_to_exclude[]
*       it_hyperlink                  =
*       it_alv_graphics               =
*       it_except_qinfo               =
*       ir_salv_adapter               =
      CHANGING
        it_outtab                     = gt_attr_vals
        it_fieldcatalog               = lt_fcat[]
        it_sort                       = lt_sort[]
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_attributes EQ c_true.

    CLEAR gs_variables-refresh_attributes.

    CALL METHOD ref_attributes_grid->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-cwidth_opt = c_true.
    CALL METHOD ref_attributes_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

    CALL METHOD ref_attributes_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
  ENDIF.

  CALL METHOD ref_attributes_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0309
*&---------------------------------------------------------------------*
*&      Form  controls_display_0312
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0312.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i.

  IF ref_classes_container IS INITIAL.
    CREATE OBJECT ref_classes_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLGLFLM_0312_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_classes_grid IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_classes_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_classes_container
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-class_assignment
                               CHANGING lt_fcat.

    PERFORM control_events_register.

    CALL METHOD ref_classes_grid->set_table_for_first_display
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
        it_outtab                     = gt_flatg_fcat
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_class_grid = c_true.

    CLEAR: gs_variables-refresh_class_grid.

    CALL METHOD ref_classes_grid->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-cwidth_opt = c_true.
    CALL METHOD ref_classes_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

    CALL METHOD ref_classes_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
  ENDIF.

  CALL METHOD ref_classes_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0312
*&---------------------------------------------------------------------*
*&      Form  controls_display_0205
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0205.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i.

  IF ref_terrain_labels_container IS INITIAL.
    CREATE OBJECT ref_terrain_labels_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPGLFLM_0205_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_terrain_labels_grid IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_environment-nocwidth_opt = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-info_fname = 'ROWCOLOR'.

    CREATE OBJECT ref_terrain_labels_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_terrain_labels_container
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-terrain_labels
                                 CHANGING lt_fcat.

    PERFORM control_events_register.

    PERFORM fl_labels_prepare.

    CALL METHOD ref_terrain_labels_grid->set_table_for_first_display
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
        it_outtab                     = gt_fl_labels_layout
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_label_grid = c_true.

    PERFORM fl_labels_prepare.
    CLEAR: gs_variables-refresh_label_grid.

    CALL METHOD ref_terrain_labels_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*  IF gs_variables-document_mode = c_mode_display.
  lv_input = '0'.
*  ELSE.
*    lv_input = '1'.
*  ENDIF.

  CALL METHOD ref_terrain_labels_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0205
*&---------------------------------------------------------------------*
*&      Form  controls_display_0206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0206.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i.

  IF ref_hierarchy_disply_container IS INITIAL.
    CREATE OBJECT ref_hierarchy_disply_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPGLFLM_0206_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_hierarchy_disply_grid IS INITIAL.

    ls_environment-switchoff_performance = c_true.
*    ls_environment-cwidth_opt = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-zebra = c_true.
*    ls_layout-stylefname = 'STYLES'.
*    ls_layout-info_fname = 'ROWCOLOR'.

    CREATE OBJECT ref_hierarchy_disply_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_hierarchy_disply_container
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-hierarchy_display
                                 CHANGING lt_fcat.

    CALL METHOD ref_hierarchy_disply_grid->set_table_for_first_display
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
        it_outtab                     = gt_flhier_list
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CLEAR gs_variables-refresh_hierarchy_grid.

  ELSEIF gs_variables-refresh_hierarchy_grid = c_true.

    CLEAR: gs_variables-refresh_hierarchy_grid.

    CALL METHOD ref_hierarchy_disply_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  lv_input = '0'.
  CALL METHOD ref_hierarchy_disply_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0206
*&---------------------------------------------------------------------*
*&      Form  controls_display_0316
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0316.

  DATA: lv_url TYPE char256.

  IF ref_container_map IS INITIAL.

    CREATE OBJECT ref_container_map
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLGLFLM_0316_CC'
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

  ENDIF.

  IF ref_html_viewer_map IS INITIAL.

    CREATE OBJECT ref_html_viewer_map
      EXPORTING
*       SHELLSTYLE         =
        parent             = ref_container_map
*       LIFETIME           = LIFETIME_DEFAULT
*       SAPHTMLP           =
*       UIFLAG             =
*       NAME               =
*       SAPHTTP            =
*       QUERY_TABLE_DISABLED =
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    CHECK sy-subrc EQ 0.

    CONCATENATE gs_tglfllvl-wpurl /agri/s_glflot-tplnr_fl
           INTO lv_url.

    CALL METHOD ref_html_viewer_map->show_url
      EXPORTING
        url                    = lv_url
*       FRAME                  =
*       IN_PLACE               = ' X'
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5.

    CLEAR gs_variables-refresh_map.

  ELSEIF NOT gs_variables-refresh_map IS INITIAL.

    CLEAR gs_variables-refresh_map.

    CONCATENATE gs_tglfllvl-wpurl /agri/s_glflot-tplnr_fl
           INTO lv_url.

    CALL METHOD ref_html_viewer_map->show_url
      EXPORTING
        url                    = lv_url
*       FRAME                  =
*       IN_PLACE               = ' X'
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5.

    CALL METHOD ref_html_viewer_map->do_refresh
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

  ENDIF.

ENDFORM.                    "controls_display_0316
*&---------------------------------------------------------------------*
*&      Form  control_events_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register .
  DATA : lv_routine(28) VALUE 'CONTROL_EVENTS_REGISTER_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

  PERFORM (lv_routine) IN PROGRAM (c_program-funloc) IF FOUND.

ENDFORM.                    " control_events_register

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
*&      Form  control_events_register_0203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0205.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_terrain_labels_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  SET HANDLER : ref_event_handler->on_toolbar_grid
                FOR ref_terrain_labels_grid,
                ref_event_handler->on_data_changed_grid
                FOR ref_terrain_labels_grid,
                ref_event_handler->on_user_command_grid
                FOR ref_terrain_labels_grid.

ENDFORM.                    "control_events_register_0203
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0309
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0309.

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_attributes_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

****Register the F4 event for grid
  lwa_f4-fieldname = 'ATWRT'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_attributes_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: "ref_event_handler->on_toolbar_attr
*               FOR ref_grid_attributes,
               ref_event_handler->on_f4_request_attr
               FOR ref_attributes_grid,
               ref_event_handler->on_data_changed_attr
               FOR ref_attributes_grid,
               ref_event_handler->on_user_command_grid
               FOR ref_attributes_grid.

*               ref_event_handler->on_button_click_attr
*               FOR ref_grid_attributes,
*               ref_event_handler->on_hotspot_click_attr
*               FOR ref_grid_attributes,
*               ref_event_handler->on_data_changed
*               FOR ref_attributes_grid.
*               ref_event_handler->on_user_command_attr
*               FOR ref_grid_attributes,
*               ref_event_handler->on_double_click_attr
*               FOR ref_grid_attributes.

ENDFORM.                    "control_events_register_0309
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0312
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0312.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_classes_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  SET HANDLER : ref_event_handler->on_toolbar_grid
                FOR ref_classes_grid,
                ref_event_handler->on_data_changed_grid
                FOR ref_classes_grid,
                ref_event_handler->on_user_command_grid
                FOR ref_classes_grid.

ENDFORM.                    "control_events_register_0312
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0319
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0319.

  DATA: lt_events TYPE cntl_simple_events,
        lwa_event TYPE cntl_simple_event.

  lwa_event-eventid = cl_gui_column_tree=>eventid_link_click.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_tree_ext_doc->set_registered_events
    EXPORTING
      events                    = lt_events[]
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3
      OTHERS                    = 4.

  IF  sy-subrc NE 0
  AND NOT sy-msgid IS INITIAL
  AND NOT sy-msgno IS INITIAL
  AND NOT sy-msgty IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER: ref_event_handler->on_hierarchy_node_add
               FOR ref_tree_ext_doc,
               ref_event_handler->on_node_layout_set
               FOR ref_tree_ext_doc.
*               ref_event_handler->on_node_hotspot_click_dfl_tree
*               FOR ref_tree_ext_doc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EDITMASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_editmask  USING "lv_alkey TYPE iflos-alkey
                           lv_tplkz TYPE /agri/gltplkz
                  CHANGING lv_strno TYPE /agri/glstrno
                           lv_subrc.

  DATA: lv_strno_tmp       TYPE ilom_strno,
        lv_separ_corrected LIKE iref-iind.

  lv_strno_tmp = lv_strno.
  CALL FUNCTION '/AGRI/GLFL_KEY_STRUCTURE'
    EXPORTING
      i_structured_key               = lv_strno_tmp
      i_structure_indicator          = lv_tplkz
      i_correct_separators           = c_true
      i_correct_block_digits         = c_true
    IMPORTING
      e_corrected_key                = lv_strno
      e_separator_corrected          = lv_separ_corrected
*     E_BLOCK_DIGIT_CORRECTED        =
*     E_EDITMASK                     =
*     E_HIERARCHY_LEVELS             =
    EXCEPTIONS
      ex_str_indicator_not_found     = 1
      ex_key_too_long                = 2
      ex_first_block_is_initial      = 3
      ex_no_compliance_with_editmask = 4
      ex_key_ends_with_separator     = 5
      OTHERS                         = 6.

*  CALL FUNCTION 'FUNC_LOCATION_KEY_STRUCTURE'
*    EXPORTING
*      structured_key              = lv_strno
*      object_table                = c_object-func_loc
*      labeling_system             = lv_alkey
*      structure_indicator         = lv_tplkz
*      correct_separators          = c_true
*      correct_block_digits        = c_true
*    IMPORTING
*      corrected_key               = lv_strno
*      separator_corrected         = lv_separ_corrected
**     BLOCK_DIGIT_CORRECTED       =
**     EDITMASK                    =
**     HIERARCHY_LEVELS            =
*    EXCEPTIONS
*      str_indicator_not_found     = 1
*      key_too_long                = 2
*      first_block_is_initial      = 3
*      no_compliance_with_editmask = 4
*      key_ends_with_separator     = 5
*      exception_from_user_exit    = 6
*      OTHERS                      = 7.
  IF sy-subrc <> 0.
    lv_subrc = 4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

  IF lv_separ_corrected IS NOT INITIAL.
    MESSAGE s003(/agri/glfl).
  ENDIF.

**--Clear label if alternative labeling enabled
*  CHECK lv_alkey IS INITIAL.
*
*  CALL FUNCTION 'FUNC_LOCATION_ASSIGN_KEY'
*    EXPORTING
*      location_label                 = lv_strno
*      labeling_system                = lv_alkey
*      structure_indicator            = lv_tplkz
**     OLD_LOCATION_LABEL             = ' '
**     OLD_LABELING_SYSTEM            = ' '
**     UPDATE_ASYNCHRON               = 'X'
*    IMPORTING
*      location_key                   = lv_tplnr
*    EXCEPTIONS
*      label_too_long                 = 1
*      label_already_used_in_session  = 2
*      label_already_locked           = 3
*      label_in_historic_version      = 4
*      label_already_used_on_db       = 5
*      label_used_in_other_label_syst = 6
*      label_syst_not_found           = 7
*      enq_system_failure             = 8
*      OTHERS                         = 9.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE c_msg_type-error NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
*    message_simple space.
** Implement suitable error handling here
**    lv_tplnr = lv_strno.
*  ENDIF.

ENDFORM.                    " CHECK_EDITMASK
*&---------------------------------------------------------------------*
*&      Form  CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM changes_confirm  CHANGING lv_answer.

  DATA: lv_document_changed,
        lv_modified.

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_fldoc_infocus IS NOT INITIAL.

  PERFORM grid_changes_check CHANGING lv_modified.

  IF lv_modified EQ c_true
  OR gs_variables-manual_changes EQ c_true.
    lv_document_changed = c_true.
  ENDIF.

  IF gs_variables-data_changed EQ c_true.
    lv_document_changed = c_true.
  ENDIF.

  IF lv_document_changed IS INITIAL.

    CALL FUNCTION '/AGRI/GLFL_CHECKCHANGES_SINGLE'
      EXPORTING
        is_fldoc  = gs_fldoc_infocus
      CHANGING
        c_changed = lv_document_changed
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

  CHECK lv_document_changed EQ c_true OR sy-datar EQ c_true.

  popup_to_confirm TEXT-007 TEXT-008 c_true lv_answer.

  CASE lv_answer.
    WHEN '1'.
      lv_answer = 'A'.
      ok_code = c_fcode-save.
*    WHEN '2'.
*      CLEAR: gs_variables-data_changed.
  ENDCASE.

ENDFORM.                    " CHANGES_CONFIRM

*&---------------------------------------------------------------------*
*&      Form  class_attributes_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM class_attributes_read USING lv_agtyp TYPE klassenart
                                 lv_clint TYPE clint
                                 lv_class TYPE klasse_d
                        CHANGING lt_athdr TYPE /agri/t_gathdr.

  DATA: lt_klah       TYPE tt_klah,
        lwa_klah      TYPE klah,
        lwa_atgrp     TYPE /agri/s_gatg_attr,
        lt_atgrp      TYPE /agri/t_gatg_attr,
        lt_attributes TYPE /agri/t_gathdr.

  FIELD-SYMBOLS: <lwa_athdr> TYPE /agri/s_gathdr.

  lwa_klah-klart = lv_agtyp.
  lwa_klah-clint = lv_clint.
  lwa_klah-class = lv_class.
  APPEND lwa_klah TO lt_klah.

  CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
    EXPORTING
      it_klah                   = lt_klah
      i_agtyp                   = lv_agtyp
      i_no_composite_attributes = c_true
    IMPORTING
      et_atgrp                  = lt_atgrp
*     et_cattr                  =
      et_athdr                  = lt_attributes
*     et_atvrref                =
*     et_cawn                   =
    .

  READ TABLE lt_atgrp INTO lwa_atgrp INDEX 1.
  IF sy-subrc = 0.
    LOOP AT lt_attributes ASSIGNING <lwa_athdr>.
      <lwa_athdr>-class = lwa_atgrp-atgrp.
      <lwa_athdr>-clint = lwa_atgrp-clint.
    ENDLOOP.
    APPEND LINES OF lt_attributes TO lt_athdr.
  ENDIF.

ENDFORM.                    "class_attributes_read
*&---------------------------------------------------------------------*
*&      Form  CLASSES_GRID_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM classes_grid_update .

  DATA:
    lwa_flatg      TYPE /agri/s_glflatg,
    lwa_flatg_fcat TYPE /agri/s_glflatg_fcat,
    lwa_mod_row    TYPE lvc_s_modi,
    lv_modified,
    lv_subrc       TYPE sy-subrc,
    lv_valid.

  FIELD-SYMBOLS: <lwa_flatg> TYPE /agri/s_glflatg.

  CHECK gs_variables-document_mode NE c_mode_display.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_fldoc_infocus-x-flhdr.

  IF ok_code EQ c_fcode-class_delete.
    PERFORM fcode_class_delete.
    CLEAR ok_code.
  ENDIF.

  lv_modified = ref_classes_grid->data_modified_check( ).
  IF lv_modified EQ c_true OR
     gs_variables-manual_changes EQ c_true.
    CALL METHOD ref_classes_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      CLEAR: gs_variables-manual_changes.
      gs_variables-refresh_class_grid = c_true.
    ENDIF.

  ENDIF.

  LOOP AT gt_class_mod_rows INTO lwa_mod_row.

    CLEAR: lwa_flatg_fcat, lwa_flatg.
    READ TABLE gt_flatg_fcat INTO lwa_flatg_fcat
                            INDEX lwa_mod_row-row_id.
    CHECK  lwa_flatg_fcat IS NOT INITIAL.

    MOVE-CORRESPONDING lwa_flatg_fcat TO lwa_flatg.

    PERFORM classes_data_check USING lwa_flatg
                            CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      gs_variables-errors = c_true.
      CONTINUE.
    ENDIF.

    READ TABLE gs_fldoc_infocus-x-flatg ASSIGNING <lwa_flatg>
                               WITH KEY class = lwa_flatg-class
                                     tplnr_fl = lwa_flatg-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_flatg NE <lwa_flatg>.
        MOVE lwa_flatg TO <lwa_flatg>.
        IF <lwa_flatg>-updkz NE c_updkz_new.
          gs_variables-data_changed = c_true.
          <lwa_flatg>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM class_exist_check USING space
                                      c_agtyp-functional_location
                             CHANGING lwa_flatg_fcat
                                      lv_subrc.
      IF lv_subrc NE 0.
        gs_variables-errors = c_true.
        CONTINUE.
      ENDIF.
      PERFORM class_attributes_read USING c_agtyp-functional_location
                                    lwa_flatg_fcat-clint
                                    lwa_flatg_fcat-class
                           CHANGING gt_athdr.
      gs_variables-refresh_class_grid = c_true.
      gs_variables-refresh_attributes = c_true.
      gs_variables-data_changed = c_true.
      lwa_flatg_fcat-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
      lwa_flatg_fcat-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_flatg_fcat TO lwa_flatg.
      APPEND lwa_flatg TO gs_fldoc_infocus-x-flatg.
    ENDIF.

  ENDLOOP.

  IF gs_variables-errors EQ c_true.
    CLEAR ok_code.
  ENDIF.
  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.                    " CLASSES_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  CLASS_EXIST_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM class_exist_check USING lv_description_only
                             lv_klart TYPE klassenart
                    CHANGING lwa_flatg_fcat TYPE /agri/s_glflatg_fcat
                             lv_subrc TYPE sy-subrc.
  DATA: lwa_klah       TYPE klah,
        lv_return_code TYPE sy-subrc.

  CLEAR: lv_subrc.
  CALL FUNCTION 'CLMA_CLASS_EXIST'
    EXPORTING
      class             = lwa_flatg_fcat-class
*     CLASSIFY_ACTIVITY = ' '
      classnumber       = lwa_flatg_fcat-clint
      classtype         = lv_klart
*     DATE              = SY-DATUM
*     description_only  = lv_description_only
*     LANGUAGE          = SY-LANGU
*     MODE              = ' '
*     NO_DESCRIPTION    = ' '
*     CLASS_ACTIVITY    =
      bypassing_buffer  = c_true
    IMPORTING
      class_description = lwa_flatg_fcat-klbez
*     CLASS_LANGUAGE    =
*     NOT_VALID         =
*     NO_ACTIVE_STATUS  =
*     NO_AUTHORITY_CLASSIFY =
*     NO_AUTHORITY_MAINTAIN =
*     NO_AUTHORITY_SELECT   =
      ret_code          = lv_return_code
      xklah             = lwa_klah
*     E_BUFFER_COUNT    =
    EXCEPTIONS
      no_valid_sign     = 1
      OTHERS            = 2.
  IF sy-subrc <> 0 OR
     lv_return_code IS NOT INITIAL.
    IF lv_description_only IS INITIAL.
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-error NUMBER '010'
                              WITH lwa_flatg_fcat-class INTO sy-msgli.
      message_simple space.
    ENDIF.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  CHECK lv_description_only IS INITIAL.
  lwa_flatg_fcat-clint = lwa_klah-clint.

ENDFORM.                    " CLASS_EXIST_CHECK
*&---------------------------------------------------------------------*
*&      Form  controls_display_0203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
        container_name              = '/AGRI/SAPGLFLM_0203_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_multi_lang_desc_grid IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_environment-nocwidth_opt = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.

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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-fl_multi_lang_desc
                                 CHANGING lt_fcat.

    PERFORM control_events_register.

    PERFORM fl_desc_prepare.

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
        it_outtab                     = gt_fl_desc_layout
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_desc_grid = c_true.

    PERFORM fl_desc_prepare.
    CLEAR: gs_variables-refresh_desc_grid.

    CALL METHOD ref_multi_lang_desc_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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

ENDFORM.                    "controls_display_0203

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
*&      Form  cam_okcode_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cam_okcode_set .

  IF sy-ucomm+0(1) CS '$'.
    ok_code = sy-ucomm.
  ENDIF.

  IF ok_code+0(1) CS '$'.

    CALL FUNCTION 'ADDR_SUBSCREEN_SET_OKCODE'
      EXPORTING
        ok_code = ok_code.
  ELSE.

    CALL FUNCTION 'ADDR_SUBSCREEN_SET_OKCODE'
      EXPORTING
        ok_code = '$CHC'.
  ENDIF.

ENDFORM.                    " cam_okcode_set
*&---------------------------------------------------------------------*
*&      Form  CLASSES_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM classes_data_display .

  DATA: lv_subrc       TYPE sy-subrc,
        lwa_flatg      TYPE /agri/s_glflatg,
        lwa_flatg_fcat TYPE /agri/s_glflatg_fcat.

  IF gt_flatg_fcat[] IS INITIAL OR
     gs_variables-refresh_class_grid IS NOT INITIAL.

    REFRESH gt_flatg_fcat[].
    LOOP AT gs_fldoc_infocus-x-flatg INTO lwa_flatg.
      MOVE-CORRESPONDING lwa_flatg TO lwa_flatg_fcat.
      PERFORM class_exist_check USING c_true
                                      c_agtyp-functional_location
                             CHANGING lwa_flatg_fcat
                                      lv_subrc.
      PERFORM classes_styles_prepare CHANGING lwa_flatg_fcat.
      APPEND lwa_flatg_fcat TO gt_flatg_fcat.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " CLASSES_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  controls_display_0307
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0307.

  DATA: lt_fcat           TYPE lvc_t_fcat,
        ls_layout         TYPE lvc_s_layo,
        ls_variant        TYPE disvariant,
        lv_structure_name LIKE dd03l-fieldname.
  DATA: lv_row_no TYPE lvc_s_row,
        lv_col_id TYPE lvc_s_col.

  CHECK NOT gt_additional_data[] IS INITIAL.

  IF ref_additional_data_container IS INITIAL.

    CREATE OBJECT ref_additional_data_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLGLFLM_0307_CC'
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

    CREATE OBJECT ref_additional_data_grid
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

    PERFORM field_catalog_prepare USING '/AGRI/S_ABGL_USER_SCRFIELDS'
                               CHANGING lt_fcat.
    PERFORM field_value_conversions USING '1'.

****Prepare ALV Grid Layout
    ls_layout-cwidth_opt = c_true.
    ls_layout-no_toolbar = c_true.
****Release 60D
    ls_layout-no_headers = c_true.
*    ls_layout-no_hgridln = c_true.
*    ls_layout-no_vgridln = c_true.
    ls_layout-no_rowmark = c_true.
***Rel.d sp1 grid titles
*    ls_layout-smalltitle = c_true.
*    ls_layout-grid_title = text-132.

*    ls_variant-report = c_program-func_loc.
*    ls_variant-handle = c_variant_handle-cm_additional_data.

    CALL METHOD ref_additional_data_grid->set_table_for_first_display
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
        it_fieldcatalog               = lt_fcat[]
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

    CALL METHOD ref_additional_data_grid->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-cwidth_opt = c_true.
    CALL METHOD ref_additional_data_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

    CALL METHOD ref_additional_data_grid->get_current_cell
      IMPORTING
        es_row_id = lv_row_no
        es_col_id = lv_col_id.
    CALL METHOD ref_additional_data_grid->refresh_table_display.
    CALL METHOD ref_additional_data_grid->set_current_cell_via_id
      EXPORTING
        is_row_id    = lv_row_no
        is_column_id = lv_col_id.
  ENDIF.

  IF gs_variables-overview_mode EQ c_mode_display OR
     gs_fldoc_infocus IS INITIAL.

    CALL METHOD ref_additional_data_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ELSE.
    CALL METHOD ref_additional_data_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ENDIF.

ENDFORM.                    "controls_display_0307
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0307
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0307.

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF NOT ref_additional_data_grid IS INITIAL.
    CALL METHOD ref_additional_data_grid->register_edit_event
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

    CALL METHOD ref_additional_data_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: ref_event_handler->on_data_changed_grid
                 FOR ref_additional_data_grid,
                 ref_event_handler->on_value_request
                 FOR ref_additional_data_grid.
  ENDIF.

ENDFORM.                    "control_events_register_0307
*&---------------------------------------------------------------------*
*&      Form  controls_display_0313
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0313.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        ls_variant          TYPE disvariant,
        lv_input            TYPE i.

  IF ref_owners_container IS INITIAL.
    CREATE OBJECT ref_owners_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLGLFLM_0313_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_owners_grid IS INITIAL.

    CLEAR: gs_variables-refresh_owners_grid.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_owners_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_owners_container
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-owners
                               CHANGING lt_fcat.

    ls_variant-report = c_program-funloc.
    ls_variant-handle = c_variant_handle-owners.

    PERFORM control_events_register.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

    CALL METHOD ref_owners_grid->set_table_for_first_display
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_BYPASSING_BUFFER            =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
        is_variant                    = ls_variant
        i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = ls_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
        it_toolbar_excluding          = lt_toolbar_excludes
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
*       IT_EXCEPT_QINFO               =
      CHANGING
        it_outtab                     = gt_owners
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_owners_grid = c_true.

    PERFORM refresh_owners_grid.

  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
  ENDIF.

  CALL METHOD ref_owners_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0313
*&---------------------------------------------------------------------*
*&      Form  controls_display_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0319.

  DATA: lt_fcat                TYPE lvc_t_fcat,
        ls_hierhdr             TYPE treev_hhdr,
        ls_variant             TYPE disvariant,
        ls_layout              TYPE lvc_s_layo,
        lt_sort                TYPE lvc_t_sort,
        lt_group_levels_layout TYPE lvc_t_fimg,
        lt_toolbar_excludes    TYPE ui_functions.

  IF ref_container_tree_ext_doc IS INITIAL.
    CREATE OBJECT ref_container_tree_ext_doc
      EXPORTING
*       PARENT                      =
        container_name              = 'SAPLGLFLM_0319_CC'
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

    CREATE OBJECT ref_tree_ext_doc
      EXPORTING
*       I_LIFETIME                  =
        i_parent                    = ref_container_tree_ext_doc
*       I_SHELLSTYLE                =
*       I_NODE_SELECTION_MODE       = cl_gui_column_Tree=>NODE_SEL_MODE_SIN
*       I_HIDE_SELECTION            =
*       I_ITEM_SELECTION            = 'X'
*       i_no_toolbar                =
        i_no_html_header            = c_true
*       I_PRINT                     =
        i_no_auto_details           = c_true
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.

    CHECK sy-subrc EQ 0.

    CLEAR ls_layout.
    ls_layout-cwidth_opt = c_true.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-info_fname = 'ROWCOLOR'.

    PERFORM control_events_register.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes[].

    PERFORM field_catalog_prepare USING c_structure_name-extdfl_fcat
                               CHANGING lt_fcat.

    PERFORM extdfl_tree_prepare CHANGING gt_glflot_buffer
                              gt_extdfl_tree.


    PERFORM sort_table_build TABLES lt_sort
                                    lt_group_levels_layout
                                    lt_fcat.

    CALL METHOD ref_tree_ext_doc->set_table_for_first_display
      EXPORTING
        i_fname_hier_ind     = 'HASHIER'
        i_value_hier_ind     = c_true
        is_layout            = ls_layout
        it_toolbar_excluding = lt_toolbar_excludes[]
        it_grouplevel_layout = lt_group_levels_layout[]
      CHANGING
        it_outtab            = gt_extdfl_tree[]
        it_fieldcatalog      = lt_fcat[]
        it_sort              = lt_sort[]
      EXCEPTIONS
        too_many_sort_fields = 1
        OTHERS               = 2.

    CALL METHOD ref_tree_ext_doc->hierarchy_header_set
      EXPORTING
        i_heading = TEXT-208
        i_tooltip = TEXT-208
        i_width   = 70.

    CALL METHOD ref_tree_ext_doc->expand_tree
      EXPORTING
        i_level                 = 99
      EXCEPTIONS
        failed                  = 1
        cntl_system_error       = 2
        error_in_node_key_table = 3
        dp_error                = 4
        node_not_found          = 5
        OTHERS                  = 6.

    CLEAR gs_variables-refresh_extdfl_tree.

  ELSEIF gs_variables-refresh_extdfl_tree EQ c_true.

****Release 60D_SP2 – Docflow Changes

    CALL METHOD ref_tree_ext_doc->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog   = lt_fcat[]
      EXCEPTIONS
        error_get_width   = 1
        cntl_system_error = 2
        dp_error          = 3
        failed            = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-extdfl_fcat
                               CHANGING lt_fcat.

    PERFORM extdfl_tree_prepare CHANGING gt_glflot_buffer
                              gt_extdfl_tree.


    CALL METHOD ref_tree_ext_doc->frontend_fieldcatalog_set
      EXPORTING
        it_fieldcatalog = lt_fcat[].

    CALL METHOD ref_tree_ext_doc->refresh_table_display.

    CALL METHOD ref_tree_ext_doc->hierarchy_header_set
      EXPORTING
        i_heading = TEXT-208
        i_tooltip = TEXT-208
        i_width   = 70.

    CALL METHOD ref_tree_ext_doc->expand_tree
      EXPORTING
        i_level                 = 99
      EXCEPTIONS
        failed                  = 1
        cntl_system_error       = 2
        error_in_node_key_table = 3
        dp_error                = 4
        node_not_found          = 5
        OTHERS                  = 6.

    CLEAR gs_variables-refresh_extdfl_tree.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0313
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0313.

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_owners_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

****Register the F4 event for grid
  lwa_f4-fieldname = 'OWNER'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_owners_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER : ref_event_handler->on_toolbar_owners
                FOR ref_owners_grid,
                ref_event_handler->on_data_changed_owner
                FOR ref_owners_grid,
                ref_event_handler->on_user_command_grid
                FOR ref_owners_grid,
                ref_event_handler->on_f4_request_grid
                FOR ref_owners_grid.

ENDFORM.                    "control_events_register_0313
*&---------------------------------------------------------------------*
*&      Form  controls_display_0317
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0317.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        ls_variant          TYPE disvariant,
        lv_input            TYPE i.

  IF ref_plants_container IS INITIAL.
    CREATE OBJECT ref_plants_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/SAPLGLFLM_0317_CC'
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF ref_plants_grid IS INITIAL.

    CLEAR: gs_variables-refresh_plants_grid.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_plants_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_plants_container
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
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-plants
                               CHANGING lt_fcat.

    ls_variant-report = c_program-funloc.
    ls_variant-handle = c_variant_handle-plants.

    PERFORM control_events_register.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

    CALL METHOD ref_plants_grid->set_table_for_first_display
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_BYPASSING_BUFFER            =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
        is_variant                    = ls_variant
        i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = ls_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
        it_toolbar_excluding          = lt_toolbar_excludes
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
*       IT_EXCEPT_QINFO               =
      CHANGING
        it_outtab                     = gt_plants
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF gs_variables-refresh_plants_grid = c_true.

    CLEAR:gs_variables-refresh_plants_grid.
    CALL METHOD ref_plants_grid->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-cwidth_opt = c_true.
    CALL METHOD ref_plants_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
    CALL METHOD ref_plants_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  IF gs_variables-document_mode = c_mode_display.
    lv_input = '0'.
  ELSE.
    lv_input = '1'.
  ENDIF.

  CALL METHOD ref_plants_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0317
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0317
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0317.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_plants_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  SET HANDLER : ref_event_handler->on_toolbar_grid
                FOR ref_plants_grid,
                ref_event_handler->on_data_changed_grid
                FOR ref_plants_grid,
                ref_event_handler->on_user_command_grid
                FOR ref_plants_grid.

ENDFORM.                    "control_events_register_0317
***&---------------------------------------------------------------------*
***&      Form  CHANGE_DOCUMENTS_READ
***&---------------------------------------------------------------------*
***       text
***----------------------------------------------------------------------*
***  -->  p1        text
***  <--  p2        text
***----------------------------------------------------------------------*
*FORM change_documents_read .
*
*  DATA : lv_objectid    TYPE cdhdr-objectid,
*         lv_objid       TYPE cdhdr-objectid,
*         lv_username    TYPE sy-uname,
*         lwa_cdhdr      TYPE cdhdr,
*         lwa_cdhdr_fl   TYPE cdhdr,
*         lt_cdhdr       TYPE TABLE OF cdhdr,
*         lt_cdhdr_fl    TYPE TABLE OF cdhdr,
*         lt_cdhdr_fa    TYPE TABLE OF cdhdr,
*         lt_cdhdr_iflo  TYPE TABLE OF cdhdr,
*         lt_cddetails   TYPE TABLE OF cdred,
*         lwa_tplnr_fl   LIKE LINE  OF gt_tplnr_fl.
*
*  IF gt_cdhdr IS INITIAL.
*    LOOP AT gt_tplnr_fl INTO lwa_tplnr_fl.
*      lv_objectid = lwa_tplnr_fl-tplnr_fl.
*
*      CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
*        EXPORTING
**         ARCHIVE_HANDLE             = 0
**         DATE_OF_CHANGE             = '00000000'
*          objectclass                = c_object-change_documents
*          objectid                   = lv_objectid
**         TIME_OF_CHANGE             = '000000'
*          username                   = lv_username
**         LOCAL_TIME                 = ' '
**         TIME_ZONE                  = 'UTC'
**         DATE_UNTIL                 = '99991231'
**         TIME_UNTIL                 = '235959'
**         NOPLUS_ASWILDCARD_INOBJID  = ' '
**         READ_CHANGEDOCU            = ' '
*        TABLES
*          i_cdhdr                    = lt_cdhdr_fl
*        EXCEPTIONS
*          no_position_found          = 1
*          wrong_access_to_archive    = 2
*          time_zone_conversion_error = 3
*          OTHERS                     = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*      CONCATENATE lv_objectid'*'INTO lv_objid.
*      CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
*        EXPORTING
**         ARCHIVE_HANDLE             = 0
**         DATE_OF_CHANGE             = '00000000'
*          objectclass                = c_object-change_documents_fa
*          objectid                   = lv_objid
**         TIME_OF_CHANGE             = '000000'
*          username                   = lv_username
**         LOCAL_TIME                 = ' '
**         TIME_ZONE                  = 'UTC'
**         DATE_UNTIL                 = '99991231'
**         TIME_UNTIL                 = '235959'
**         NOPLUS_ASWILDCARD_INOBJID  = ' '
**         READ_CHANGEDOCU            = ' '
*        TABLES
*          i_cdhdr                    = lt_cdhdr_fa
*        EXCEPTIONS
*          no_position_found          = 1
*          wrong_access_to_archive    = 2
*          time_zone_conversion_error = 3
*          OTHERS                     = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*      CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
*        EXPORTING
**         ARCHIVE_HANDLE             = 0
**         DATE_OF_CHANGE             = '00000000'
*          objectclass                = c_object-change_documents_iflo
*          objectid                   = lv_objectid
**         TIME_OF_CHANGE             = '000000'
*          username                   = lv_username
**         LOCAL_TIME                 = ' '
**         TIME_ZONE                  = 'UTC'
**         DATE_UNTIL                 = '99991231'
**         TIME_UNTIL                 = '235959'
**         NOPLUS_ASWILDCARD_INOBJID  = ' '
**         READ_CHANGEDOCU            = ' '
*        TABLES
*          i_cdhdr                    = lt_cdhdr_iflo
*        EXCEPTIONS
*          no_position_found          = 1
*          wrong_access_to_archive    = 2
*          time_zone_conversion_error = 3
*          OTHERS                     = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      APPEND LINES OF lt_cdhdr_fl   TO lt_cdhdr.
*      APPEND LINES OF lt_cdhdr_fa   TO lt_cdhdr.
*      APPEND LINES OF lt_cdhdr_iflo TO lt_cdhdr.
*
*      LOOP AT lt_cdhdr INTO lwa_cdhdr_fl.
*        CALL FUNCTION 'CHANGEDOCUMENT_READ'
*         EXPORTING
**         ARCHIVE_HANDLE                   = 0
*          changenumber                     = lwa_cdhdr_fl-changenr
**         DATE_OF_CHANGE                   = '00000000'
*          objectclass                      = lwa_cdhdr_fl-objectclas
*          objectid                         = lwa_cdhdr_fl-objectid
**         TABLEKEY                         = ' '
**         TABLENAME                        = ' '
**         TIME_OF_CHANGE                   = '000000'
**         USERNAME                         = lv_username
**         LOCAL_TIME                       = ' '
**         TABLEKEY254                      = ' '
**         KEYGUID                          = ' '
**         DATE_UNTIL                       = '99991231'
**         TIME_UNTIL                       = '235959'
**         KEYGUID_STR                      = ' '
**         READ_CHANGEDOCU                  = ' '
**         TIME_ZONE                        = ' '
**       IMPORTING
**         ET_CDRED_STR                     =
*         TABLES
*            editpos                        = lt_cddetails
*         EXCEPTIONS
*          no_position_found                = 1
*          wrong_access_to_archive          = 2
*          time_zone_conversion_error       = 3
*          OTHERS                           = 4
*                  .
*        IF sy-subrc <> 0.
** Implement suitable error handling here
*        ENDIF.
*        APPEND LINES OF lt_cddetails TO gt_cddetails_final.
*        REFRESH: lt_cddetails.
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    " CHANGE_DOCUMENTS_READ
***&---------------------------------------------------------------------*
***&      Form  CHANGE_DOCUMENTS_DETAILS_READ
***&---------------------------------------------------------------------*
***       text
***----------------------------------------------------------------------*
***  -->  p1        text
***  <--  p2        text
***----------------------------------------------------------------------*
**FORM change_documents_details_read .
**
**  DATA : lv_objectid  TYPE cdhdr-objectid,
**         lv_username  TYPE sy-uname,
**         lwa_cdhdr    TYPE cdhdr,
**         lwa_cdhdr_fl TYPE cdhdr,
**         lt_cdhdr_fl  TYPE TABLE OF cdhdr,
**         lt_cddetails TYPE TABLE OF cdred,
**         lwa_tplnr_fl LIKE LINE OF gt_tplnr_fl.
**
**  IF gt_cdhdr IS INITIAL.
**    LOOP AT gt_tplnr_fl INTO lwa_tplnr_fl.
**      lv_objectid = lwa_tplnr_fl-tplnr_fl.
**
**      CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
**        EXPORTING
**          objectclass                = c_object-fl_change_documents
**          objectid                   = lv_objectid
***         TIME_OF_CHANGE             = '000000'
**          username                   = lv_username
***         LOCAL_TIME                 = ' '
***         TIME_ZONE                  = 'UTC'
***         DATE_UNTIL                 = '99991231'
***         TIME_UNTIL                 = '235959'
***         NOPLUS_ASWILDCARD_INOBJID  = ' '
***         READ_CHANGEDOCU            = ' '
**        TABLES
**          i_cdhdr                    = lt_cdhdr_fl
**        EXCEPTIONS
**          no_position_found          = 1
**          wrong_access_to_archive    = 2
**          time_zone_conversion_error = 3
**          OTHERS                     = 4.
**      IF sy-subrc <> 0.
*** Implement suitable error handling here
**      ENDIF.
**      LOOP AT lt_cdhdr_fl INTO lwa_cdhdr_fl.
**        CALL FUNCTION 'CHANGEDOCUMENT_READ'
**          EXPORTING
**            changenumber                     = lwa_cdhdr_fl-changenr
**            objectclass                      = lwa_cdhdr_fl-objectclas
**            objectid                         = lwa_cdhdr_fl-objectid
***         TABLEKEY                           = ' '
***         TABLENAME                          = ' '
***         TIME_OF_CHANGE                     = '000000'
***         USERNAME                           = lv_username
***         LOCAL_TIME                         = ' '
***         TABLEKEY254                        = ' '
***         KEYGUID                            = ' '
***         DATE_UNTIL                         = '99991231'
***         TIME_UNTIL                         = '235959'
***         KEYGUID_STR                        = ' '
***         READ_CHANGEDOCU                    = ' '
***         TIME_ZONE                          = ' '
***       IMPORTING
***         ET_CDRED_STR                       =
**          TABLES
**            editpos                          = lt_cddetails
**       EXCEPTIONS
**           no_position_found                 = 1
**           wrong_access_to_archive           = 2
**           time_zone_conversion_error        = 3
**         OTHERS                              = 4
**                  .
**        IF sy-subrc <> 0.
*** Implement suitable error handling here
**        ENDIF.
**        APPEND LINES OF lt_cddetails TO gt_cddetails_final.
**        REFRESH: lt_cddetails.
**      ENDLOOP.
**    ENDLOOP.
**  ENDIF.
**
**ENDFORM.                    " CHANGE_DOCUMENTS_DETAILS_READ
*&---------------------------------------------------------------------*
*&      Form  CDOC_TABLES_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM cdoc_tables_get .
*  FIELD-SYMBOLS: <lwa_tcdob> TYPE t_tcdob,
*                 <lwa_gcdobjac> LIKE LINE OF gt_gcdobjact.
*
*  DATA : lwa_gcdobjac LIKE LINE OF gt_gcdobjact,
*         lt_table_info  TYPE TABLE OF dfies,
*         lt_tcdob TYPE TABLE OF t_tcdob,
*         lv_tabname_txt(60) TYPE c,
*         lwa_ddtypedesc LIKE LINE OF gt_ddtypedesc.
*
******Select tables related to change document object
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_tcdob FROM tcdob WHERE
*                                     object    = c_object-change_documents OR
*                                     object    = c_object-change_documents_fa OR
*                                     object    = c_object-change_documents_iflo.
*  IF gt_tcdob IS INITIAL.
*    MESSAGE i001(/agri/gcdoc).
*  ENDIF.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_gcdobjact FROM /agri/tgcdobjact
*             WHERE objectclas = c_object-change_documents OR
*                   objectclas = c_object-change_documents_fa OR
*                   objectclas = c_object-change_documents_iflo .
*
******Get description of tables selected
*  LOOP AT gt_tcdob ASSIGNING <lwa_tcdob>.
*
*    READ TABLE gt_gcdobjact ASSIGNING <lwa_gcdobjac> WITH KEY tabname = <lwa_tcdob>-tabname.
*    IF sy-subrc EQ 0 AND <lwa_gcdobjac>-descr IS INITIAL.
*      PERFORM cdoc_table_descr_get USING <lwa_gcdobjac>-tabname
*                                    CHANGING lv_tabname_txt.
*      <lwa_gcdobjac>-descr = lv_tabname_txt.
*    ELSE.
*      MOVE-CORRESPONDING <lwa_tcdob> TO lwa_gcdobjac .
*      PERFORM cdoc_table_descr_get USING lwa_gcdobjac-tabname
*                                   CHANGING lv_tabname_txt.
*      lwa_gcdobjac-descr = lv_tabname_txt.
*      APPEND lwa_gcdobjac TO  gt_gcdobjact .
*    ENDIF.
*    READ TABLE gt_cddetails_final WITH KEY tabname = <lwa_tcdob>-tabname
*                 TRANSPORTING NO FIELDS.
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'DDIF_FIELDINFO_GET'
*        EXPORTING
*          tabname        = <lwa_tcdob>-tabname
*        TABLES
*          dfies_tab      = lt_table_info
**         FIXED_VALUES   =
*        EXCEPTIONS
*          not_found      = 1
*          internal_error = 2
*          OTHERS         = 3.
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      lwa_ddtypedesc-typename = <lwa_tcdob>-tabname.
*      lwa_ddtypedesc-fields   =  lt_table_info.
*      APPEND lwa_ddtypedesc TO gt_ddtypedesc.
*      APPEND <lwa_tcdob> TO lt_tcdob.
*    ENDIF.
*
*  ENDLOOP.
*  gt_tcdob = lt_tcdob.
*
*ENDFORM.                    " CDOC_TABLES_GET
*&---------------------------------------------------------------------*
*&      Form  CDOC_TABLE_DESCR_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_GCDOBJAC>_TABNAME  text
*      <--P_LV_TABNAME_TXT  text
*----------------------------------------------------------------------*
FORM cdoc_table_descr_get  USING    lv_tabname
                           CHANGING lv_tabname_txt.
  CASE lv_tabname .
    WHEN '/AGRI/GLFLOT'.
      lv_tabname_txt = TEXT-035.
    WHEN 'IFLOT'.
      lv_tabname_txt = TEXT-036.
    WHEN 'ADRC'.
      lv_tabname_txt = TEXT-037.
    WHEN 'ILOA'.
      lv_tabname_txt = TEXT-038.
*{   REPLACE        SS8K900110                                        1
*\    WHEN 'IFLOTX'.
    WHEN 'IFLOTX' OR '/AGRI/GLFLOTX'.
*}   REPLACE
      lv_tabname_txt = TEXT-039.
    WHEN 'IHPAVB'.
      lv_tabname_txt = TEXT-040.
    WHEN '/AGRI/GLFLATG'.
      lv_tabname_txt = TEXT-041.
    WHEN '/AGRI/GLFLATV'.
      lv_tabname_txt = TEXT-042.
    WHEN '/AGRI/GLFLCMA'.
      lv_tabname_txt = TEXT-043.
    WHEN '/AGRI/GLFLOWN'.
      lv_tabname_txt = TEXT-044.
  ENDCASE.
ENDFORM.                    " CDOC_TABLE_DESCR_GET
*&---------------------------------------------------------------------*
*&      Form  CLASSES_STYLES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM classes_styles_prepare CHANGING lwa_flatg_fcat TYPE
                                                /agri/s_glflatg_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  IF lwa_flatg_fcat-class IS NOT INITIAL.
    lwa_style-fieldname = 'CLASS'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_flatg_fcat-styles.
  ENDIF.

ENDFORM.                    " CLASSES_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  CHECK_PLANT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_plant_all CHANGING lwa_flhdr TYPE /agri/s_glflot.
*                              lwa_iflot TYPE /agri/s_gliflot
*                              lwa_ifloa TYPE /agri/s_gliloa.

*---Replace Unreleased Interfaces
*  CALL FUNCTION 'ITOB_CHECK_PLANT_ALL'
*    EXPORTING
*      swerk_imp         = lwa_flhdr-swerk
*      iwerk_imp         = lwa_flhdr-iwerk
**     wergw_imp         = /agri/s_gliflot-wergw
*      gsber_imp         = lwa_flhdr-gsber
**     USE_BUF           = 'X'
**     DIALOG_MODE       = 'X'
**     DIALOG_CURSOR     = ' '
**     DIALOG_STEPL      = 0
**     INIT_MESSAGE_DATA = 'X'
**     X_MESS_TYPE       = 'E'
*      check_auth_tcode  = c_tcode-funloc
*    IMPORTING
**     T001W_S_EXP       =
*      bukrs_s_exp       = lwa_flhdr-bukrs
*      kokrs_s_exp       = lwa_flhdr-kokrs
*      iwerk_exp         = lwa_flhdr-iwerk
**     T001W_I_EXP       =
**     BUKRS_I_EXP       =
**     KOKRS_I_EXP       =
**     T399I_EXP         =
**     T001W_W_EXP       =
**     bukrs_w_exp       =
**     kokrs_w_exp       =
*    EXCEPTIONS
*      empty_key         = 1
*      application_error = 2
*      OTHERS            = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

  CALL FUNCTION '/AGRI/G_ITOB_CHECK_PLANT_ALL'
    EXPORTING
     i_swerk_imp               = lwa_flhdr-swerk
     I_IWERK_IMP               = lwa_flhdr-iwerk
*     wergw_imp         = /agri/s_gliflot-wergw
     I_GSBER_IMP               = lwa_flhdr-gsber
*     I_USE_BUF                 = 'X'
*     I_DIALOG_MODE             = 'X'
*     I_DIALOG_CURSOR           = ' '
*     I_DIALOG_STEPL            = 0
*     I_INIT_MESSAGE_DATA       = 'X'
*     I_X_MESS_TYPE             = 'E'
     I_CHECK_AUTH_TCODE        = c_tcode-funloc
   IMPORTING
     E_BUKRS_S                 = lwa_flhdr-bukrs
     E_KOKRS_S                 = lwa_flhdr-kokrs
     E_IWERK                   = lwa_flhdr-iwerk
   EXCEPTIONS
     EMPTY_KEY                 = 1
     APPLICATION_ERROR         = 2
     OTHERS                    = 3
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


*---

ENDFORM.                    " CHECK_PLANT_ALL
*&---------------------------------------------------------------------*
*&      Form  CLASSES_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM classes_data_check  USING    lwa_flatg TYPE /agri/s_glflatg
                         CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_cnt TYPE i.

  CLEAR: lv_subrc.
  LOOP AT gt_flatg_fcat TRANSPORTING NO FIELDS
                        WHERE class EQ lwa_flatg-class.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '005' INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK lv_subrc IS INITIAL.

ENDFORM.                    " CLASSES_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_DIMENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_dimension  USING    lv_uom       TYPE msehi
                               lv_dimension TYPE dimid
                               lv_display_message
                      CHANGING lv_subrc     TYPE sy-subrc.

  CHECK lv_uom IS NOT INITIAL.

  CALL FUNCTION 'DIMENSION_CHECK'
    EXPORTING
*     BUSINESS_UOM           = ' '
      dimid                  = lv_dimension
*     LANGUAGE               = SY-LANGU
      msehi                  = lv_uom
*     UNIT_EXTERN_LONG       = ' '
    EXCEPTIONS
      dimension_check_failed = 1
      unit_not_valid         = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    IF lv_display_message IS NOT INITIAL.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '044'
         WITH lv_dimension
         INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_DIMENSION
*&---------------------------------------------------------------------*
*&      Form  CUSTOMER_DATA_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM customer_data_export .
  PERFORM badi_reference_cs_get USING gs_fldoc_infocus-x-flhdr-fltyp.
  CALL BADI ref_badi_glfl_cs->customer_data_export
    EXPORTING
      i_mode   = gs_variables-document_mode
      is_flhdr = gs_fldoc_infocus-x-flhdr
*     is_iflot = gs_fldoc_infocus-x-iflot
*     is_iloa  = gs_fldoc_infocus-x-iloa
      it_ihpa  = gs_fldoc_infocus-x-ihpa
      it_flppl = gs_fldoc_infocus-x-flppl
      it_flown = gs_fldoc_infocus-x-flown.
ENDFORM.                    " CUSTOMER_DATA_EXPORT
*&---------------------------------------------------------------------*
*&      Form  CUSTOMER_DATA_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM customer_data_import .
  DATA: ls_flhdr        TYPE /agri/s_glflot,
        ls_add_data     TYPE /agri/s_glflotca,
        ls_add_data_tmp TYPE /agri/s_glflotca.

  FIELD-SYMBOLS: <lwa_additional_data> TYPE /agri/s_abgl_user_scrfields,
                 <lv_field_value>      TYPE any.

  PERFORM badi_reference_cs_get USING gs_fldoc_infocus-x-flhdr-fltyp.

  CHECK gs_variables-document_mode NE c_mode_display.

  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO ls_add_data.

  CALL BADI ref_badi_glfl_cs->customer_data_import
    EXPORTING
      i_mode   = gs_variables-document_mode
    IMPORTING
      es_flhdr = ls_flhdr.

  MOVE-CORRESPONDING ls_flhdr TO ls_add_data_tmp.
  IF NOT ls_flhdr IS INITIAL AND ls_add_data NE ls_add_data_tmp.
    MOVE-CORRESPONDING ls_add_data_tmp TO gs_fldoc_infocus-x-flhdr.
****Refresh Additional Data 1
    LOOP AT gt_additional_data ASSIGNING <lwa_additional_data>.
      ASSIGN COMPONENT <lwa_additional_data>-fieldname
      OF STRUCTURE gs_fldoc_infocus-x-flhdr TO <lv_field_value>.
      IF sy-subrc EQ 0 .
        <lwa_additional_data>-fieldval = <lv_field_value>.
      ENDIF.
    ENDLOOP.
    PERFORM field_value_conversions USING '2'.
    gs_variables-refresh_additional_data_grid = c_true.

    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
    gs_variables-data_changed = c_true.
  ENDIF.
ENDFORM.                    " CUSTOMER_DATA_IMPORT
*&---------------------------------------------------------------------*
*& Form CHILD_NODES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_FLDOC_INFOCUS_TPLNR_FL
*&      --> LT_GLFLOT_BUFFER
*&      <-- LT_DOCFLOW_TREE
*&      <-- LV_LEVEL
*&---------------------------------------------------------------------*
FORM child_nodes_get USING lv_tplnr
                           lt_glflot_buffer TYPE /agri/t_glflot
                  CHANGING lt_child_nodes   TYPE /agri/t_glfldfl_fcat
                           lv_level.

  DATA: lt_child_nodes_tmp TYPE /agri/t_glfldfl_fcat,
        lwa_child_node     TYPE /agri/s_glfldfl_fcat,
        lwa_glflot_buffer  TYPE /agri/s_glflot,
        lv_tabix           TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_child_node> TYPE /agri/s_glfldfl_fcat.

  ADD 1 TO lv_level.
  LOOP AT lt_glflot_buffer INTO lwa_glflot_buffer
                          WHERE tplma = lv_tplnr.

    MOVE-CORRESPONDING lwa_glflot_buffer TO lwa_child_node.
    lwa_child_node-level     = lv_level.
    lwa_child_node-tplnr_txt = lwa_glflot_buffer-strno.
    lwa_child_node-tplma_txt = lwa_glflot_buffer-tplma.

    SHIFT lwa_child_node-tplma_txt LEFT DELETING LEADING '0'.
    SHIFT lwa_child_node-tplnr_txt LEFT DELETING LEADING '0'.
    APPEND lwa_child_node TO lt_child_nodes.

  ENDLOOP.

  LOOP AT lt_child_nodes ASSIGNING <lwa_child_node>
                             WHERE level EQ lv_level.

    IF <lwa_child_node>-tplnr_fl EQ <lwa_child_node>-tplma.
      CONTINUE.
    ENDIF.
****
    lv_tabix = sy-tabix + 1.
    REFRESH lt_child_nodes_tmp.
    PERFORM child_nodes_get USING <lwa_child_node>-tplnr_fl
                                  lt_glflot_buffer
                         CHANGING lt_child_nodes_tmp
                                  lv_level.

    IF NOT lt_child_nodes_tmp[] IS INITIAL.
      INSERT LINES OF lt_child_nodes_tmp INTO lt_child_nodes
      INDEX lv_tabix.
      <lwa_child_node>-hashier = c_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
