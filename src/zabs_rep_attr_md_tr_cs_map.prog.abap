*&---------------------------------------------------------------------*
*& Report ZABS_REP_ATTR_MD_TR_CS_MAP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_attr_md_tr_cs_map.

***Class
CLASS: lcl_event_handler DEFINITION DEFERRED.

DATA: ref_grid_mat_char      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_mat_char TYPE REF TO cl_gui_custom_container,
      ref_event_handler      TYPE REF TO lcl_event_handler.

DATA: fcode           TYPE sy-ucomm,
      ok_code         TYPE sy-ucomm,
      gv_grid_refresh TYPE xfeld.

DATA: BEGIN OF gs_variables,
        data_change,
        data_saved,
        refresh_batch_char,
        refresh_colum_batch_char,
        overview_mode,
        aufnr                    TYPE aufnr,
      END OF gs_variables.

CONSTANTS: BEGIN OF c_fcode,
             continue                TYPE sy-ucomm VALUE 'CONT',
             enter                   TYPE sy-ucomm VALUE 'ENTR',
             insert_batch_char       TYPE sy-ucomm VALUE 'BINS',
             delete_batch_char       TYPE sy-ucomm VALUE 'BDEL',
             change_colum_batch_char TYPE sy-ucomm VALUE 'BCOL',
             create_cat              TYPE sy-ucomm VALUE 'CRCA',
           END OF c_fcode.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:
      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed_grid FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed sender,

      on_f4_request FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display.

ENDCLASS.                    "lcl_event_handler DEFINITION

START-OF-SELECTION.
  SELECT *
    FROM zabst_md_atr_map
    INTO TABLE @DATA(lt_md_atr_map).
  IF sy-subrc = 0.
    SORT lt_md_atr_map BY erzet DESCENDING erdat DESCENDING.
    SELECT atinn, atnam
      FROM cabn
      INTO TABLE @DATA(lt_atinn)
      FOR ALL ENTRIES IN @lt_md_atr_map
      WHERE atnam EQ @lt_md_atr_map-mdatnam.
    IF sy-subrc EQ 0.
      SORT lt_atinn BY atinn.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& status_set
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& title_set
*&---------------------------------------------------------------------*
FORM title_set .
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

*--Local data declaration
  DATA : lt_toolbar_excludes TYPE ui_functions,
         ls_environment      TYPE /agri/s_glvc_environment,
         ls_layout           TYPE lvc_s_layo,
         lt_fcat             TYPE lvc_t_fcat,
         lv_input            TYPE i,
         ls_variant          TYPE disvariant.

  IF ref_container_mat_char IS NOT INITIAL.

    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ref_grid_mat_char->set_frontend_fieldcatalog( it_fieldcatalog = lt_fcat ).

    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'D'.
    ls_layout-smalltitle = abap_true.

    ref_grid_mat_char->set_frontend_layout( is_layout = ls_layout ).

    ref_grid_mat_char->refresh_table_display( ).

  ELSE.

    CREATE OBJECT ref_container_mat_char
      EXPORTING
        container_name              = 'ATTR_0100_CC'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    ls_environment-switchoff_performance = abap_true.
    CREATE OBJECT ref_grid_mat_char
      EXPORTING
        i_parent           = ref_container_mat_char
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
      RETURN.
    ENDIF.

*--Handle Events
    PERFORM control_events_register.

*--Preparing Field Catalog
    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'D'.
    ls_layout-smalltitle = abap_true.

    ls_variant-report = sy-repid.
    ls_variant-handle = '0100'.

*--Exclude unwanted buttons
    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

*--Display Data
    CALL METHOD ref_grid_mat_char->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = lt_md_atr_map
        it_fieldcatalog               = lt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

*--Register event for Input Capability
  lv_input = 1.
  CALL METHOD ref_grid_mat_char->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

*---Register Edit Event For Grid
  CALL METHOD ref_grid_mat_char->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*& Handle Events
*&---------------------------------------------------------------------*
FORM control_events_register .

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  IF ref_grid_mat_char IS NOT INITIAL.
    CALL METHOD ref_grid_mat_char->register_edit_event
      EXPORTING
        i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ENDIF.

***Register f4 For Fields.
  lwa_f4-fieldname = 'VALUE'.
  lwa_f4-register = abap_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_grid_mat_char->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_user_command_grid
               FOR ref_grid_mat_char,
               ref_event_handler->on_f4_request
               FOR ref_grid_mat_char,
               ref_event_handler->on_data_changed_grid
               FOR ref_grid_mat_char.

ENDFORM.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
* Class Implimentation
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

*--On user Command
  METHOD on_user_command_grid.

*    IF e_ucomm EQ c_fcode-change_colum_batch_char.
*      IF gs_variables-refresh_colum_batch_char EQ abap_false.
*        gs_variables-refresh_colum_batch_char = abap_true.
*      ELSE.
*        CLEAR: gs_variables-refresh_colum_batch_char.
*      ENDIF.
*      gs_variables-refresh_batch_char = abap_true.
*    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_user_command

*--On data change
  METHOD on_data_changed_grid.

    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
      PERFORM handle_data_changed USING er_data_changed.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_grid

*--On F4 Request
  METHOD on_f4_request.

*--Local Data declarations
    DATA: lt_selected_values TYPE tt_cawn,
          lv_value           TYPE atwrt.

* CASE e_fieldname.
*
*      WHEN 'MDATNAM'.
*        lv_shlpname = '/AGRI/CSH_GAT'.
*
*        IF gs_atgrp_environment-omode EQ c_mode_display.
*          lv_display  = c_true.
*        ENDIF.
*
*        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*          EXPORTING
*            tabname           = '/AGRI/S_GKSML_FCAT'
*            fieldname         = 'ATNAM'
*            searchhelp        = lv_shlpname
*            shlpparam         = 'ATNAM'
*            display           = lv_display
*          TABLES
*            return_tab        = lt_return_tab[]
*          EXCEPTIONS ##FM_SUBRC_OK
*            field_not_found   = 1
*            no_help_for_field = 2
*            inconsistent_help = 3
*            no_values_found   = 4
*            OTHERS            = 5.
*
*        READ TABLE lt_return_tab INTO lwa_return_tab INDEX 1.
*
*        IF sy-subrc EQ 0.
*          ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
*          lwa_modified_values-row_id = es_row_no-row_id.
*          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
*          lwa_modified_values-fieldname = e_fieldname.
*          lwa_modified_values-value = lwa_return_tab-fieldval.
*          APPEND lwa_modified_values TO <lt_modified_values>.
*
*          gs_variables-manual_changes = c_true.
*          CALL METHOD cl_gui_cfw=>set_new_ok_code
*            EXPORTING
*              new_code = c_fcode-enter.
*        ENDIF.
*
*    ENDCASE.

    er_event_data->m_event_handled = abap_true.
*--Process data to fill values using f4 help
    READ TABLE lt_md_atr_map ASSIGNING FIELD-SYMBOL(<fs_md_atr_map>)
                                     INDEX es_row_no-row_id.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_atinn INTO DATA(ls_atinn)
    WITH KEY atnam = <fs_md_atr_map>-mdatnam BINARY SEARCH.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    gs_variables-data_change = abap_true.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& handle_data_changed
*&---------------------------------------------------------------------*
FORM handle_data_changed  USING      p_er_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol.

  gs_variables-data_change = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*& toolbar_buttons_exclude
*&---------------------------------------------------------------------*
FORM toolbar_buttons_exclude TABLES lt_toolbar_excludes TYPE ui_functions.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_info      TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.
****Editable Grid Buttons
  APPEND /agri/cl_gui_alv_grid=>mc_fc_info      TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_check     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_refresh   TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_mb_paste     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_append_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy  TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_cut   TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_delete_row
                                               TO lt_toolbar_excludes.
*  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
*                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_move_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste_new_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_undo  TO lt_toolbar_excludes.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& field_catalog_prepare
*&---------------------------------------------------------------------*
FORM field_catalog_prepare  CHANGING lt_fcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS : <lwa_fcat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABST_MD_ATR_MAP'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
    CASE <lwa_fcat>-fieldname.
      WHEN 'MDCLASS' OR 'MDATNAM'.

      WHEN OTHERS.
        <lwa_fcat>-tech = abap_true.
    ENDCASE.
  ENDLOOP.

  IF gs_variables-overview_mode IS INITIAL.
    READ TABLE lt_fcat ASSIGNING <lwa_fcat>
      WITH KEY fieldname = 'TRCLASS'.
    IF sy-subrc = 0.
      <lwa_fcat>-edit = abap_true.
      <lwa_fcat>-f4availabl = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& fcode_processing
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.            "FCODE_PROCESSING

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& fcode_cont
*&---------------------------------------------------------------------*
FORM fcode_canc.
  LEAVE TO SCREEN 0.
ENDFORM.            "FCODE_CANC
