*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMCLS .
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
****Worklist
    METHODS : on_toolbar_wl FOR EVENT toolbar
                  OF /agri/cl_worklist_container
      IMPORTING e_object e_interactive,

      on_user_command_wl FOR EVENT user_command
                    OF /agri/cl_worklist_container
        IMPORTING e_ucomm,

      on_view_changed FOR EVENT view_changed
                    OF /agri/cl_worklist_container
        IMPORTING e_view,

      on_hotspot_click_wl FOR EVENT hotspot_click
                    OF /agri/cl_worklist_container
        IMPORTING e_row_id e_column_id
                    es_row_no,

*****Grid
      on_data_changed_items FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after,

      on_data_changed_rcvrs FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after,

      on_data_changed_rcbom FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after,

      on_f4_request_items FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

      on_f4_request_rcvrs FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

      on_f4_request_rcbom FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

      on_hotspot_click_items FOR EVENT hotspot_click
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_row_id e_column_id
                    es_row_no,

      on_hotspot_click_rcvrs FOR EVENT hotspot_click
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_row_id e_column_id
                    es_row_no,

      on_hotspot_click_latest FOR EVENT hotspot_click
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_row_id e_column_id
                    es_row_no,

      on_f4_request FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_user_command_grid_rcvrs FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_toolbar_grid FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,

      on_toolbar_grid_rcvrs FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,

      on_user_command_grid_rcbom FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_toolbar_grid_rcbom FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive sender.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
****Worklist
****Toolbar
  METHOD on_toolbar_wl.
    DATA : lv_view    TYPE i,
           lwa_button TYPE stb_button.

    lv_view = ref_worklist->view_in_focus_get( ).

    IF lv_view EQ 2.

      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search_more icon_search_next
                            TEXT-048 space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search icon_search TEXT-047
                            space.
    ENDIF.

  ENDMETHOD.                    "on_toolbar_wl

** on-user command..
  METHOD on_user_command_wl.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.
  ENDMETHOD.                    "on_user_command_wl

****ON VIEW CHANGE
  METHOD on_view_changed.
    PERFORM worklist_refresh USING e_view.
  ENDMETHOD.                    "on_view_changed

****ON HOTSPOT CLICK
  METHOD on_hotspot_click_wl.
    DATA: lv_view             TYPE i,
          lwa_selected_doc    LIKE LINE OF gt_selected_docs,
          lwa_worklist_header LIKE LINE OF gt_worklist_header,
          lv_rcnum            TYPE zfmrcnum.

    CLEAR gt_selected_docs.
    REFRESH: gt_selected_docs.

    IF lv_view IS INITIAL.
      lv_view = ref_worklist->view_in_focus_get( ).
    ENDIF.

    IF lv_view EQ 1.
      READ TABLE gt_worklist_header INTO lwa_worklist_header
                                 INDEX e_row_id-index.
      lv_rcnum = lwa_worklist_header-rcnum.
    ELSE.
      READ TABLE gt_search_header INTO lwa_worklist_header
                               INDEX e_row_id-index.
      lv_rcnum = lwa_worklist_header-rcnum.
    ENDIF.

    CHECK lv_rcnum NE gs_rcdoc_infocus-rcnum.
    CHECK NOT lwa_worklist_header IS INITIAL.

    lwa_selected_doc-rcnum = lv_rcnum.
    APPEND lwa_selected_doc TO gt_selected_docs.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-worklist_hotspot.

    PERFORM fcode_/agri/min.
  ENDMETHOD.                    "on_hotspot_click_wl

  METHOD on_data_changed_items.

**    IF sy-uname EQ 'T_T.KONNO'.
**      BREAK-POINT.
**    ENDIF.

    REFRESH: gt_dose_modi.

    gs_variables-manual_changes = c_true.
    APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_dose_modi .
    SORT gt_dose_modi BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_dose_modi COMPARING row_id.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_attr

  METHOD on_data_changed_rcvrs.

    REFRESH: gt_rcvrs_modi.

    gs_variables-manual_changes = c_true.
    APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_rcvrs_modi .
    SORT gt_rcvrs_modi BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_rcvrs_modi COMPARING row_id.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.

  METHOD on_data_changed_rcbom.

    REFRESH: gt_rcbom_modi.

    gs_variables-manual_changes = c_true.
    APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_rcbom_modi.
    SORT gt_rcbom_modi BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_rcbom_modi COMPARING row_id.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_attr
  METHOD on_hotspot_click_items.


    DATA: lwa_dose_modi LIKE LINE OF gt_dose_modi.

    REFRESH: gt_dose_modi.

    gs_variables-manual_changes = c_true.
    lwa_dose_modi-fieldname = e_column_id.
    lwa_dose_modi-row_id = e_row_id-index.
    APPEND lwa_dose_modi TO gt_dose_modi.
    SORT gt_dose_modi BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_dose_modi COMPARING row_id.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_hotspot_click_attr

  METHOD on_hotspot_click_rcvrs.


    DATA: lwa_dose_modi LIKE LINE OF gt_dose_modi.

    REFRESH: gt_dose_modi.

    gs_variables-manual_changes = c_true.
    lwa_dose_modi-fieldname = e_column_id.
    lwa_dose_modi-row_id = e_row_id-index.
    APPEND lwa_dose_modi TO gt_dose_modi.
    SORT gt_dose_modi BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_dose_modi COMPARING row_id.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_hotspot_click_attr

  METHOD on_f4_request_items.

    DATA: lwa_rclst_value  TYPE zsc_fmrclst_fcat,
          lwa_rclst_value2 TYPE zsc_fmrclst_fcat.

    DATA: lwa_modified_values TYPE lvc_s_modi,
          lv_change,
          lv_value            TYPE matnr,
          lv_subrc            TYPE sy-subrc,
          lv_tabix            TYPE sy-tabix.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi,
                   <lwa_acdata>         TYPE any.


    IF e_fieldname = 'MATNR_INS'.
      PERFORM material_f4_values_get USING    e_display
                                     CHANGING lv_value
                                              lv_change
                                              lv_subrc.
    ENDIF.

    ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
    lwa_modified_values-row_id = es_row_no-row_id.
    lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
    lwa_modified_values-fieldname = e_fieldname.
    lwa_modified_values-value = lv_value.
    APPEND lwa_modified_values TO <lt_modified_values>.

    gs_variables-items_manual_changes = c_true.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.


    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request_attr

  METHOD on_f4_request_rcvrs.

    DATA: lwa_rclst_value  TYPE zsc_fmrclst_fcat,
          lwa_rclst_value2 TYPE zsc_fmrclst_fcat.

    DATA: lwa_modified_values TYPE lvc_s_modi,
          lv_change,
          lv_alter            TYPE stlal,
          lv_rout             TYPE plnnr,
          lv_value            TYPE char40,
          lv_subrc            TYPE sy-subrc,
          lv_tabix            TYPE sy-tabix.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi,
                   <lwa_acdata>         TYPE any.


    IF e_fieldname = 'STLAL'.
      PERFORM alternative_f4_values_get USING e_display
                                     CHANGING lv_alter
                                              lv_change
                                              lv_subrc.
      MOVE lv_alter TO lv_value.
    ELSEIF e_fieldname = 'PLNNR'.
      PERFORM rout_f4_values_get USING e_display
                              CHANGING lv_rout
                                       lv_change
                                       lv_subrc.
      MOVE lv_rout TO lv_value.
    ENDIF.

    ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
    lwa_modified_values-row_id = es_row_no-row_id.
    lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
    lwa_modified_values-fieldname = e_fieldname.
    lwa_modified_values-value = lv_value.
    APPEND lwa_modified_values TO <lt_modified_values>.

    gs_variables-items_manual_changes = c_true.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.


    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request_attr
  METHOD on_f4_request_rcbom.

**    DATA: lwa_item_value  TYPE  zsc_fmrcnum_fcat,
**          lwa_item_value2 TYPE zsc_fmrcnum_fcat.
**
**    DATA: lwa_modified_values TYPE lvc_s_modi,
**          lv_change,
**          lv_value            TYPE /agri/gltplnr_fl,
**          lv_subrc            TYPE sy-subrc,
**          lv_tabix            TYPE sy-tabix.

  ENDMETHOD.                    "on_f4_request_attr

  METHOD on_hotspot_click_latest.

  ENDMETHOD.                    "on_hotspot_click_latest

  METHOD on_f4_request.

  ENDMETHOD.                    "on_f4_request

  METHOD on_toolbar_grid.

    DATA: lwa_button TYPE stb_button.

    CASE sender.
      WHEN ref_grid_rclst.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                      c_fcode-delete_rows icon_delete_row
                      TEXT-006 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                     c_fcode-bom_create icon_bom_sub_item
                     TEXT-009 space.
        ENDIF.
    ENDCASE.
*
  ENDMETHOD.                      "on_toolbar_grid

  METHOD on_toolbar_grid_rcvrs.

    DATA: lwa_button TYPE stb_button.

    CASE sender.
      WHEN ref_grid_rcvrs.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                     c_fcode-vrs_create icon_create
                     TEXT-016 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                      c_fcode-delete_version icon_delete_row
                      TEXT-053 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                     c_fcode-vrs_modify icon_modify
                     TEXT-017 space.

        ENDIF.
    ENDCASE.
*
  ENDMETHOD.                      "on_toolbar_grid

  METHOD on_toolbar_grid_rcbom.

    DATA: lwa_button TYPE stb_button.

    CASE sender.
      WHEN ref_grid_rcbom.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                      "on_toolbar_grid

  METHOD on_user_command_grid.

    REFRESH gt_selected_rows.

    IF e_ucomm EQ c_fcode-delete_rows.
      CALL METHOD ref_grid_rclst->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-delete_rows.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
        e_ucomm = c_fcode-enter.
      ENDIF.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_grid_user_command

  METHOD on_user_command_grid_rcvrs.

    REFRESH gt_selected_rows.

    IF e_ucomm EQ c_fcode-delete_rows.
      CALL METHOD ref_grid_rcvrs->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-delete_rows.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
        e_ucomm = c_fcode-enter.
      ENDIF.

    ELSEIF e_ucomm EQ c_fcode-vrs_modify.
      CALL METHOD ref_grid_rcvrs->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-vrs_modify.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
        e_ucomm = c_fcode-enter.
      ENDIF.
    ELSEIF e_ucomm EQ c_fcode-delete_version.
      CALL METHOD ref_grid_rcvrs->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-delete_version.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
        e_ucomm = c_fcode-enter.
      ENDIF.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_grid_user_command

  METHOD on_user_command_grid_rcbom.

    REFRESH gt_selected_row_rcbom.

    IF e_ucomm EQ c_fcode-update_mass.
      CALL METHOD ref_grid_rcbom->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_row_rcbom.
      IF NOT gt_selected_row_rcbom IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-update_mass.

      ELSEIF gt_selected_row_rcbom IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
        e_ucomm = c_fcode-enter.
      ENDIF.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_grid_user_command


ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_log_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_log_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_log_display_profile
      FOR EVENT log_display_profile
                  OF /agri/cl_process_log_manager
      IMPORTING eref_event_data.
ENDCLASS.                    "lcl_log_handler DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_log_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_log_handler IMPLEMENTATION.
  METHOD on_log_display_profile.
  ENDMETHOD.                    "on_log_display_profile

ENDCLASS.                    "lcl_log_handler IMPLEMENTATION
