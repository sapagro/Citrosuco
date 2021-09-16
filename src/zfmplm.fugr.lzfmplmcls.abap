*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMCLS .
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

               on_f4_request_items FOR EVENT onf4
                                  OF /agri/cl_gui_alv_grid
                                  IMPORTING e_fieldname e_fieldvalue
                                            es_row_no er_event_data
                                            et_bad_cells e_display,

               on_hotspot_click_items FOR EVENT hotspot_click
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

               on_toolbar_grid FOR EVENT toolbar
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
    DATA : lv_view     TYPE i,
            lwa_button TYPE stb_button.

    lv_view = ref_worklist->view_in_focus_get( ).

    IF lv_view EQ 2.

      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search_more icon_search_next
                            text-048 space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search icon_search text-047
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
**    DATA: lv_view                 TYPE i,
**          lwa_selected_doc        LIKE LINE OF gt_selected_docs,
**          lwa_worklist_header     LIKE LINE OF gt_worklist_header,
**          lv_acnum                TYPE zfmacnum.
**
**    CLEAR gt_selected_docs.
**    REFRESH: gt_selected_docs.
**
**    IF lv_view IS INITIAL.
**      lv_view = ref_worklist->view_in_focus_get( ).
**    ENDIF.
**
**    IF lv_view EQ 1.
**      READ TABLE gt_worklist_header INTO lwa_worklist_header
**                                 INDEX e_row_id-index.
**      lv_acnum = lwa_worklist_header-acnum.
**    ELSE.
**      READ TABLE gt_search_header INTO lwa_worklist_header
**                               INDEX e_row_id-index.
**      lv_acnum = lwa_worklist_header-acnum.
**    ENDIF.
**
**    CHECK lv_acnum NE gs_acdoc_infocus-acnum.
**    CHECK NOT lwa_worklist_header IS INITIAL.
**
**    lwa_selected_doc-acnum = lv_acnum.
**    APPEND lwa_selected_doc TO gt_selected_docs.
**
**    CALL METHOD cl_gui_cfw=>set_new_ok_code
**      EXPORTING
**        new_code = c_fcode-worklist_hotspot.
**
**    PERFORM fcode_/agri/min.
  ENDMETHOD.                    "on_hotspot_click_wl

  METHOD on_data_changed_items.
*    gs_variables-attr_manual_changes = c_true.
*    CALL METHOD cl_gui_cfw=>set_new_ok_code
*      EXPORTING
*        new_code = c_fcode-enter.
  ENDMETHOD.                    "on_data_changed_attr
  METHOD on_hotspot_click_items.

**    DATA: lwa_attr_value TYPE /agri/s_FMACatv_fcat.
**
**    READ TABLE gt_attr_values
**    INTO lwa_attr_value
**    INDEX es_row_no-row_id.
**
**    CHECK sy-subrc EQ 0.
**
**    PERFORM attributes_navigate USING lwa_attr_value.

  ENDMETHOD.                    "on_hotspot_click_attr
  METHOD on_f4_request_items.

**    DATA: lwa_attr_value TYPE /agri/s_FMACatv_fcat,
**          lwa_attr_value2 TYPE /agri/s_FMACatv_fcat,
**          lwa_modified_values TYPE lvc_s_modi.
**
**    DATA: lv_value TYPE atwrt,
**          lv_ivact TYPE /agri/glivact,
**          lv_change,
**          lv_subrc TYPE sy-subrc,
**          lv_tabix TYPE sy-tabix,
**          lwa_athdr  TYPE /agri/s_gathdr,
**          lwa_athdr2 TYPE /agri/s_gathdr,
**          lwa_cskey  TYPE /agri/s_glcs_key.
**
**    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi.
**
**    READ TABLE gt_attr_values INTO lwa_attr_value
**    INDEX es_row_no-row_id.
**
**    CHECK sy-subrc EQ 0.
**    lv_tabix  = es_row_no-row_id.
**
**    READ TABLE gt_athdr INTO lwa_athdr
**                        WITH KEY atinn = lwa_attr_value-atinn
**                      BINARY SEARCH.
**    CHECK sy-subrc EQ 0.
**
**    IF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**       lwa_athdr-atfel EQ 'CHARG_IN'.
**      PERFORM batches_f4_values_get USING e_display
**                                          c_inventory_category-tree
**                                 CHANGING lv_value
**                                          lv_change
**                                          lv_subrc.
**    ELSEIF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**           lwa_athdr-atfel EQ 'CHARG_INF'.
**      PERFORM batches_f4_values_get USING e_display
**                                          c_inventory_category-fruit
**                                 CHANGING lv_value
**                                          lv_change
**                                          lv_subrc.
**    ELSEIF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**           lwa_athdr-atfel EQ 'IVACT'.
**      PERFORM inv_activity_f4_values_get USING e_display
**                                      CHANGING lv_value
**                                               lv_change
**                                               lv_subrc.
**    ELSE.
**      PERFORM attributes_f4_values_get USING lwa_athdr
**                                             e_display
**                                    CHANGING lv_value
**                                             lv_change
**                                             lv_subrc.
**    ENDIF.
**
**    IF lv_change EQ c_true.
**      ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
**      lwa_modified_values-row_id = es_row_no-row_id.
**      lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
**      lwa_modified_values-fieldname = e_fieldname.
**      lwa_modified_values-value = lv_value.
**      APPEND lwa_modified_values TO <lt_modified_values>.
**
**      gs_variables-attr_manual_changes = c_true.
**
**      CALL METHOD cl_gui_cfw=>set_new_ok_code
**        EXPORTING
**          new_code = c_fcode-enter.
**    ENDIF.
**
**    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request_attr
  METHOD on_hotspot_click_latest.

***    DATA: lwa_latest_value TYPE /agri/s_FMAC_list_fcat,
***          lwa_attr_value TYPE /agri/s_FMACatv_fcat.
***
***    READ TABLE gt_latest_values
***    INTO lwa_latest_value
***    INDEX es_row_no-row_id.
***
***    CHECK sy-subrc EQ 0.
***    MOVE-CORRESPONDING lwa_latest_value TO lwa_attr_value.
***    PERFORM attributes_navigate USING lwa_attr_value.

  ENDMETHOD.                    "on_hotspot_click_latest

  METHOD on_f4_request.

**    DATA: lwa_fcat  TYPE lvc_s_fcat,
**          lwa_modified_values TYPE lvc_s_modi.
**
**    DATA: lv_value  TYPE atwrt,
**          lv_change,
**          lv_subrc  TYPE sy-subrc,
**          lv_tabix  TYPE sy-tabix,
**          lwa_athdr TYPE /agri/s_gathdr,
**          lwa_cskey TYPE /agri/s_glcs_key.
**
**    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi,
**                   <lwa_acdata> TYPE any.
**
**    READ TABLE gt_fcat INTO lwa_fcat
**                       WITH KEY fieldname = e_fieldname.
**    CHECK sy-subrc EQ 0.
**    lv_tabix  = es_row_no-row_id.
**
**    READ TABLE gt_athdr INTO lwa_athdr
**                        WITH KEY atinn = lwa_fcat-scrtext_s
**                      BINARY SEARCH.
**    CHECK sy-subrc EQ 0.
**
**    IF lwa_athdr-attab EQ '/AGRI/GLAMHDR'.
**      READ TABLE <gt_acdata> ASSIGNING <lwa_acdata>
**                           INDEX lv_tabix.
**      IF sy-subrc EQ 0.
**        MOVE-CORRESPONDING <lwa_acdata> TO lwa_cskey.
**        CLEAR /agri/s_glflcma.
**        SELECT SINGLE * FROM /agri/glflcma
**          INTO CORRESPONDING FIELDS OF /agri/s_glflcma
**         WHERE tplnr_fl EQ lwa_cskey-tplnr_fl
**           AND contr    EQ lwa_cskey-contr.
**      ENDIF.
**    ENDIF.
**
**    IF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**       lwa_athdr-atfel EQ 'CHARG_IN'.
**      PERFORM batches_f4_values_get USING e_display
**                                          c_inventory_category-tree
**                                 CHANGING lv_value
**                                          lv_change
**                                          lv_subrc.
**    ELSEIF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**           lwa_athdr-atfel EQ 'CHARG_INF'.
**      PERFORM batches_f4_values_get USING e_display
**                                          c_inventory_category-fruit
**                                 CHANGING lv_value
**                                          lv_change
**                                          lv_subrc.
**    ELSEIF lwa_athdr-attab EQ '/AGRI/GLAMHDR' AND
**           lwa_athdr-atfel EQ 'IVACT'.
**      PERFORM inv_activity_f4_values_get USING e_display
**                                      CHANGING lv_value
**                                               lv_change
**                                               lv_subrc.
**    ELSE.
**      PERFORM attributes_f4_values_get USING lwa_athdr
**                                             e_display
**                                    CHANGING lv_value
**                                             lv_change
**                                             lv_subrc.
**    ENDIF.
**    IF lv_change EQ c_true.
**      ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
**      lwa_modified_values-row_id = es_row_no-row_id.
**      lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
**      lwa_modified_values-fieldname = e_fieldname.
**      lwa_modified_values-value = lv_value.
**      APPEND lwa_modified_values TO <lt_modified_values>.
**
**      gs_variables-attr_manual_changes = c_true.
**
**      CALL METHOD cl_gui_cfw=>set_new_ok_code
**        EXPORTING
**          new_code = c_fcode-enter.
**    ENDIF.
**
**    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request

  METHOD on_toolbar_grid.

**    DATA: lwa_button TYPE stb_button.
**
**    CASE sender.
**      WHEN ref_grid_items.
**        CLEAR e_object->mt_toolbar.
**      WHEN ref_grid_attributes_mass.
**        IF gs_variables-document_mode NE c_mode_display.
**          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
**                                space space space.
**          toolbar_button_insert e_object->mt_toolbar lwa_button space
**                      c_fcode-select_compute icon_change_number
**                      text-012 space.
**          toolbar_button_insert e_object->mt_toolbar lwa_button space
**                      c_fcode-mass_doc_delete icon_delete_row
**                      text-006 space.
**        ENDIF.
**      WHEN ref_grid_mdlist_display.
**        toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
**                      space space space.
**        toolbar_button_insert e_object->mt_toolbar lwa_button space
**                            c_fcode-attr_val_descr_switch
**                            icon_active_inactive text-030 space.
**      WHEN OTHERS.
**    ENDCASE.
***
  ENDMETHOD.                      "on_toolbar_grid
  METHOD on_user_command_grid.

**    REFRESH gt_selected_rows.
**
**    IF e_ucomm EQ c_fcode-mass_doc_delete.
**      CALL METHOD ref_grid_attributes_mass->get_selected_rows
**        IMPORTING
**          et_index_rows = gt_selected_rows.
**      IF NOT gt_selected_rows IS INITIAL.
******Desc Layout Row Delete
**        CALL METHOD cl_gui_cfw=>set_new_ok_code
**          EXPORTING
**            new_code = c_fcode-mass_doc_delete.
**
**      ELSEIF gt_selected_rows IS INITIAL.
******Please select a row
**        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
**        message_simple space.
**        e_ucomm = c_fcode-enter.
**      ENDIF.
**    ENDIF.
**
**    CALL METHOD cl_gui_cfw=>set_new_ok_code
**      EXPORTING
**        new_code = e_ucomm.
**
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

**    DATA: ls_fcat TYPE bal_s_fcat.
**
**    IF eref_event_data->ms_display_profile-end_col LT 110.
**      eref_event_data->ms_display_profile-end_col = 110.
**    ENDIF.
**
**    IF eref_event_data->ms_display_profile-end_row LT 50.
**      eref_event_data->ms_display_profile-end_row = 50.
**    ENDIF.
**
**    IF eref_event_data->mv_initiator EQ c_log_initiator-save.
**
**      READ TABLE eref_event_data->ms_display_profile-mess_fcat
**                         WITH KEY ref_field = 'acnum' TRANSPORTING NO FIELDS.
**      IF sy-subrc NE 0.
**        ls_fcat-ref_table  = '/AGRI/S_FMAC_CONTEXT'.
**        ls_fcat-ref_field  = 'acnum'.
**        ls_fcat-outputlen  = 25.
**        ls_fcat-col_pos    = 1.
**        APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**      ENDIF.
**
**    ELSE.
**
**      IF /agri/1899SC_FMACHDR-aslvl EQ c_measurement_level-terrain.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**        WITH KEY ref_field = 'TPLNR_FL' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'TPLNR_FL'.
**          ls_fcat-outputlen  = 25.
**          ls_fcat-col_pos = 1.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**      ELSEIF /agri/1899SC_FMACHDR-aslvl EQ c_measurement_level-crop_seasons
**          OR /agri/1899SC_FMACHDR-aslvl EQ c_measurement_level-harvest.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**              WITH KEY ref_field = 'TPLNR_FL' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'TPLNR_FL'.
**          ls_fcat-outputlen  = 25.
**          ls_fcat-col_pos = 1.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**        WITH KEY ref_field = 'CMNUM' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          READ TABLE eref_event_data->ms_display_profile-mess_fcat
**          INTO ls_fcat WITH KEY col_pos = 2.
**          IF sy-subrc EQ 0.
**            ls_fcat-col_pos = 3.
**            MODIFY eref_event_data->ms_display_profile-mess_fcat FROM ls_fcat
**                                          INDEX sy-tabix TRANSPORTING col_pos.
**          ENDIF.
**
**          CLEAR ls_fcat.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'CMNUM'.
**          ls_fcat-outputlen  = 10.
**          ls_fcat-col_pos    = 2.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**              WITH KEY ref_field = 'DATAB' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          READ TABLE eref_event_data->ms_display_profile-mess_fcat
**          INTO ls_fcat WITH KEY col_pos = 3.
**          IF sy-subrc EQ 0.
**            ls_fcat-col_pos = 4.
**            MODIFY eref_event_data->ms_display_profile-mess_fcat FROM ls_fcat
**                                          INDEX sy-tabix TRANSPORTING col_pos.
**          ENDIF.
**          CLEAR ls_fcat.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'DATAB'.
**          ls_fcat-outputlen  = 10.
**          ls_fcat-col_pos    = 3.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**        WITH KEY ref_field = 'DATBI' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          READ TABLE eref_event_data->ms_display_profile-mess_fcat
**          INTO ls_fcat WITH KEY col_pos = 4.
**          IF sy-subrc EQ 0.
**            ls_fcat-col_pos = 5.
**            MODIFY eref_event_data->ms_display_profile-mess_fcat FROM ls_fcat
**                                          INDEX sy-tabix TRANSPORTING col_pos.
**          ENDIF.
**          CLEAR ls_fcat.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'DATBI'.
**          ls_fcat-outputlen  = 10.
**          ls_fcat-col_pos    = 4.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**      ELSEIF /agri/1899SC_FMACHDR-aslvl EQ c_measurement_level-irrigation.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**        WITH KEY ref_field = 'EQUNR' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'EQUNR'.
**          ls_fcat-outputlen  = 25.
**          ls_fcat-col_pos = 1.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**        READ TABLE eref_event_data->ms_display_profile-mess_fcat
**        WITH KEY ref_field = 'TPLNR_FL' TRANSPORTING NO FIELDS.
**        IF sy-subrc NE 0.
**          READ TABLE eref_event_data->ms_display_profile-mess_fcat
**          INTO ls_fcat WITH KEY col_pos = 2.
**          IF sy-subrc EQ 0.
**            ls_fcat-col_pos = 3.
**            MODIFY eref_event_data->ms_display_profile-mess_fcat FROM ls_fcat
**                                          INDEX sy-tabix TRANSPORTING col_pos.
**          ENDIF.
**          CLEAR ls_fcat.
**          ls_fcat-ref_table  = 'ZSC_FMAC_CONTEXT'.
**          ls_fcat-ref_field  = 'TPLNR_FL'.
**          ls_fcat-outputlen  = 30.
**          ls_fcat-col_pos    = 2.
**          APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
**        ENDIF.
**
**      ENDIF.
**
**    ENDIF.

  ENDMETHOD.                    "on_log_display_profile

ENDCLASS.                    "lcl_log_handler IMPLEMENTATION
