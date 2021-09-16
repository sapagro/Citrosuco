*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLMCLS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    DATA: mv_tabix         TYPE sy-tabix,
          mv_docflow_tabix TYPE sy-tabix.

    TYPES : BEGIN OF t_root_node,
              root_key(30) TYPE c,
            END OF t_root_node.

    DATA : mt_docflow_rootnode TYPE TABLE OF t_root_node,
           mt_layout_rootnode  TYPE TABLE OF t_root_node.

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

      on_f4_request_grid FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

*****Grid
      on_data_changed_grid FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after
                    sender,
      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_toolbar_grid      FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive,

*--attributes grid
      on_f4_request_attr   FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display,

      on_data_changed_attr FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after,
      on_toolbar_owners    FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_data_changed_owner FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after,
      on_value_request FOR EVENT onf4 OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no
                    er_event_data et_bad_cells e_display,

      on_hierarchy_node_add FOR EVENT on_add_hierarchy_node
                    OF /agri/cl_alv_tree_simple_hier
        IMPORTING index_outtab grouplevel,

      on_node_hotspot_click_dfl_tree  FOR EVENT link_click
                    OF /agri/cl_alv_tree_simple_hier
        IMPORTING fieldname
                    index_outtab
                    grouplevel,

      on_node_layout_set   FOR EVENT node_layout_set
                    OF /agri/cl_alv_tree_simple_hier
        IMPORTING e_index e_grouplevel
                    eref_node_layout.

ENDCLASS.                    "lcl_event_handler DEFINITION


**&---------------------------------------------------------------------*
*&       Class LCL_STATUS_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_status_handler DEFINITION.
  PUBLIC SECTION.
    INTERFACES /agri/if_gstat_exit.

    ALIASES: appl_data_get  FOR /agri/if_gstat_exit~appl_data_get,
             status_profile_trigger_set FOR /agri/if_gstat_exit~status_profile_trigger_set,
              status_flow_outcome_set    FOR /agri/if_gstat_exit~status_flow_outcome_set,
              status_flow_step_set       FOR /agri/if_gstat_exit~status_flow_step_set.
ENDCLASS.               "LCL_STATUS_HANDLER


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
           lwa_btnmnu TYPE stb_btnmnu,
           lwa_button TYPE stb_button.
    DATA: lref_ctxmenu TYPE REF TO cl_ctmenu.

    lv_view = ref_worklist->view_in_focus_get( ).

    IF lv_view EQ 2.

      CREATE OBJECT: lref_ctxmenu.

      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.

      menu_function_add lref_ctxmenu c_fcode-mass_header_change
                                     TEXT-021.

      static_context_set e_object->mt_btnmnu lwa_btnmnu
                         lref_ctxmenu c_fcode-enter.

      toolbar_button_insert e_object->mt_toolbar lwa_button 2
                              c_fcode-enter icon_mass_change
                              TEXT-020 space.

      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search_more icon_search_next
                            TEXT-017 space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search icon_search TEXT-016
                            space.
    ENDIF.

  ENDMETHOD.                    "on_toolbar_wl
** on-user command..
  METHOD on_user_command_wl.
    IF e_ucomm EQ c_fcode-mass_header_change.
      PERFORM selected_rows_get.
      IF gt_selected_terrains[] IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.
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
*          lv_infocus_doc_type(20),
          lwa_selected_doc    LIKE LINE OF gt_selected_docs,
          lwa_worklist_header LIKE LINE OF gt_worklist_header,
          lv_tplnr            TYPE /agri/gltplnr_fl.

    CLEAR gt_selected_docs.
    REFRESH: gt_selected_docs.

    IF lv_view IS INITIAL.
      lv_view = ref_worklist->view_in_focus_get( ).
    ENDIF.

    IF lv_view EQ 1.
      READ TABLE gt_worklist_header INTO lwa_worklist_header
                                 INDEX e_row_id-index.
      lv_tplnr = lwa_worklist_header-tplnr_fl.
    ELSE.
      READ TABLE gt_search_header INTO lwa_worklist_header
                               INDEX e_row_id-index.
      lv_tplnr = lwa_worklist_header-tplnr_fl.
    ENDIF.

    CHECK lv_tplnr NE gs_fldoc_infocus-tplnr_fl.
    CHECK NOT lwa_worklist_header IS INITIAL.

    lwa_selected_doc-tplnr_fl = lv_tplnr.
    APPEND lwa_selected_doc TO gt_selected_docs.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-worklist_hotspot.

    PERFORM fcode_/agri/min.
  ENDMETHOD.                    "on_hotspot_click_wl

****Grid
  METHOD on_data_changed_grid.

    gs_variables-manual_changes = c_true.
    REFRESH: gt_class_mod_rows,
             gt_plant_mod_rows.
    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
      IF sender EQ ref_classes_grid."sy-dynnr = c_screen-class_assignment.
        gs_variables-manual_changes = c_true.
        APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_class_mod_rows .
        SORT gt_class_mod_rows BY row_id.
        DELETE ADJACENT DUPLICATES FROM gt_class_mod_rows COMPARING row_id.
      ELSEIF sender EQ ref_multi_lang_desc_grid."sy-dynnr = c_screen-multi_lang_desc.
        gs_variables-manual_changes = c_true.
        APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_desc_mod_rows .
        SORT gt_desc_mod_rows BY row_id.
        DELETE ADJACENT DUPLICATES FROM gt_desc_mod_rows COMPARING row_id.
      ELSEIF sender EQ ref_plants_grid.
        gs_variables-manual_changes = c_true.
        APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_plant_mod_rows .
        SORT gt_plant_mod_rows BY row_id.
        DELETE ADJACENT DUPLICATES FROM gt_plant_mod_rows COMPARING row_id.
      ENDIF.
    ENDIF.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_grid
  "on_data_changed_grid

  METHOD on_data_changed_owner.

    gs_variables-manual_changes = c_true.
    APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_owner_mod_rows .
    SORT gt_owner_mod_rows BY row_id.
    DELETE ADJACENT DUPLICATES FROM gt_owner_mod_rows COMPARING row_id..
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_owner

  METHOD on_user_command_grid.

    REFRESH: gt_selected_rows.

    IF e_ucomm EQ c_fcode-class_delete.
      CALL METHOD ref_classes_grid->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-class_delete.
      ELSEIF gt_selected_rows IS INITIAL.
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ELSEIF e_ucomm EQ c_fcode-owner_delete.
      CALL METHOD ref_owners_grid->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-owner_delete.
      ELSEIF gt_selected_rows IS INITIAL.
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ELSEIF e_ucomm EQ c_fcode-plant_delete.
      CALL METHOD ref_plants_grid->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-plant_delete.
      ELSEIF gt_selected_rows IS INITIAL.
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ELSEIF e_ucomm EQ c_fcode-label_activate.
      CALL METHOD ref_terrain_labels_grid->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-label_activate.
      ELSEIF gt_selected_rows IS INITIAL.
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ELSE.
      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = e_ucomm.
    ENDIF.

  ENDMETHOD.                    "on_user_command_grid
  "on_user_command_grid

  METHOD on_toolbar_grid.

    DATA : lwa_button TYPE stb_button.

    CASE sy-dynnr.
      WHEN c_screen-class_assignment.
        CLEAR e_object->mt_toolbar.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-class_delete icon_delete_row
                                TEXT-019 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-class_insert icon_insert_row
                                TEXT-003 space.
        ENDIF.

      WHEN c_screen-multi_lang_desc.
        CLEAR e_object->mt_toolbar.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-desc_delete icon_delete_row
                                TEXT-019 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-desc_insert icon_insert_row
                                TEXT-003 space.
        ENDIF.

      WHEN c_screen-plants.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                                space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-plant_delete icon_delete_row
                                TEXT-019 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-plant_insert icon_insert_row
                                TEXT-003 space.
        ENDIF.
      WHEN c_screen-terrain_labels.
*        CLEAR e_object->mt_toolbar.
        IF gs_variables-document_mode NE c_mode_display.
          toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                      space space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-label_create icon_create
                                space space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-label_activate
                                icon_activate space space.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                      "on_toolbar_grid

  METHOD on_toolbar_owners.

    DATA : lwa_button TYPE stb_button.

    IF gs_variables-document_mode NE c_mode_display.
      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.
      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-owner_delete icon_delete_row
                            TEXT-017 space.
      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-owner_insert icon_insert_row
                            TEXT-003 space.
    ENDIF.

  ENDMETHOD.                    "on_toolbar_owners

  METHOD on_f4_request_attr.

    DATA: lv_tabix            TYPE sy-tabix,
          lv_atwrt            TYPE atwrt,
          lv_change,
          lwa_modified_values TYPE lvc_s_modi.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi.
    lv_tabix = es_row_no-row_id.

    PERFORM attr_values_f4_get USING lv_tabix
                            CHANGING lv_atwrt
                                     lv_change.
    IF lv_change EQ c_true.

      IF lv_atwrt IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        IF <lt_modified_values> IS ASSIGNED.
          lwa_modified_values-row_id = es_row_no-row_id.
          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
          lwa_modified_values-fieldname = e_fieldname.
          lwa_modified_values-value = lv_atwrt.
          APPEND lwa_modified_values TO <lt_modified_values>.
        ENDIF.
      ENDIF.

*      gs_variables-attr_manual_changes = c_true.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = c_fcode-enter.
    ENDIF.

    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request_attr

  METHOD on_data_changed_attr.
    gs_variables-attr_manual_changes = c_true.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.
  ENDMETHOD.                    "on_data_changed_attr

  METHOD on_f4_request_grid.

    DATA: lv_tabix            TYPE sy-tabix,
          lv_owner            TYPE /agri/glowner,
          lv_change,
          lwa_modified_values TYPE lvc_s_modi.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi.
    lv_tabix = es_row_no-row_id.

    IF e_fieldname = 'OWNER'.

      PERFORM owner_values_get USING space
                                     e_display
                            CHANGING lv_owner.

      IF lv_owner IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        IF <lt_modified_values> IS ASSIGNED.
          lwa_modified_values-row_id = es_row_no-row_id.
          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
          lwa_modified_values-fieldname = e_fieldname.
          lwa_modified_values-value = lv_owner.
          APPEND lwa_modified_values TO <lt_modified_values>.
        ENDIF.
      ENDIF.

*      gs_variables-attr_manual_changes = c_true.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = c_fcode-enter.

      er_event_data->m_event_handled = c_true.

    ENDIF.

  ENDMETHOD.                    "on_f4_request_grid

  METHOD on_value_request.

    DATA: lt_return_values TYPE TABLE OF ddshretval INITIAL SIZE 0,
          lwa_return_value TYPE ddshretval.
    DATA: lwa_additional_data LIKE LINE OF gt_additional_data,
          lwa_modified_values TYPE lvc_s_modi,
          lwa_mass_field      TYPE /agri/s_guserfields_fcat,
          ls_tpar             TYPE tpar,
          lv_retfield         TYPE ddshretval-retfield,
          lv_tabname          TYPE dd02l-tabname,
          lv_fieldname        TYPE fieldname,
          lv_searchhelp       TYPE shlpname,
          lv_display_only.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi.

****Additional data
    READ TABLE gt_additional_data INTO lwa_additional_data
                                       INDEX es_row_no-row_id.
    CHECK sy-subrc EQ 0.
    lv_fieldname = lwa_additional_data-fieldname.
***Single Fields
**    lv_tabname = gs_variables-user_structure.
    lv_tabname = lwa_additional_data-tabname.
    CONCATENATE lv_tabname '-' lwa_additional_data-fieldname
                                   INTO lv_retfield.

    IF gs_variables-document_mode EQ c_mode_display.
      lv_display_only = c_true.
    ENDIF.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = lv_tabname
        fieldname         = lv_fieldname
        searchhelp        = lv_searchhelp
*       SHLPPARAM         = ' '
        dynpprog          = c_program-funloc
        dynpnr            = sy-dynnr
*       dynprofield       = lv_field
*       STEPL             = 0
*       VALUE             = ' '
*       MULTIPLE_CHOICE   = ' '
        display           = lv_display_only
*       SUPPRESS_RECORDLIST = ' '
*       CALLBACK_PROGRAM  = ' '
*       CALLBACK_FORM     = ' '
      TABLES
        return_tab        = lt_return_values[]
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc EQ 0.
      IF lwa_mass_field-fieldname EQ 'OWNER'.
        READ TABLE lt_return_values INTO lwa_return_value
                             WITH KEY fieldname = lv_fieldname.
      ELSE.
        READ TABLE lt_return_values INTO lwa_return_value
                             WITH KEY retfield = lv_retfield.
      ENDIF.
      IF sy-subrc EQ 0.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        lwa_modified_values-row_id = es_row_no-row_id.
        lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
        lwa_modified_values-fieldname = e_fieldname.
        lwa_modified_values-value = lwa_return_value-fieldval.
        APPEND lwa_modified_values TO <lt_modified_values>.

****Get the corresponding description
        lwa_modified_values-row_id = es_row_no-row_id.
        lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
        lwa_modified_values-fieldname = 'FIELDDSCR'.
        IF lv_fieldname EQ 'OWNER'.
          PERFORM owner_text_get USING lwa_mass_field-fvalue
                                       lwa_return_value-fieldval
                              CHANGING lwa_modified_values-value.
        ELSE.
          keytext_get_simple lv_tabname
                             lv_fieldname
                             lwa_return_value-fieldval
                             lwa_modified_values-value.
        ENDIF.
        APPEND lwa_modified_values TO <lt_modified_values>.
        gs_variables-data_changed = c_true.
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_value_request

  METHOD on_node_layout_set.

    DATA: lwa_extdfl_tree TYPE /agri/s_glfldfl_fcat,
          lwa_root_node   LIKE LINE OF mt_layout_rootnode.

    FIELD-SYMBOLS: <ls_layout>   TYPE any,
                   <lv_style>    TYPE any,
                   <lv_disabled> TYPE any.

    IF sy-dynnr = c_screen-reference_summary.

      READ TABLE gt_extdfl_tree INTO lwa_extdfl_tree INDEX e_index.

      CHECK sy-subrc = 0.

      ASSIGN eref_node_layout->* TO <ls_layout>.

      CHECK <ls_layout> IS ASSIGNED.

      IF e_grouplevel EQ 'TPLMA'.
        IF e_index NE me->mv_tabix.
          READ TABLE mt_layout_rootnode
          WITH KEY root_key = lwa_extdfl_tree-tplma
          TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
****Root node already processed check VBELN
            IF NOT lwa_extdfl_tree-tplnr_fl IS INITIAL.
              lwa_root_node-root_key = lwa_extdfl_tree-tplnr_fl.
              APPEND lwa_root_node TO mt_layout_rootnode.
            ENDIF.
            IF lwa_extdfl_tree-tplnr_fl NE gs_fldoc_infocus-tplnr_fl.
              mv_tabix = e_index.
              EXIT.
            ENDIF.
          ELSE.
****Root node not processed check VGBEL
            IF NOT lwa_extdfl_tree-tplma IS INITIAL.
              lwa_root_node-root_key = lwa_extdfl_tree-tplma.
              APPEND lwa_root_node TO mt_layout_rootnode.
            ENDIF.
            IF lwa_extdfl_tree-tplma NE gs_fldoc_infocus-tplnr_fl.
              mv_tabix = e_index.
****
              ASSIGN eref_node_layout->* TO <ls_layout>.
              CHECK <ls_layout> IS ASSIGNED.
              UNASSIGN: <lv_disabled>.
              ASSIGN COMPONENT 'DISABLED' OF STRUCTURE <ls_layout> TO <lv_disabled>.
              CHECK: <lv_disabled> IS ASSIGNED,
                     sy-subrc EQ 0.
              <lv_disabled> = c_true.
****
              EXIT.
            ENDIF.
          ENDIF.

        ELSE.

          IF NOT lwa_extdfl_tree-tplnr_fl IS INITIAL.
            lwa_root_node-root_key = lwa_extdfl_tree-tplnr_fl.
            APPEND lwa_root_node TO mt_layout_rootnode.
          ENDIF.

          IF lwa_extdfl_tree-tplnr_fl NE gs_fldoc_infocus-tplnr_fl.
            mv_tabix = e_index.
            EXIT.
          ENDIF.

        ENDIF.
      ELSE.
        CHECK lwa_extdfl_tree-tplnr_fl EQ gs_fldoc_infocus-tplnr_fl.
        IF lwa_extdfl_tree-tplma EQ lwa_extdfl_tree-tplnr_fl
          AND e_grouplevel EQ 'TPLNR_FL'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD on_hierarchy_node_add.

    DATA: lwa_current_tree     LIKE LINE OF gt_extdfl_tree,
          lwa_current_tree_tmp LIKE LINE OF gt_extdfl_tree,
          lwa_node_layout      TYPE lvc_s_lyin,
          lwa_root_node        LIKE LINE OF mt_docflow_rootnode,
          lt_node_layout       TYPE lvc_t_lyin,
          lv_read_node.

    IF sy-dynnr = c_screen-reference_summary.
      CHECK grouplevel EQ 'TPLMA'.

      READ TABLE gt_extdfl_tree INTO lwa_current_tree
                               INDEX index_outtab.
      CHECK sy-subrc = 0.

      IF lwa_current_tree-tplma = gs_fldoc_infocus-tplnr_fl.

        DELETE mt_docflow_rootnode WHERE root_key IS INITIAL.

        READ TABLE me->mt_docflow_rootnode
        WITH KEY root_key = lwa_current_tree-tplma
        TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.

          lwa_root_node-root_key = lwa_current_tree-tplma.
          APPEND lwa_root_node TO me->mt_docflow_rootnode.

          lwa_current_tree-erdat = gs_fldoc_infocus-x-flhdr-erdat.
          lwa_current_tree-erzet = gs_fldoc_infocus-x-flhdr-erzet.
          lwa_current_tree-ernam = gs_fldoc_infocus-x-flhdr-ernam.

        ENDIF.
      ENDIF.

      CALL METHOD ref_tree_ext_doc->set_hierarchy_data
        EXPORTING
          is_outtab_line = lwa_current_tree
          it_item_layout = lt_node_layout.

      me->mv_docflow_tabix = index_outtab.

    ENDIF.

  ENDMETHOD.

  METHOD on_node_hotspot_click_dfl_tree.

    DATA: lwa_extdfl_tree TYPE /agri/s_glfldfl_fcat,
          lv_tplnr        TYPE /agri/s_glflot-tplnr_fl.

    CHECK NOT index_outtab IS INITIAL.

    REFRESH gt_tplnr[].

    READ TABLE gt_extdfl_tree INTO lwa_extdfl_tree INDEX index_outtab.

    CHECK sy-subrc EQ 0.

    IF NOT grouplevel IS INITIAL.

      SELECT SINGLE tplnr_fl FROM /agri/glflot
        INTO lv_tplnr
       WHERE tplnr_fl EQ lwa_extdfl_tree-tplma.

    ELSE.

      SELECT SINGLE tplnr_fl FROM /agri/glflot
        INTO lv_tplnr
       WHERE tplnr_fl EQ lwa_extdfl_tree-tplnr_fl.

    ENDIF.

    CHECK NOT lv_tplnr IS INITIAL.
    APPEND lv_tplnr TO gt_tplnr.

    CALL FUNCTION '/AGRI/GLFL_PROCESS' DESTINATION 'NONE'
      EXPORTING
*       I_MODE                        = 'A'
*       I_DISPLAY_ONLY                = 'X'
        it_tplnr                      = gt_tplnr
*       I_SAVE                        = 'X'
*       IT_FILTER                     =
*       IS_WSLINK                     =
      EXCEPTIONS
        enter_terrain                 = 1
        invalid_parameter_combination = 2
        no_documents_to_process       = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_STATUS_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_status_handler IMPLEMENTATION.
  METHOD status_profile_trigger_set.
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_fldoc_infocus-x-flhdr-fltyp
                                          /agri/if_bse_con=>mc_pactg-import .

      status_script_context_set 'I_CALL_TYPE' i_call_type
                                          /agri/if_bse_con=>mc_pactg-import .

      status_script_context_set 'IT_ACTIVE_FLOWS' it_active_flows
                                           /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_ACTIVE_FLOW_STEPS' it_active_flow_steps
                                           /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_ALL_FLOWS' it_all_flows
                                           /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_ALL_FLOW_STEPS' it_all_flow_steps
                                           /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XFLHDR' gs_fldoc_infocus-x-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YFLHDR' gs_fldoc_infocus-y-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IS_XIFLOT' gs_fldoc_infocus-x-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IS_YIFLOT' gs_fldoc_infocus-y-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XADRC' gs_fldoc_infocus-x-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YADRC' gs_fldoc_infocus-y-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IT_XILOA' gs_fldoc_infocus-x-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.
*
*      status_script_context_set 'IT_YILOA' gs_fldoc_infocus-y-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIHPA' gs_fldoc_infocus-x-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIHPA' gs_fldoc_infocus-y-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATG' gs_fldoc_infocus-x-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATG' gs_fldoc_infocus-y-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATV' gs_fldoc_infocus-x-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATV' gs_fldoc_infocus-y-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLCMA' gs_fldoc_infocus-x-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLCMA' gs_fldoc_infocus-y-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLOWN' gs_fldoc_infocus-x-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLOWN' gs_fldoc_infocus-y-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'C_TRIGGER' c_trigger
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'C_STOP_SET' c_stop_set
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'CT_MESSAGES' ct_messages
                                 /agri/if_bse_con=>mc_pactg-export.

      status_script_execute i_bsenm.

      status_script_context_get 'C_TRIGGER' c_trigger.
      status_script_context_get 'C_STOP_SET' c_stop_set.
      status_script_context_get 'CT_MESSAGES' ct_messages.

    ENDIF.
****
    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XFLHDR'  gs_fldoc_infocus-x-flhdr  space.
      brf_context_set 'IS_YFLHDR'  gs_fldoc_infocus-y-flhdr  space.
*      brf_context_set 'IS_XIFLOT'  gs_fldoc_infocus-x-iflot  space.
*      brf_context_set 'IS_YIFLOT'  gs_fldoc_infocus-y-iflot  space.
      brf_context_set 'IT_XADRC'   gs_fldoc_infocus-x-adrc   space.
      brf_context_set 'IT_YADRC'   gs_fldoc_infocus-y-adrc   space.
*      brf_context_set 'IT_XILOA'   gs_fldoc_infocus-x-iloa   space.
*      brf_context_set 'IT_YILOA'   gs_fldoc_infocus-y-iloa   space.
      brf_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx space.
      brf_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx space.
      brf_context_set 'IT_XIHPA'   gs_fldoc_infocus-x-ihpa   space.
      brf_context_set 'IT_YIHPA'   gs_fldoc_infocus-y-ihpa   space.
      brf_context_set 'IT_XFLATG'  gs_fldoc_infocus-x-flatg  space.
      brf_context_set 'IT_YFLATG'  gs_fldoc_infocus-y-flatg  space.
      brf_context_set 'IT_XFLATV'  gs_fldoc_infocus-x-flatv  space.
      brf_context_set 'IT_YFLATV'  gs_fldoc_infocus-y-flatv  space.
      brf_context_set 'IT_XFLCMA'  gs_fldoc_infocus-x-flcma  space.
      brf_context_set 'IT_YFLCMA'  gs_fldoc_infocus-y-flcma  space.
      brf_context_set 'IT_XFLOWN'  gs_fldoc_infocus-x-flown  space.
      brf_context_set 'IT_YFLOWN'  gs_fldoc_infocus-y-flown  space.

      brf_context_set 'I_CALL_TYPE' i_call_type space.
      brf_context_set 'IT_ACTIVE_FLOWS' it_active_flows space.
      brf_context_set 'IT_ACTIVE_FLOW_STEPS' it_active_flow_steps space.
      brf_context_set 'IT_ALL_FLOWS' it_all_flows space.
      brf_context_set 'IT_ALL_FLOW_STEPS' it_all_flow_steps space.

      brf_context_set 'C_TRIGGER' c_trigger c_true.
      brf_context_set 'C_STOP_SET'    c_stop_set c_true.
      brf_context_set 'CT_MESSAGES' ct_messages c_true.



***Execute the BRF fucntion
      single_brf_function_process c_brf_object-glfl i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        brf_context_get 'C_TRIGGER' c_trigger.
        brf_context_get 'C_STOP_SET'    c_stop_set.
        brf_context_get 'CT_MESSAGES' ct_messages.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_fldoc_infocus-x-flhdr-fltyp.

***Profile BADI
    IF ref_badi_glfl_all IS BOUND.
      CALL BADI ref_badi_glfl_all->status_profile_trigger_set
        EXPORTING
          flt_val              = gs_fldoc_infocus-x-flhdr-fltyp
          i_call_type          = i_call_type
          it_active_flows      = it_active_flows
          it_active_flow_steps = it_active_flow_steps
          it_all_flows         = it_all_flows
          it_all_flow_steps    = it_all_flow_steps
          is_flhdr             = gs_fldoc_infocus-x-flhdr
          is_yflhdr            = gs_fldoc_infocus-y-flhdr
*         is_iflot             = gs_fldoc_infocus-x-iflot
*         is_yiflot            = gs_fldoc_infocus-y-iflot
          is_adrc              = gs_fldoc_infocus-x-adrc
          is_yadrc             = gs_fldoc_infocus-y-adrc
*         is_iloa              = gs_fldoc_infocus-x-iloa
*         is_yiloa             = gs_fldoc_infocus-y-iloa
          it_iflotx            = gs_fldoc_infocus-x-iflotx
          it_yiflotx           = gs_fldoc_infocus-y-iflotx
          it_ihpa              = gs_fldoc_infocus-x-ihpa
          it_yihpa             = gs_fldoc_infocus-y-ihpa
          it_flatg             = gs_fldoc_infocus-x-flatg
          it_yflatg            = gs_fldoc_infocus-y-flatg
          it_flatv             = gs_fldoc_infocus-x-flatv
          it_yflatv            = gs_fldoc_infocus-y-flatv
          it_flcma             = gs_fldoc_infocus-x-flcma
          it_yflcma            = gs_fldoc_infocus-y-flcma
          it_flown             = gs_fldoc_infocus-x-flown
          it_yflown            = gs_fldoc_infocus-y-flown
        CHANGING
          c_trigger            = c_trigger
          c_stop_set           = c_stop_set
          ct_messages          = ct_messages.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD status_flow_outcome_set.
***Release 60E_SP2 - Support Script in Status
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_fldoc_infocus-x-flhdr-fltyp
                                          /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_STEP_OUTCOME' is_step_outcome
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_STEP_OUTCOMES' it_step_outcomes
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XFLHDR' gs_fldoc_infocus-x-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YFLHDR' gs_fldoc_infocus-y-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IS_XIFLOT' gs_fldoc_infocus-x-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.
*
*      status_script_context_set 'IS_YIFLOT' gs_fldoc_infocus-y-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XADRC' gs_fldoc_infocus-x-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YADRC' gs_fldoc_infocus-y-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IT_XILOA' gs_fldoc_infocus-x-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.
*
*      status_script_context_set 'IT_YILOA' gs_fldoc_infocus-y-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIHPA' gs_fldoc_infocus-x-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIHPA' gs_fldoc_infocus-y-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATG' gs_fldoc_infocus-x-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATG' gs_fldoc_infocus-y-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATV' gs_fldoc_infocus-x-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATV' gs_fldoc_infocus-y-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLCMA' gs_fldoc_infocus-x-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLCMA' gs_fldoc_infocus-y-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLOWN' gs_fldoc_infocus-x-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLOWN' gs_fldoc_infocus-y-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'C_NEW_OUTCOME'  c_new_outcome
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'C_STOP_SET'  c_stop_set
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'CT_ACT_RECIPIENTS'  ct_act_recipients
                          /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'CT_MESSAGES'  ct_messages
                          /agri/if_bse_con=>mc_pactg-export.

      status_script_execute i_bsenm.

      status_script_context_get 'C_NEW_OUTCOME' c_new_outcome.
      status_script_context_get 'C_STOP_SET'    c_stop_set.
      status_script_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.
      status_script_context_get 'CT_MESSAGES' ct_messages.
    ENDIF.
***
    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XFLHDR'  gs_fldoc_infocus-x-flhdr  space.
      brf_context_set 'IS_YFLHDR'  gs_fldoc_infocus-y-flhdr  space.
*      brf_context_set 'IS_XIFLOT'  gs_fldoc_infocus-x-iflot  space.
*      brf_context_set 'IS_YIFLOT'  gs_fldoc_infocus-y-iflot  space.
      brf_context_set 'IT_XADRC'   gs_fldoc_infocus-x-adrc   space.
      brf_context_set 'IT_YADRC'   gs_fldoc_infocus-y-adrc   space.
*      brf_context_set 'IT_XILOA'   gs_fldoc_infocus-x-iloa   space.
*      brf_context_set 'IT_YILOA'   gs_fldoc_infocus-y-iloa   space.
      brf_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx space.
      brf_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx space.
      brf_context_set 'IT_XIHPA'   gs_fldoc_infocus-x-ihpa   space.
      brf_context_set 'IT_YIHPA'   gs_fldoc_infocus-y-ihpa   space.
      brf_context_set 'IT_XFLATG'  gs_fldoc_infocus-x-flatg  space.
      brf_context_set 'IT_YFLATG'  gs_fldoc_infocus-y-flatg  space.
      brf_context_set 'IT_XFLATV'  gs_fldoc_infocus-x-flatv  space.
      brf_context_set 'IT_YFLATV'  gs_fldoc_infocus-y-flatv  space.
      brf_context_set 'IT_XFLCMA'  gs_fldoc_infocus-x-flcma  space.
      brf_context_set 'IT_YFLCMA'  gs_fldoc_infocus-y-flcma  space.
      brf_context_set 'IT_XFLOWN'  gs_fldoc_infocus-x-flown  space.
      brf_context_set 'IT_YFLOWN'  gs_fldoc_infocus-y-flown  space.

      brf_context_set 'IS_STEP_OUTCOME' is_step_outcome space.
      brf_context_set 'IT_STEP_OUTCOME' it_step_outcomes space.

      brf_context_set 'C_NEW_OUTCOME' c_new_outcome c_true.
      brf_context_set 'C_STOP_SET'    c_stop_set c_true.
      brf_context_set 'CT_ACT_RECIPIENTS' ct_act_recipients c_true.
      brf_context_set 'CT_MESSAGES' ct_messages c_true.



***Execute the BRF fucntion
      single_brf_function_process c_brf_object-glfl i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        brf_context_get 'C_NEW_OUTCOME' c_new_outcome.
        brf_context_get 'C_STOP_SET'    c_stop_set.
        brf_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.
        brf_context_get 'CT_MESSAGES' ct_messages.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_fldoc_infocus-x-flhdr-fltyp.

***Profile BADI
    IF ref_badi_glfl_all IS BOUND.
      CALL BADI ref_badi_glfl_all->status_flow_outcome_set
        EXPORTING
          flt_val           = gs_fldoc_infocus-x-flhdr-fltyp
          is_step_outcome   = is_step_outcome
          it_step_outcomes  = it_step_outcomes
          is_flhdr          = gs_fldoc_infocus-x-flhdr
          is_yflhdr         = gs_fldoc_infocus-y-flhdr
*         is_iflot          = gs_fldoc_infocus-x-iflot
*         is_yiflot         = gs_fldoc_infocus-y-iflot
          is_adrc           = gs_fldoc_infocus-x-adrc
          is_yadrc          = gs_fldoc_infocus-y-adrc
*         is_iloa           = gs_fldoc_infocus-x-iloa
*         is_yiloa          = gs_fldoc_infocus-y-iloa
          it_iflotx         = gs_fldoc_infocus-x-iflotx
          it_yiflotx        = gs_fldoc_infocus-y-iflotx
          it_ihpa           = gs_fldoc_infocus-x-ihpa
          it_yihpa          = gs_fldoc_infocus-y-ihpa
          it_flatg          = gs_fldoc_infocus-x-flatg
          it_yflatg         = gs_fldoc_infocus-y-flatg
          it_flatv          = gs_fldoc_infocus-x-flatv
          it_yflatv         = gs_fldoc_infocus-y-flatv
          it_flcma          = gs_fldoc_infocus-x-flcma
          it_yflcma         = gs_fldoc_infocus-y-flcma
          it_flown          = gs_fldoc_infocus-x-flown
          it_yflown         = gs_fldoc_infocus-y-flown
        CHANGING
          c_new_outcome     = c_new_outcome
          c_stop_set        = c_stop_set
          ct_act_recipients = ct_act_recipients
          ct_messages       = ct_messages.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD status_flow_step_set.
***Release 60E_SP2 - Support Script in Status
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_fldoc_infocus-x-flhdr-fltyp
                                              /agri/if_bse_con=>mc_pactg-import .

      status_script_context_set 'IS_STEP_OUTCOME' is_step_outcome
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XFLHDR' gs_fldoc_infocus-x-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YFLHDR' gs_fldoc_infocus-y-flhdr
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IS_XIFLOT' gs_fldoc_infocus-x-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.
*
*      status_script_context_set 'IS_YIFLOT' gs_fldoc_infocus-y-iflot
*                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XADRC' gs_fldoc_infocus-x-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YADRC' gs_fldoc_infocus-y-adrc
                                      /agri/if_bse_con=>mc_pactg-import.

*      status_script_context_set 'IT_XILOA' gs_fldoc_infocus-x-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.
*
*      status_script_context_set 'IT_YILOA' gs_fldoc_infocus-y-iloa
*                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XIHPA' gs_fldoc_infocus-x-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YIHPA' gs_fldoc_infocus-y-ihpa
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATG' gs_fldoc_infocus-x-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATG' gs_fldoc_infocus-y-flatg
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLATV' gs_fldoc_infocus-x-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLATV' gs_fldoc_infocus-y-flatv
                                     /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLCMA' gs_fldoc_infocus-x-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLCMA' gs_fldoc_infocus-y-flcma
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XFLOWN' gs_fldoc_infocus-x-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YFLOWN' gs_fldoc_infocus-y-flown
                                    /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'C_NEW_OUTCOME'  c_new_outcome
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'CT_ACT_RECIPIENTS'  ct_act_recipients
                          /agri/if_bse_con=>mc_pactg-export.

      status_script_execute i_bsenm.

      status_script_context_get 'C_NEW_OUTCOME' c_new_outcome.
      status_script_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.
    ENDIF.
***
    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XFLHDR'  gs_fldoc_infocus-x-flhdr  space.
      brf_context_set 'IS_YFLHDR'  gs_fldoc_infocus-y-flhdr  space.
*      brf_context_set 'IS_XIFLOT'  gs_fldoc_infocus-x-iflot  space.
*      brf_context_set 'IS_YIFLOT'  gs_fldoc_infocus-y-iflot  space.
      brf_context_set 'IT_XADRC'   gs_fldoc_infocus-x-adrc   space.
      brf_context_set 'IT_YADRC'   gs_fldoc_infocus-y-adrc   space.
*      brf_context_set 'IT_XILOA'   gs_fldoc_infocus-x-iloa   space.
*      brf_context_set 'IT_YILOA'   gs_fldoc_infocus-y-iloa   space.
      brf_context_set 'IT_XIFLOTX' gs_fldoc_infocus-x-iflotx space.
      brf_context_set 'IT_YIFLOTX' gs_fldoc_infocus-y-iflotx space.
      brf_context_set 'IT_XIHPA'   gs_fldoc_infocus-x-ihpa   space.
      brf_context_set 'IT_YIHPA'   gs_fldoc_infocus-y-ihpa   space.
      brf_context_set 'IT_XFLATG'  gs_fldoc_infocus-x-flatg  space.
      brf_context_set 'IT_YFLATG'  gs_fldoc_infocus-y-flatg  space.
      brf_context_set 'IT_XFLATV'  gs_fldoc_infocus-x-flatv  space.
      brf_context_set 'IT_YFLATV'  gs_fldoc_infocus-y-flatv  space.
      brf_context_set 'IT_XFLCMA'  gs_fldoc_infocus-x-flcma  space.
      brf_context_set 'IT_YFLCMA'  gs_fldoc_infocus-y-flcma  space.
      brf_context_set 'IT_XFLOWN'  gs_fldoc_infocus-x-flown  space.


      brf_context_set 'IS_STEP_OUTCOME' is_step_outcome space.


      brf_context_set 'C_NEW_OUTCOME' c_new_outcome c_true.
      brf_context_set 'CT_ACT_RECIPIENTS' ct_act_recipients c_true.

***Execute the BRF fucntion
      single_brf_function_process c_brf_object-glfl i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.


        brf_context_get 'C_NEW_OUTCOME' c_new_outcome.
        brf_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_fldoc_infocus-x-flhdr-fltyp.

***Profile BADI
    IF ref_badi_glfl_all IS BOUND.
      CALL BADI ref_badi_glfl_all->status_flow_step_set
        EXPORTING
          flt_val           = gs_fldoc_infocus-x-flhdr-fltyp
          is_step_outcome   = is_step_outcome
          is_flhdr          = gs_fldoc_infocus-x-flhdr
          is_yflhdr         = gs_fldoc_infocus-y-flhdr
*         is_iflot          = gs_fldoc_infocus-x-iflot
*         is_yiflot         = gs_fldoc_infocus-y-iflot
          is_adrc           = gs_fldoc_infocus-x-adrc
          is_yadrc          = gs_fldoc_infocus-y-adrc
*         is_iloa           = gs_fldoc_infocus-x-iloa
*         is_yiloa          = gs_fldoc_infocus-y-iloa
          it_iflotx         = gs_fldoc_infocus-x-iflotx
          it_yiflotx        = gs_fldoc_infocus-y-iflotx
          it_ihpa           = gs_fldoc_infocus-x-ihpa
          it_yihpa          = gs_fldoc_infocus-y-ihpa
          it_flatg          = gs_fldoc_infocus-x-flatg
          it_yflatg         = gs_fldoc_infocus-y-flatg
          it_flatv          = gs_fldoc_infocus-x-flatv
          it_yflatv         = gs_fldoc_infocus-y-flatv
          it_flcma          = gs_fldoc_infocus-x-flcma
          it_yflcma         = gs_fldoc_infocus-y-flcma
          it_flown          = gs_fldoc_infocus-x-flown
          it_yflown         = gs_fldoc_infocus-y-flown
        CHANGING
          c_new_outcome     = c_new_outcome
          ct_act_recipients = ct_act_recipients.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD appl_data_get.
    DATA: lwa_data_ref  TYPE /agri/s_gdataref.

    GET REFERENCE OF gs_fldoc_infocus-x-flhdr INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/S_GLFLHDR'.
    APPEND lwa_data_ref TO ct_ref_data.

*    CLEAR lwa_data_ref.
*    GET REFERENCE OF gs_fldoc_infocus-x-iflot INTO lwa_data_ref-value.
*    lwa_data_ref-name = '/AGRI/S_GLIFLOT'.
*    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-adrc INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/S_GLADRC'.
    APPEND lwa_data_ref TO ct_ref_data.

*    CLEAR lwa_data_ref.
*    GET REFERENCE OF gs_fldoc_infocus-x-iloa INTO lwa_data_ref-value.
*    lwa_data_ref-name = '/AGRI/S_GLILOA'.
*    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-iflotx INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLIFLOTX'.
    APPEND lwa_data_ref TO ct_ref_data.

*    CLEAR lwa_data_ref.
*    GET REFERENCE OF gs_fldoc_infocus-x-iflotx INTO lwa_data_ref-value.
*    lwa_data_ref-name = '/AGRI/T_GLIFLOTX'.
*    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-ihpa INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLIHPA'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-flatg INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLFLATG'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-flatv INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLFLATV'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-flcma INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLFLCMA'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_fldoc_infocus-x-flown INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_GLFLOWN'.
    APPEND lwa_data_ref TO ct_ref_data.

  ENDMETHOD.                    "appl_data_get

ENDCLASS.               "LCL_STATUS_HANDLER
