*&---------------------------------------------------------------------*
*& Include          LZFG_ACMCLS
*&---------------------------------------------------------------------*
"lcl_event_handler IMPLEMENTATION
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
****Worklist
    METHODS: on_toolbar_wl FOR EVENT toolbar
                  OF /agri/cl_worklist_container
      IMPORTING e_object e_interactive,

      on_user_command_wl FOR EVENT user_command
                    OF /agri/cl_worklist_container
        IMPORTING e_ucomm,


      on_user_command FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_view_changed FOR EVENT view_changed
                    OF /agri/cl_worklist_container
        IMPORTING e_view,

      on_hotspot_click_wl FOR EVENT hotspot_click
                    OF /agri/cl_worklist_container
        IMPORTING e_row_id e_column_id
                    es_row_no,
*****Grid
      on_data_changed_grid FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed
                    e_onf4
                    e_onf4_before
                    e_onf4_after
*                                             e_fieldname
*                                             es_row_no
                    sender,

      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_toolbar_grid      FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,

      on_handle_f4 FOR EVENT onf4 OF /agri/cl_gui_alv_grid
        IMPORTING sender
                    e_fieldname
                    e_fieldvalue
                    es_row_no
                    er_event_data
                    et_bad_cells
                    e_display
        ,
      on_hotspot_click FOR EVENT hotspot_click OF /agri/cl_gui_alv_grid
        IMPORTING
            e_row_id
            e_column_id
            es_row_no sender,

      on_value_request     FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no
                    er_event_data
                    et_bad_cells
                    e_display.

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
    DATA: lv_view    TYPE i,
          lwa_button TYPE stb_button.

    lv_view = ref_worklist->view_in_focus_get( ).

    IF lv_view EQ 2.

      toolbar_button_insert e_object->mt_toolbar lwa_button 3 space
                            space space space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search_more icon_search_next
                            TEXT-010 space.

      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-wl_search icon_search TEXT-009
                            space.

    ENDIF.

  ENDMETHOD.                    "on_toolbar_wl

  METHOD on_user_command.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.
  ENDMETHOD.

  METHOD on_user_command_wl.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.
  ENDMETHOD.                    "on_user_command_wl
  METHOD on_view_changed.
    PERFORM worklist_refresh USING e_view.
  ENDMETHOD.                    "on_view_changed
  METHOD on_hotspot_click_wl.
    DATA: lv_view             TYPE i,
          lwa_selected_doc    LIKE LINE OF gt_selected_docs,
          lwa_worklist_header LIKE LINE OF gt_worklist_header,
          lv_accom            TYPE /agri/fmaccom.

    CLEAR gt_selected_docs.
    REFRESH: gt_selected_docs.

    IF lv_view IS INITIAL.
      lv_view = ref_worklist->view_in_focus_get( ).
    ENDIF.

    IF lv_view EQ 1.
      READ TABLE gt_worklist_header INTO lwa_worklist_header
                                 INDEX e_row_id-index.
      lv_accom = lwa_worklist_header-accom.
    ELSE.
      READ TABLE gt_search_header INTO lwa_worklist_header
                               INDEX e_row_id-index.
      lv_accom = lwa_worklist_header-accom.
    ENDIF.

    CHECK lv_accom NE gs_acdoc_infocus-accom.
    CHECK NOT lwa_worklist_header IS INITIAL.

    lwa_selected_doc-accom = lv_accom.
    APPEND lwa_selected_doc TO gt_selected_docs.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-worklist_hotspot.

    PERFORM fcode_/agri/min.
  ENDMETHOD.                    "on_hotspot_click_wl
  METHOD on_data_changed_grid.
    DATA: lwa_row_id  TYPE lvc_s_row,
          lwa_col_id  TYPE lvc_s_col,
          lwa_mod_row TYPE lvc_s_modi.

    gs_variables-manual_changes = c_true.
*    REFRESH: gt_desc_mod_rows.

    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
*** 60E fg desc changes
      CASE sender.
        WHEN ref_grid_items.
          REFRESH gt_items_mod_rows.
          gs_variables-manual_changes = c_true.
          APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_items_mod_rows.
*          SORT gt_items_mod_rows BY row_id.
*          DELETE ADJACENT DUPLICATES FROM gt_items_mod_rows COMPARING row_id fieldname.
*          DELETE gt_items_mod_rows WHERE tabix NE 1.
        WHEN ref_multi_lang_desc_grid.
          gs_variables-manual_changes = c_true.
          APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_desc_mod_rows .
      ENDCASE.
    ELSE.
      gs_variables-manual_changes = c_false.
    ENDIF.

    IF gt_desc_mod_rows IS NOT INITIAL OR
       gt_items_mod_rows IS NOT INITIAL.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = c_fcode-enter.

    ENDIF.
  ENDMETHOD.                    "on_data_changed_grid
  METHOD on_user_command_grid.

    REFRESH gt_selected_rows.
    IF e_ucomm EQ c_fcode-desc_delete.
      CALL METHOD ref_multi_lang_desc_grid->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-desc_delete.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.

    ELSEIF e_ucomm EQ c_fcode-item_delete.
      CALL METHOD ref_grid_items->get_selected_rows
        IMPORTING
          et_index_rows = gt_selected_rows.
      IF NOT gt_selected_rows IS INITIAL.
****Desc Layout Row Delete
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-item_delete.

      ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
        MESSAGE ID '/AGRI/GLOBAL' TYPE 'I' NUMBER 321 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_user_command_grid
  METHOD on_toolbar_grid.
    DATA : lwa_button TYPE stb_button.

    CASE sender. "sy-dynnr.
      WHEN ref_multi_lang_desc_grid. "c_screen-multi_lang_desc.
        CLEAR e_object->mt_toolbar.
        IF gs_variables-document_mode NE c_mode_display.
****Delete Item Row
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-desc_delete icon_delete_row
                                TEXT-017 space.
****Insert Item Row
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-desc_insert icon_insert_row
                                TEXT-003 space.
          toolbar_button_insert e_object->mt_toolbar lwa_button space
                                c_fcode-set_values icon_change_number
                                TEXT-053 space.
        ENDIF.
      WHEN ref_grid_items.
        IF gs_variables-document_mode NE c_mode_display.
          IF gs_acdoc_infocus-x-achdr-status = c_process_status-cnf
             AND gs_acdoc_infocus-updkz NE c_updkz_new.
****Reversal of Confirmation
            toolbar_button_insert e_object->mt_toolbar lwa_button space
                                  c_fcode-conf_reversed icon_status_reverse
                                  TEXT-047 space.
          ENDIF.

          IF ( gs_acdoc_infocus-x-achdr-status = c_process_status-ctd
            OR gs_acdoc_infocus-x-achdr-status = c_process_status-cnf )
            AND gs_acdoc_infocus-updkz NE c_updkz_new.
            toolbar_button_insert e_object->mt_toolbar lwa_button 3
                                            space space space space.
****Delete Item Row
            toolbar_button_insert e_object->mt_toolbar lwa_button space
                                  c_fcode-item_delete icon_delete_row
                                  TEXT-017 space.
****Insert Item Row
            toolbar_button_insert e_object->mt_toolbar lwa_button space
                                  c_fcode-item_insert icon_insert_row
                                  TEXT-003 space.
****Set Value
            toolbar_button_insert e_object->mt_toolbar lwa_button space
                                  c_fcode-set_values icon_change_number
                                  TEXT-053 space.
          ENDIF.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_toolbar_grid
  METHOD on_handle_f4.

    TYPES: BEGIN OF ty_tplnr_sh,
             zztplnr TYPE zabs_del_tplnr,
             pltxt   TYPE /agri/glpltxt,
           END OF ty_tplnr_sh.

    TYPES: BEGIN OF ty_actrns,
             zzactrn TYPE zfmactrn,
             acdtr   TYPE zfmacdtr,
           END OF ty_actrns.

    TYPES: BEGIN OF ty_shift.
        INCLUDE TYPE zfmacwork_shift.
    TYPES: stime TYPE tzonref-tstamps,
           etime TYPE tzonref-tstamps,
           END OF ty_shift.

    DATA: lwa_return_tab  TYPE ddshretval,
          lwa_resource    TYPE /agri/fmacres,
          lwa_mod_cell    TYPE lvc_s_modi,
          lwa_style       TYPE lvc_s_styl,
          lt_fcat         TYPE lvc_t_fcat,
          lwa_fcat        TYPE lvc_s_fcat,
          lwa_items       TYPE /agri/s_fmacitm_layout,
          er_data_changed TYPE REF TO cl_alv_changed_data_protocol,
          lv_stime        TYPE tzonref-tstamps,
          lv_etime        TYPE tzonref-tstamps,
          lt_fmacwork     TYPE STANDARD TABLE OF ty_shift,
          lv_date         TYPE datum.

    FIELD-SYMBOLS: <lwa_items> TYPE /agri/s_fmacitm_layout,
                   <lt_itab>   TYPE lvc_t_modi.

    DATA: lt_return   TYPE STANDARD TABLE OF ddshretval.
    DATA: ls_acm_ord TYPE zabs_acm_ord,
          lt_acm_ord TYPE STANDARD TABLE OF zabs_acm_ord.

    FIELD-SYMBOLS: <fs_t_details> TYPE STANDARD TABLE,
                   <fs_s_details> TYPE any.

    DATA: ls_tplnr_sh TYPE ty_tplnr_sh,
          lt_tplnr_sh TYPE STANDARD TABLE OF ty_tplnr_sh,
          lt_actrns   TYPE STANDARD TABLE OF ty_actrns,
          ls_actrns   TYPE ty_actrns.

*--- It does not generate the output of the standard aid
    er_event_data->m_event_handled = c_true.

    CASE e_fieldname.

      WHEN 'AUFNR'.
        CLEAR gt_returntab[].

        PERFORM f4_orders_display USING gt_returntab
                                        es_row_no-row_id.

        READ TABLE gt_returntab INTO lwa_return_tab WITH KEY retfield = 'AUFNR'.
        IF sy-subrc = 0.

          ASSIGN er_event_data->m_data->* TO <lt_itab>.
          lwa_mod_cell-row_id    = es_row_no-row_id.
          lwa_mod_cell-fieldname = 'AUFNR'.
          lwa_mod_cell-value = lwa_return_tab-fieldval.
          APPEND lwa_mod_cell TO <lt_itab>.
        ENDIF.
      WHEN 'IDRESOURCE'.
        CLEAR gt_returntab[].
        PERFORM f4_resource_display USING e_fieldvalue
                                            gt_returntab
                                            es_row_no-row_id
                                            c_rstype-labor
                                            e_fieldname.
        READ TABLE gt_returntab INTO lwa_return_tab WITH  KEY retfield = 'IDRESOURCE'.
        IF sy-subrc = 0.

          ASSIGN er_event_data->m_data->* TO <lt_itab>.
          lwa_mod_cell-row_id    = es_row_no-row_id.
          lwa_mod_cell-fieldname = 'IDRESOURCE'.
          lwa_mod_cell-value = lwa_return_tab-fieldval.
          APPEND lwa_mod_cell TO <lt_itab>.

          READ TABLE gt_items_layout ASSIGNING <lwa_items> INDEX es_row_no-row_id.
          IF sy-subrc EQ 0.
            <lwa_items>-idresource  = lwa_return_tab-fieldval.
          ENDIF.

          IF <lwa_items>-idresource IS NOT INITIAL.
            REFRESH: <lwa_items>-styles.
            IF ref_grid_items IS NOT INITIAL.
              IF <lwa_items>-status EQ c_process_status-ctd.
                CALL METHOD ref_grid_items->get_frontend_fieldcatalog
                  IMPORTING
                    et_fieldcatalog = lt_fcat.
                LOOP AT lt_fcat INTO lwa_fcat WHERE fieldname = 'IDACTVL'.
                  lwa_style-fieldname = lwa_fcat-fieldname.
                  lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT lwa_style INTO TABLE <lwa_items>-styles.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.

          CALL METHOD cl_gui_cfw=>set_new_ok_code
            EXPORTING
              new_code = c_fcode-enter.
        ENDIF.

      WHEN 'EQUNR'.
        CLEAR gt_returntab[].
        PERFORM f4_resource_display USING e_fieldvalue
                                            gt_returntab
                                            es_row_no-row_id
                                            c_rstype-equnr
                                            e_fieldname.
        READ TABLE gt_returntab INTO lwa_return_tab WITH  KEY retfield = 'IDRESOURCE'. " 'IDRESOURCE'
        IF sy-subrc = 0.

          ASSIGN er_event_data->m_data->* TO <lt_itab>.
          lwa_mod_cell-row_id    = es_row_no-row_id.
          lwa_mod_cell-fieldname = 'EQUNR'.
          lwa_mod_cell-value = lwa_return_tab-fieldval.
          APPEND lwa_mod_cell TO <lt_itab>.

          READ TABLE gt_items_layout ASSIGNING <lwa_items> INDEX es_row_no-row_id.
          IF sy-subrc EQ 0.
            <lwa_items>-equnr  = lwa_return_tab-fieldval.
            IF <lwa_items>-arbpl IS INITIAL.
              SELECT SINGLE arbpl                       "#EC CI_NOORDER
              FROM /agri/fmacres
              INTO <lwa_items>-arbpl
              WHERE idresource = lwa_return_tab-fieldval.
            ENDIF.
          ENDIF.

          CALL METHOD cl_gui_cfw=>set_new_ok_code
            EXPORTING
              new_code = c_fcode-enter.
        ENDIF.
      WHEN 'IDACTVL'.
        CLEAR gt_returntab[].
        PERFORM f4_activity_display USING e_fieldvalue
                                          gt_returntab
                                          es_row_no-row_id
                                          c_rstype-labor
                                          e_fieldname.
        READ TABLE gt_returntab INTO lwa_return_tab WITH  KEY retfield = 'IDACTV'.
        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <lt_itab>.
          lwa_mod_cell-row_id    = es_row_no-row_id.
          lwa_mod_cell-fieldname = 'IDACTVL'.
          lwa_mod_cell-value = lwa_return_tab-fieldval.
          APPEND lwa_mod_cell TO <lt_itab>.

          READ TABLE gt_items_layout ASSIGNING <lwa_items> INDEX es_row_no-row_id.
          READ TABLE <lwa_items>-styles	INTO lwa_style WITH KEY fieldname = 'IDACTVL'.
          IF sy-subrc EQ 0 AND lwa_style-style EQ '00100000'.
            CLEAR <lt_itab>.
          ELSE.
            <lwa_items>-idactvl  = lwa_return_tab-fieldval.
          ENDIF.

          CALL METHOD cl_gui_cfw=>set_new_ok_code
            EXPORTING
              new_code = c_fcode-enter.
        ENDIF.
      WHEN 'IDACTVE'.
        CLEAR gt_returntab[].
        PERFORM f4_activity_display USING e_fieldvalue
                                          gt_returntab
                                          es_row_no-row_id
                                          c_rstype-equnr
                                          e_fieldname.
        READ TABLE gt_returntab INTO lwa_return_tab WITH  KEY retfield = 'IDACTV'.
        IF sy-subrc = 0.

          ASSIGN er_event_data->m_data->* TO <lt_itab>.
          lwa_mod_cell-row_id    = es_row_no-row_id.
          lwa_mod_cell-fieldname = 'IDACTVE'.
          lwa_mod_cell-value = lwa_return_tab-fieldval.
          APPEND lwa_mod_cell TO <lt_itab>.

          READ TABLE gt_items_layout ASSIGNING <lwa_items> INDEX es_row_no-row_id.
          READ TABLE <lwa_items>-styles	INTO lwa_style WITH KEY fieldname = 'IDACTVE'.
          IF sy-subrc EQ 0 AND lwa_style-style EQ '00100000'.
            CLEAR <lt_itab>.
          ELSE.
            <lwa_items>-idactve  = lwa_return_tab-fieldval.
          ENDIF.

          CALL METHOD cl_gui_cfw=>set_new_ok_code
            EXPORTING
              new_code = c_fcode-enter.
        ENDIF.

      WHEN 'ZZTPLNR'.

        REFRESH gt_returntab.
        ASSIGN ('(SAPLZFG_ACM)GT_DETAILS') TO <fs_t_details>.
        IF sy-subrc EQ 0 AND <fs_t_details> IS ASSIGNED.
          LOOP AT <fs_t_details> ASSIGNING <fs_s_details>.
            CLEAR ls_acm_ord.
            MOVE-CORRESPONDING <fs_s_details> TO ls_acm_ord.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = ls_acm_ord-tplnr_fl
              IMPORTING
                output     = ls_tplnr_sh-zztplnr
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

*        ls_tplnr_sh-zztplnr = ls_acm_ord-tplnr_fl.
            APPEND ls_tplnr_sh TO lt_tplnr_sh.
          ENDLOOP.

          SORT lt_tplnr_sh BY zztplnr.
          DELETE ADJACENT DUPLICATES FROM lt_tplnr_sh COMPARING zztplnr.

          IF lt_tplnr_sh IS NOT INITIAL.
            SELECT tplnr_fl pltxt
              FROM /agri/glflot
              INTO TABLE lt_tplnr_sh
               FOR ALL ENTRIES IN lt_tplnr_sh
             WHERE tplnr_fl EQ lt_tplnr_sh-zztplnr.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
                EXPORTING
                  retfield        = 'ZZTPLNR'
*                 retfield        = lv_retfield
                  dynpprog        = sy-repid
                  dynpnr          = sy-dynnr
                  dynprofield     = 'ZZTPLNR'
*                 dynprofield     = lv_dynprofield
                  stepl           = 1
                  value           = 'X'
                  value_org       = 'S'
                TABLES
                  value_tab       = lt_tplnr_sh
                  return_tab      = lt_return
                EXCEPTIONS ##FM_SUBRC_OK
                  parameter_error = 1
                  no_values_found = 2
                  OTHERS          = 3.
              IF sy-subrc EQ 0.
                gt_returntab = lt_return.

                READ TABLE gt_returntab INTO lwa_return_tab WITH KEY retfield = 'ZZTPLNR'.
                IF sy-subrc = 0.

                  ASSIGN er_event_data->m_data->* TO <lt_itab>.
                  lwa_mod_cell-row_id    = es_row_no-row_id.
                  lwa_mod_cell-fieldname = 'ZZTPLNR'.
                  lwa_mod_cell-value = lwa_return_tab-fieldval.
                  APPEND lwa_mod_cell TO <lt_itab>.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-enter.

      WHEN 'ZZACTRN'.

        READ TABLE gt_items_layout
              INTO DATA(lwa_items_layout) INDEX es_row_no-row_id.
        IF sy-subrc EQ 0.
          SELECT *
            FROM zfmacwork_shift
            INTO TABLE lt_fmacwork
            WHERE werks  EQ /agri/s_fmachdr-werks
              AND arbpl  EQ lwa_items_layout-arbpl
              AND period EQ lwa_items_layout-strtdat(6).
          IF sy-subrc EQ 0.
            CLEAR: lv_stime, lv_etime.
            CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
              EXPORTING
                iv_sdate = lwa_items_layout-strtdat
                iv_edate = lwa_items_layout-findat
                iv_stime = lwa_items_layout-strttim
                iv_etime = lwa_items_layout-fintim
              IMPORTING
                ev_stime = lv_stime
                ev_etime = lv_etime.

            LOOP AT lt_fmacwork ASSIGNING FIELD-SYMBOL(<fs_fmacwork>).
              CLEAR lv_date.
              lv_date = lwa_items_layout-strtdat.
              IF <fs_fmacwork>-achit GT <fs_fmacwork>-achft.
                lv_date = lv_date + 1.
              ENDIF.

              CALL FUNCTION 'ZABS_FM_SHIFT_TIMES'
                EXPORTING
                  iv_sdate = lwa_items_layout-strtdat
                  iv_edate = lv_date
                  iv_stime = <fs_fmacwork>-achit
                  iv_etime = <fs_fmacwork>-achft
                IMPORTING
                  ev_stime = <fs_fmacwork>-stime
                  ev_etime = <fs_fmacwork>-etime.
            ENDLOOP.


            LOOP AT lt_fmacwork INTO DATA(lwa_fmacwork)
                                         WHERE stime LE lv_stime
                                           AND etime GE lv_etime.
              CLEAR ls_actrns.
              ls_actrns-zzactrn = lwa_fmacwork-actrn.
              ls_actrns-acdtr   = lwa_fmacwork-acdtr.
              COLLECT ls_actrns INTO lt_actrns.
            ENDLOOP.

            IF lt_actrns IS NOT INITIAL.
              REFRESH gt_returntab.
              CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
                EXPORTING
                  retfield        = 'ZZACTRN'
*                 retfield        = lv_retfield
                  dynpprog        = sy-repid
                  dynpnr          = sy-dynnr
                  dynprofield     = 'ZZACTRN'
*                 dynprofield     = lv_dynprofield
                  stepl           = 1
                  value           = 'X'
                  value_org       = 'S'
                TABLES
                  value_tab       = lt_actrns
                  return_tab      = lt_return
                EXCEPTIONS ##FM_SUBRC_OK
                  parameter_error = 1
                  no_values_found = 2
                  OTHERS          = 3.
              IF sy-subrc EQ 0.
                gt_returntab = lt_return.

                READ TABLE gt_returntab INTO lwa_return_tab WITH KEY retfield = 'ZZACTRN'.
                IF sy-subrc = 0.
                  ASSIGN er_event_data->m_data->* TO <lt_itab>.
                  lwa_mod_cell-row_id    = es_row_no-row_id.
                  lwa_mod_cell-fieldname = 'ZZACTRN'.
                  lwa_mod_cell-value = lwa_return_tab-fieldval.
                  APPEND lwa_mod_cell TO <lt_itab>.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = c_fcode-enter.
    ENDCASE.
  ENDMETHOD.                    "on_handle_f4
  METHOD on_hotspot_click.

    DATA: lv_fcode       TYPE sy-ucomm,
          lwa_fpitm_fcat TYPE /agri/s_fmfpitm_fcat,
          lwa_items      TYPE /agri/s_fmacitm_layout,
          lv_belnr       TYPE belnr_d,
          lv_tcode       TYPE tcode,
          lwa_fpcnf_fcat TYPE /agri/s_fmfp_cnf_fcat.

    CASE sender.
      WHEN ref_grid_items.
        IF e_column_id EQ 'AUFNR'.
          READ TABLE gt_items_layout INTO lwa_items INDEX e_row_id-index.
          IF sy-subrc EQ 0.
            PERFORM display_production_order USING lwa_items-aufnr.
          ENDIF.
        ELSEIF e_column_id EQ 'EBELN'.
          READ TABLE gt_items_layout INTO lwa_items INDEX e_row_id-index.
          IF sy-subrc EQ 0.
            CHECK lwa_items-ebeln IS NOT INITIAL.
            SET PARAMETER ID 'BES' FIELD lwa_items-ebeln.
            CALL METHOD /agri/cl_global_services=>transaction_call_process
              EXPORTING
                i_tcode                = 'ME23N  ' "'FB05'
*               i_skip_first_screen    =
*               i_leave_to_transaction_code    =
*               i_leave_to_current_transaction =
*               i_use_bdc_data         = 'X'
**       i_bdc_mode_val         =
**       i_bdc_update_val       =
*               is_bdc_options         = lwa_bdc_options
*               it_bdcdata             = lt_bdcdata[]
*      IMPORTING
*               et_bdc_messages        = lt_bdc_messages
              EXCEPTIONS
                invalid_parameters     = 1
                call_transaction_error = 2.

            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.

        ELSEIF e_column_id EQ 'MBLNR'.
          CLEAR: lwa_items.
          READ TABLE gt_items_layout INTO lwa_items INDEX e_row_id-index.
          IF sy-subrc = 0.
            CHECK lwa_items-mblnr IS NOT INITIAL.
            SUBMIT mb_call_migo_dialog WITH i_action = 'A04' "#EC CI_SUBMIT
            WITH i_refdoc = 'R02'
            WITH i_notree = 'X'
            WITH i_skip_f = 'X'
            WITH i_okcode = 'OK_GO'
            WITH i_new_ro = 'X'
            WITH i_mblnr = lwa_items-mblnr
            WITH i_mjahr = /agri/s_fmachdr-gjahr
            AND RETURN.
          ENDIF.
        ENDIF.
    ENDCASE.

    READ TABLE gt_items_layout INTO lwa_items INDEX e_row_id-index.
    IF lwa_items-rueck IS NOT INITIAL.
      SET PARAMETER ID 'RCK' FIELD lwa_items-rueck.
      CALL METHOD /agri/cl_global_services=>transaction_call_process
        EXPORTING
          i_tcode                = 'CO14'
*         i_skip_first_screen    =
*         i_leave_to_transaction_code    =
*         i_leave_to_current_transaction =
*         i_use_bdc_data         = 'X'
**       i_bdc_mode_val         =
**       i_bdc_update_val       =
*         is_bdc_options         = lwa_bdc_options
*         it_bdcdata             = lt_bdcdata[]
*      IMPORTING
*         et_bdc_messages        = lt_bdc_messages
        EXCEPTIONS
          invalid_parameters     = 1
          call_transaction_error = 2.


      CALL TRANSACTION 'CO14' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.                    "on_hotspot_click

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
        dynpprog          = c_program
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
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_status_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_status_handler DEFINITION.
****Release 60E Status Changes for BRF
  PUBLIC SECTION.
    INTERFACES /agri/if_gstat_exit.

    ALIASES: appl_data_get  FOR /agri/if_gstat_exit~appl_data_get,
             status_profile_trigger_set FOR /agri/if_gstat_exit~status_profile_trigger_set,
             status_flow_outcome_set    FOR /agri/if_gstat_exit~status_flow_outcome_set,
             status_flow_step_set       FOR /agri/if_gstat_exit~status_flow_step_set.
***
ENDCLASS.               "LCL_STATUS_HANDLER
*----------------------------------------------------------------------*
*       CLASS lcl_status_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_status_handler IMPLEMENTATION.

  METHOD status_profile_trigger_set.
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_acdoc_infocus-x-achdr-accom
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

      status_script_context_set 'IS_XACHDR' gs_acdoc_infocus-x-achdr
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YACHDR' gs_acdoc_infocus-y-achdr
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes
                                      /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes
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

    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XACHDR' gs_acdoc_infocus-x-achdr space.
      brf_context_set 'IS_YACHDR' gs_acdoc_infocus-y-achdr space.
      brf_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm space.
      brf_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm space.
      brf_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes space.
      brf_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes space.

      brf_context_set 'I_CALL_TYPE' i_call_type space.
      brf_context_set 'IT_ACTIVE_FLOWS' it_active_flows space.
      brf_context_set 'IT_ACTIVE_FLOW_STEPS' it_active_flow_steps space.
      brf_context_set 'IT_ALL_FLOWS' it_all_flows space.
      brf_context_set 'IT_ALL_FLOW_STEPS' it_all_flow_steps space.

      brf_context_set 'C_TRIGGER' c_trigger c_true.
      brf_context_set 'C_STOP_SET'    c_stop_set c_true.
      brf_context_set 'CT_MESSAGES' ct_messages c_true.

***Execute the BRF fucntion
      single_brf_function_process c_brf_object-fmac i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        brf_context_get 'C_TRIGGER' c_trigger.
        brf_context_get 'C_STOP_SET'  c_stop_set.
        brf_context_get 'CT_MESSAGES' ct_messages.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_acdoc_infocus-x-achdr-actyp.

***Profile BADI
    IF ref_badi_fmac_all IS BOUND.
      CALL BADI ref_badi_fmac_all->status_profile_trigger_set
        EXPORTING
          flt_val              = gs_acdoc_infocus-x-achdr-accom
          i_call_type          = i_call_type
          it_active_flows      = it_active_flows
          it_active_flow_steps = it_active_flow_steps
          it_all_flows         = it_all_flows
          it_all_flow_steps    = it_active_flow_steps
          is_xachdr            = gs_acdoc_infocus-x-achdr
          is_yachdr            = gs_acdoc_infocus-y-achdr
          it_xacitm            = gs_acdoc_infocus-x-acitm
          it_yacitm            = gs_acdoc_infocus-y-acitm
          it_xacdet            = gs_acdoc_infocus-x-acdet
          it_yacdet            = gs_acdoc_infocus-y-acdet
        CHANGING
          c_trigger            = c_trigger
          c_stop_set           = c_stop_set
          ct_messages          = ct_messages.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD status_flow_outcome_set.
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_acdoc_infocus-x-achdr-accom
                                          /agri/if_bse_con=>mc_pactg-import .
      status_script_context_set 'IS_STEP_OUTCOME' is_step_outcome
                                /agri/if_bse_con=>mc_pactg-import.
      status_script_context_set 'IT_STEP_OUTCOMES' it_step_outcomes
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XIRHDR' gs_acdoc_infocus-x-achdr
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YIRHDR' gs_acdoc_infocus-y-achdr
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes
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

    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XIRHDR' gs_acdoc_infocus-x-achdr space.
      brf_context_set 'IS_YIRHDR' gs_acdoc_infocus-y-achdr space.
      brf_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm space.
      brf_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm space.
      brf_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes space.
      brf_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes space.

      brf_context_set 'IS_STEP_OUTCOME' is_step_outcome space.
      brf_context_set 'IT_STEP_OUTCOME' it_step_outcomes space.

      brf_context_set 'C_NEW_OUTCOME' c_new_outcome c_true.
      brf_context_set 'C_STOP_SET'    c_stop_set c_true.
      brf_context_set 'CT_ACT_RECIPIENTS' ct_act_recipients c_true.
      brf_context_set 'CT_MESSAGES' ct_messages c_true.

***Execute the BRF fucntion
      single_brf_function_process c_brf_object-fmac i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        brf_context_get 'C_NEW_OUTCOME' c_new_outcome.
        brf_context_get 'C_STOP_SET'    c_stop_set.
        brf_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.
        brf_context_get 'CT_MESSAGES' ct_messages.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_acdoc_infocus-x-achdr-actyp.

***Profile BADI
    IF ref_badi_fmac_all IS BOUND.
      CALL BADI ref_badi_fmac_all->status_flow_outcome_set
        EXPORTING
          flt_val           = gs_acdoc_infocus-x-achdr-accom
          is_step_outcome   = is_step_outcome
          it_step_outcomes  = it_step_outcomes
          is_xachdr         = gs_acdoc_infocus-x-achdr
          is_yachdr         = gs_acdoc_infocus-y-achdr
          it_xacitm         = gs_acdoc_infocus-x-acitm
          it_yacitm         = gs_acdoc_infocus-y-acitm
          it_xacdet         = gs_acdoc_infocus-x-acdet
          it_yacdet         = gs_acdoc_infocus-y-acdet
        CHANGING
          c_new_outcome     = c_new_outcome
          c_stop_set        = c_stop_set
          ct_act_recipients = ct_act_recipients
          ct_messages       = ct_messages.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD status_flow_step_set.
    IF i_bsenm IS NOT INITIAL.
      status_script_context_init.

      status_script_context_set 'FLT_VAL' gs_acdoc_infocus-x-achdr-accom
                                              /agri/if_bse_con=>mc_pactg-import .

      status_script_context_set 'IS_STEP_OUTCOME' is_step_outcome
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XIRHDR' gs_acdoc_infocus-x-achdr
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YIRHDR' gs_acdoc_infocus-y-achdr
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes
                                /agri/if_bse_con=>mc_pactg-import.

      status_script_context_set 'C_NEW_OUTCOME'  c_new_outcome
                                /agri/if_bse_con=>mc_pactg-export.

      status_script_context_set 'CT_ACT_RECIPIENTS'  ct_act_recipients
                          /agri/if_bse_con=>mc_pactg-export.

      status_script_execute i_bsenm.

      status_script_context_get 'C_NEW_OUTCOME' c_new_outcome.
      status_script_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.
    ENDIF.

    IF i_brf_function IS NOT INITIAL.
** set value of  parameter
      brf_context_init.
      brf_context_set 'IS_XIRHDR' gs_acdoc_infocus-x-achdr space.
      brf_context_set 'IS_YIRHDR' gs_acdoc_infocus-y-achdr space.
      brf_context_set 'IS_XACITM' gs_acdoc_infocus-x-acitm space.
      brf_context_set 'IS_YACITM' gs_acdoc_infocus-y-acitm space.
      brf_context_set 'IT_XACDES' gs_acdoc_infocus-x-acdes space.
      brf_context_set 'IT_YACDES' gs_acdoc_infocus-y-acdes space.

      brf_context_set 'IS_STEP_OUTCOME' is_step_outcome space.
      brf_context_set 'C_NEW_OUTCOME' c_new_outcome c_true.
      brf_context_set 'CT_ACT_RECIPIENTS' ct_act_recipients c_true.

***Execute the BRF fucntion
      single_brf_function_process c_brf_object-fmac i_brf_function.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.

        brf_context_get 'C_NEW_OUTCOME' c_new_outcome.
        brf_context_get 'CT_ACT_RECIPIENTS' ct_act_recipients.


      ENDIF.
    ENDIF.

    PERFORM badi_reference_get USING gs_acdoc_infocus-x-achdr-actyp.

***Profile BADI
    IF ref_badi_fmac_all IS BOUND.
      CALL BADI ref_badi_fmac_all->status_flow_step_set
        EXPORTING
          flt_val           = gs_acdoc_infocus-x-achdr-accom
          is_step_outcome   = is_step_outcome
          is_xachdr         = gs_acdoc_infocus-x-achdr
          is_yachdr         = gs_acdoc_infocus-y-achdr
          it_xacitm         = gs_acdoc_infocus-x-acitm
          it_yacitm         = gs_acdoc_infocus-y-acitm
          it_xacdet         = gs_acdoc_infocus-x-acdet
          it_yacdet         = gs_acdoc_infocus-y-acdet
        CHANGING
          c_new_outcome     = c_new_outcome
          ct_act_recipients = ct_act_recipients.
    ENDIF.

  ENDMETHOD.                    "brf_function_process

  METHOD appl_data_get.
    DATA: lwa_data_ref  TYPE /agri/s_gdataref.

    GET REFERENCE OF gs_acdoc_infocus-x-achdr INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/S_FMACHDR'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_acdoc_infocus-x-acitm INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_FMACITM'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_acdoc_infocus-x-acitm INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_FMACDET'.
    APPEND lwa_data_ref TO ct_ref_data.

    CLEAR lwa_data_ref.
    GET REFERENCE OF gs_acdoc_infocus-x-acdes INTO lwa_data_ref-value.
    lwa_data_ref-name = '/AGRI/T_FMACHDRT'.
    APPEND lwa_data_ref TO ct_ref_data.
    CLEAR lwa_data_ref.

  ENDMETHOD.                    "appl_data_get

ENDCLASS.               "LCL_STATUS_HANDLER
