*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_NURSERY_PROCESSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& status_set
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
*& title_set
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
*& controls_display
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
         ls_variant          TYPE disvariant,
         lv_input            TYPE i.

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
        container_name              = 'MATCHAR_0100_CC'
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
        it_outtab                     = gt_batch_char
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

    IF e_ucomm EQ c_fcode-change_colum_batch_char.
      IF gs_variables-refresh_colum_batch_char EQ abap_false.
        gs_variables-refresh_colum_batch_char = abap_true.
      ELSE.
        CLEAR: gs_variables-refresh_colum_batch_char.
      ENDIF.
      gs_variables-refresh_batch_char = abap_true.
    ENDIF.

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

    TYPES: BEGIN OF ty_code,
             code     TYPE qcode,
             kurztext TYPE qtxt_code,
           END OF ty_code.

    TYPES: BEGIN OF ty_value,
             codegruppe TYPE qpk1ac-codegruppe,
             fillr(1)   TYPE c,
             code       TYPE qpk1ac-code,
           END OF ty_value.

*--Local Data declarations
    DATA: lt_selected_values TYPE tt_cawn,
          lv_value           TYPE atwrt,
          lt_code            TYPE STANDARD TABLE OF ty_code,
          lt_return          TYPE STANDARD TABLE OF ddshretval,
          ls_value           TYPE ty_value.

*--Process data to fill values using f4 help
    READ TABLE gt_batch_char ASSIGNING FIELD-SYMBOL(<fs_batch_char>)
                                     INDEX es_row_no-row_id.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    ASSIGN ('(/AGRI/SAPLFMNSM)GS_FPDOC_INFOCUS-X-FPHDR-IWERK')
                    TO FIELD-SYMBOL(<fs_iwerk>).
    IF <fs_iwerk> IS ASSIGNED AND <fs_iwerk> IS NOT INITIAL.
      SELECT SINGLE atinn, werks, katalogart, auswahlmge
        FROM cabn
        INTO @DATA(ls_cabn)
       WHERE atinn EQ @<fs_batch_char>-atinn
         AND werks EQ @<fs_iwerk>.
    ENDIF.

    IF ls_cabn-auswahlmge IS NOT INITIAL.
      SELECT code
        FROM qpac
        INTO TABLE lt_code
       WHERE werks = ls_cabn-werks
         AND katalogart = ls_cabn-katalogart
         AND auswahlmge = ls_cabn-auswahlmge.
      IF sy-subrc EQ 0.
        SELECT code, kurztext
          FROM qpct
          INTO TABLE @DATA(lt_qpct)
           FOR ALL ENTRIES IN @lt_code
         WHERE katalogart EQ @ls_cabn-katalogart
           AND codegruppe EQ @ls_cabn-auswahlmge
           AND code       EQ @lt_code-code
           AND sprache    EQ @sy-langu.
        IF sy-subrc EQ 0.
          SORT lt_qpct BY code.
        ENDIF.

        LOOP AT lt_code ASSIGNING FIELD-SYMBOL(<fs_code>).
          READ TABLE lt_qpct INTO DATA(ls_qpct)
                WITH KEY code = <fs_code>-code
              BINARY SEARCH.
          IF sy-subrc EQ 0.
            <fs_code>-kurztext = ls_qpct-kurztext.
          ENDIF.
        ENDLOOP.

        REFRESH lt_return.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CODE'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            dynprofield     = 'VALUE'
            stepl           = 1
            value           = 'X'
            value_org       = 'S'
          TABLES
            value_tab       = lt_code
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.

        READ TABLE lt_return INTO DATA(ls_return)
              WITH KEY retfield = 'VALUE'.
        IF sy-subrc EQ 0.
          CLEAR ls_value.
          ls_value-codegruppe = ls_cabn-auswahlmge.
          ls_value-code = ls_return-fieldval.
          <fs_batch_char>-value = ls_value.
          READ TABLE lt_code INTO DATA(ls_code)
            WITH KEY code = ls_value-code.
          IF sy-subrc EQ 0.
            <fs_batch_char>-kurztext = ls_code-kurztext.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

*--Calling function to get search help for characteristics
      CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
        EXPORTING
          i_atinn           = <fs_batch_char>-atinn
*         i_display_only    = abap_true
*         i_multiple_values = lv_multiple_values
*         it_sel_value      = lt_selected_values_tmp[]
        IMPORTING
          e_value           = <fs_batch_char>-value
        TABLES
          t_values          = lt_selected_values
        EXCEPTIONS
          charact_not_found = 1
          no_values_found   = 2
          OTHERS            = 3.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = abap_true.
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
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
                                               TO lt_toolbar_excludes.
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
      i_structure_name       = c_str_batch_char  "'ZABST_BTCHCHR'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
    CASE <lwa_fcat>-fieldname.
      WHEN 'ATBEZ' OR 'VALUE' OR 'KURZTEXT'.

      WHEN OTHERS.
        <lwa_fcat>-tech = abap_true.
    ENDCASE.
  ENDLOOP.

  IF gs_variables-overview_mode IS INITIAL.
    READ TABLE lt_fcat ASSIGNING <lwa_fcat>
      WITH KEY fieldname = 'VALUE'.
    IF sy-subrc = 0.
      <lwa_fcat>-edit = abap_true.
      <lwa_fcat>-f4availabl = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& fcode_processing
*&---------------------------------------------------------------------*
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
FORM fcode_cont.
*--Calling FM to save Batch Characteristics
  CALL FUNCTION 'ZABS_FM_BATCH_CHAR_SAVE'.
  LEAVE TO SCREEN 0.
ENDFORM.            "FCODE_CANC

*&---------------------------------------------------------------------*
*& Module  EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& exit_processing
*&---------------------------------------------------------------------*
MODULE exit_processing INPUT.
  PERFORM exit_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form EXIT_PROCESSING
*&---------------------------------------------------------------------*
*& exit_processing
*&---------------------------------------------------------------------*
FORM exit_processing.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_CODE_TEXTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fetch_code_texts .

*--Local data declaration
  DATA: lrt_codegruppe TYPE RANGE OF qcodegrp,
        lrt_code       TYPE RANGE OF qcode,
        lv_codegruppe  TYPE string, "qcodegrp, "string,
        lv_code        TYPE string. "qcode. "string.

  IF gt_batch_char[] IS NOT INITIAL.
    SELECT atinn, atnam, auswahlmge
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
      FOR ALL ENTRIES IN @gt_batch_char
     WHERE atinn = @gt_batch_char-atinn.

    IF sy-subrc EQ 0.
      DELETE lt_cabn WHERE auswahlmge IS INITIAL.
      IF lt_cabn IS NOT INITIAL.
        LOOP AT lt_cabn INTO DATA(lwa_cabn).
          READ TABLE gt_batch_char ASSIGNING FIELD-SYMBOL(<lwa_batch_char>)
            WITH KEY atnam = lwa_cabn-atnam.
          IF sy-subrc EQ 0.
            CLEAR: lv_codegruppe, lv_code.
            DATA(lv_value) = <lwa_batch_char>-value.
            SPLIT lv_value AT space INTO lv_codegruppe lv_code.
            CONDENSE: lv_codegruppe, lv_code.
            IF lv_codegruppe IS NOT INITIAL.
              INSERT INITIAL LINE INTO TABLE lrt_codegruppe
                ASSIGNING FIELD-SYMBOL(<lrs_codegruppe>).
              IF sy-subrc EQ 0.
                <lrs_codegruppe> = 'IEQ'.
                <lrs_codegruppe>-low = lv_codegruppe.
              ENDIF.
            ENDIF.
            IF lv_code IS NOT INITIAL.
              INSERT INITIAL LINE INTO TABLE lrt_code
                ASSIGNING FIELD-SYMBOL(<lrs_code>).
              IF sy-subrc EQ 0.
                <lrs_code> = 'IEQ'.
                <lrs_code>-low = lv_code.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      DATA(lv_katalogart) = '1'.
      SELECT * FROM qpct
        INTO TABLE @DATA(lt_qpct)
       WHERE katalogart EQ @lv_katalogart
         AND codegruppe IN @lrt_codegruppe[]
         AND code       IN @lrt_code[]
         AND sprache    EQ @sy-langu.

      SORT lt_qpct BY codegruppe ASCENDING
                      code       ASCENDING
                      version    DESCENDING.

      LOOP AT lt_cabn INTO lwa_cabn.
        READ TABLE gt_batch_char ASSIGNING <lwa_batch_char>
          WITH KEY atnam = lwa_cabn-atnam.
        IF sy-subrc EQ 0.
          CLEAR: lv_codegruppe, lv_code.
          lv_value = <lwa_batch_char>-value.
          SPLIT lv_value AT space INTO lv_codegruppe lv_code.
          CONDENSE: lv_codegruppe, lv_code.
          READ TABLE lt_qpct INTO DATA(ls_qpct)
            WITH KEY codegruppe = lv_codegruppe
                     code       = lv_code BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_batch_char>-kurztext = ls_qpct-kurztext.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
