************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_ACT_MAINTAIN                               *
* Tcode          : ZABS_FMACTIVITIES                                   *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.23.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Activities Maintainence data preparation and        *
*                  diaplay data                                        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_toolbar.

*--Workarea declaration
    DATA: ls_toolbar  TYPE stb_button.

    IF r_amain IS NOT INITIAL.
*--Edit
      MOVE zcl_abs_abap_maintain=>c_tb_fncode_change TO
      ls_toolbar-function. "'CHANGE'
      IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display.
        MOVE icon_change     TO ls_toolbar-icon.
      ELSE.
        MOVE icon_display      TO ls_toolbar-icon.
      ENDIF.
      MOVE zcl_abs_abap_maintain=>c_tb_quickinfo     TO
      ls_toolbar-quickinfo."'DISPLAY<->CHANGE'
      MOVE space              TO ls_toolbar-text.
      MOVE space              TO ls_toolbar-disabled.
      APPEND ls_toolbar       TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "handle_user_command

*--Handle Data Changed
  METHOD handle_data_changed.

    IF e_onf4 IS INITIAL.
*--Calling handle data changed subroutine
      PERFORM handle_data_changed USING er_data_changed.

*--Set manual changes flag
      IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
        gs_variables-manual_changes = abap_true.
      ELSE.
        gs_variables-manual_changes = abap_false.
      ENDIF.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'
    ENDIF."E_ONF4

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*& Form FCODE_ENTR
*&---------------------------------------------------------------------*
*& FCODE_ENTR
*&---------------------------------------------------------------------*
FORM fcode_entr.

*--Fetching activities data
  PERFORM get_act_data.
  CLEAR gs_variables-error.
*--Processing Activities data to perform validations
  LOOP AT gt_act_display ASSIGNING FIELD-SYMBOL(<fs_act_display>).
    IF gs_variables-error IS INITIAL.
      PERFORM validations CHANGING <fs_act_display>.
    ENDIF.
*--When create activities is active
    IF r_acrea IS NOT INITIAL.
      PERFORM styles_prepare CHANGING <fs_act_display>.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCREEN_MODIFY
*&---------------------------------------------------------------------*
*& SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.

*--Screen Validations
  LOOP AT SCREEN.
*--When maintain activities is active
    IF r_aupl = abap_true.
      REFRESH: s_idactv.
      IF screen-group1 = 'K1'.
        screen-active = '0'.
      ENDIF.
*--When create activities is active
    ELSEIF r_acrea = abap_true.
      IF screen-group1 IS NOT INITIAL.
        screen-active = '0'.
      ENDIF.
*--When maintain activities is active
    ELSEIF r_amain = abap_true.
      CLEAR p_path.
      IF screen-group1 = 'P'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH: gt_fmacact, gt_fmacactt, gt_act_display, gt_act_old.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_ACT_DATA
*&---------------------------------------------------------------------*
*& GET_ACT_DATA
*&---------------------------------------------------------------------*
FORM get_act_data.

  IF r_acrea IS NOT INITIAL.
*--Fetching Activities data
    SELECT *
    FROM /agri/fmacact
    INTO TABLE gt_fmacact
    FOR ALL ENTRIES IN gt_act_display
    WHERE idactv = gt_act_display-idactv.
    IF sy-subrc <> 0.
      DATA(lv_subrc) = sy-subrc.
    ENDIF.

  ELSEIF r_amain IS NOT INITIAL.
*--Fetching Activities data
    SELECT *
      FROM /agri/fmacact
    INTO TABLE gt_fmacact
    WHERE idactv IN s_idactv.
    IF sy-subrc = 0.
      lv_subrc = sy-subrc.
    ENDIF.
  ENDIF.

  IF lv_subrc = 0.
    SORT gt_fmacact BY idactv.
    DATA(lt_fmacact_temp) = gt_fmacact.
    DELETE ADJACENT DUPLICATES FROM lt_fmacact_temp COMPARING idactv.
*--Fetching description from activities data
    SELECT *
      FROM /agri/fmacactt
      INTO TABLE gt_fmacactt
      FOR ALL ENTRIES IN lt_fmacact_temp
      WHERE idactv = lt_fmacact_temp-idactv.
    IF sy-subrc = 0.
      SORT gt_fmacactt BY idactv spras.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  IF sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_onli    "'ONLI'
   OR sy-ucomm = space.
*--Validating Activity ID when maintain activities is active
    IF r_amain IS NOT INITIAL.
      IF s_idactv IS INITIAL.
        MESSAGE e060(zabs_msgcls).
      ENDIF.
    ENDIF.
*--Validating file path when uplaod activities is active
    IF r_aupl IS NOT INITIAL.
      IF p_path IS INITIAL.
        MESSAGE e061(zabs_msgcls).
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_EXCEL_DATA
*&---------------------------------------------------------------------*
*& get_excel_data
*&---------------------------------------------------------------------*
FORM get_excel_data.

*--Workarea declaration
  DATA :ls_fmacact_temp  TYPE /agri/fmacact,
        ls_fmacactt_temp TYPE /agri/fmacactt,

*--Internal table declaration
        lt_tab_raw       TYPE truxs_t_text_data,
        lt_upl_act       TYPE STANDARD TABLE OF
                           zabs_str_actxls_maintain,
        lt_update        TYPE STANDARD TABLE OF /agri/fmacact,
        lt_insert        TYPE STANDARD TABLE OF /agri/fmacact,
        lt_delete        TYPE STANDARD TABLE OF /agri/fmacact,

        lt_update_tt     TYPE STANDARD TABLE OF /agri/fmacactt,
        lt_insert_tt     TYPE STANDARD TABLE OF /agri/fmacactt,
        lt_delete_tt     TYPE STANDARD TABLE OF /agri/fmacactt,
        it_taba          TYPE STANDARD TABLE OF dd07v,
        it_tabn          TYPE STANDARD TABLE OF dd07v,
        lr_table         TYPE REF TO cl_salv_table,
        lr_columns       TYPE REF TO cl_salv_columns_table,

*--Local variable
        lv_msg           TYPE REF TO cx_salv_msg,
        lv_errors        TYPE char1,
        lv_tabix         TYPE sy-tabix,
        lv_file          TYPE rlgrap-filename.

*--FM to convert excel data format to sap format
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = abap_true
      i_tab_raw_data       = lt_tab_raw
      i_filename           = p_path
    TABLES
      i_tab_converted_data = lt_upl_act
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.

    LEAVE LIST-PROCESSING.
    LEAVE TO SCREEN 0.

  ELSE.

*--Fetch Farm Management Activity data
    SELECT *
      FROM /agri/fmacact
    INTO TABLE gt_fmacact.
    IF sy-subrc = 0.
      SORT gt_fmacact BY idactv.
      DATA(lt_fmacact_temp) = gt_fmacact.
      DELETE ADJACENT DUPLICATES FROM lt_fmacact_temp COMPARING idactv.

      SELECT *
        FROM /agri/fmacactt
      INTO TABLE gt_fmacactt
        FOR ALL ENTRIES IN lt_fmacact_temp
        WHERE idactv = lt_fmacact_temp-idactv.
      IF sy-subrc = 0.
        SORT gt_fmacactt BY idactv.
      ENDIF.
    ENDIF.

*--Processing excel data with validations to fill the final table
    LOOP AT lt_upl_act ASSIGNING
    FIELD-SYMBOL(<fs_upl_act>).

      lv_tabix = sy-tabix + 1.
      LOOP AT lt_upl_act INTO DATA(ls_upl_act) FROM lv_tabix.
        IF <fs_upl_act>-idactv EQ ls_upl_act-idactv.
          <fs_upl_act>-msgtxt = TEXT-020.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_warning.
          "'@09@'
        ENDIF.
      ENDLOOP.

      IF <fs_upl_act>-msgtxt IS NOT INITIAL.
        CONTINUE.
      ENDIF.

*--Validating whether Activity ID is mandatory or not
      IF  <fs_upl_act>-idactv IS INITIAL.
        <fs_upl_act>-msgtxt = TEXT-002.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@'
        CONTINUE.
      ENDIF.

      IF  <fs_upl_act>-delete NE abap_true.

        <fs_upl_act>-msgtyp =
       zcl_abs_abap_maintain=>c_icon_error. "'@0A@'

*--Validating whether Description(PT) is mandatory or not
        IF  <fs_upl_act>-ptdesc IS INITIAL.
          <fs_upl_act>-msgtxt = TEXT-004.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@' .
          CONTINUE.
        ENDIF.

*--Checking whether Activity Type is given for Billable YES
        IF <fs_upl_act>-bill = zcl_abs_abap_maintain=>c_bill_yes AND
        "'YES'
        <fs_upl_act>-actype IS INITIAL.
          <fs_upl_act>-msgtxt = TEXT-005.
          CONTINUE.
        ENDIF.

        IF <fs_upl_act>-msgtxt IS INITIAL.
          CLEAR <fs_upl_act>-msgtyp.
        ENDIF.

*--Validating Activity Category
        CALL FUNCTION 'DD_DOMA_GET'
          EXPORTING
            domain_name   = zcl_abs_abap_maintain=>c_domain_actcg "'ZABS_DOM_ACTCG'
            langu         = sy-langu
            withtext      = abap_true
          TABLES
            dd07v_tab_a   = it_taba
            dd07v_tab_n   = it_tabn
          EXCEPTIONS
            illegal_value = 1
            op_failure    = 2
            OTHERS        = 3.
        IF sy-subrc EQ 0.
          READ TABLE it_taba TRANSPORTING NO FIELDS
                WITH KEY domvalue_l = <fs_upl_act>-zzactcg.
          IF sy-subrc NE 0.
            <fs_upl_act>-msgtxt = TEXT-023.
            <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

*--Inserting, Updating, deleting records from Farm management Resource
*table
      IF <fs_upl_act>-msgtyp IS INITIAL.
        IF  <fs_upl_act>-delete EQ abap_true.
*--Deleting records from /agri/fmacact
          READ TABLE gt_fmacact INTO DATA(ls_fmacact)
                            WITH KEY idactv =
                                <fs_upl_act>-idactv
                            BINARY SEARCH.
          IF sy-subrc = 0.
            APPEND ls_fmacact TO lt_delete.
            CLEAR ls_fmacact.
            <fs_upl_act>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_success. "'@08@'
            <fs_upl_act>-msgtxt = TEXT-006.
          ELSE.
            <fs_upl_act>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
            <fs_upl_act>-msgtxt = TEXT-007.
          ENDIF.
*--Deleting records from standard table /agri/fmacactt
          READ TABLE gt_fmacactt INTO DATA(ls_fmacactt)
                           WITH KEY idactv =
                               <fs_upl_act>-idactv
                           BINARY SEARCH.
          IF sy-subrc = 0.
            APPEND ls_fmacactt TO lt_delete_tt.
            CLEAR ls_fmacactt.
            <fs_upl_act>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_success. "'@08@'
            <fs_upl_act>-msgtxt = TEXT-006.
          ELSE.
            <fs_upl_act>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
            <fs_upl_act>-msgtxt = TEXT-007.
          ENDIF.

        ELSE.

*--Updating records into /agri/fmacact
          READ TABLE gt_fmacact INTO ls_fmacact
                                      WITH KEY idactv =
                                      <fs_upl_act>-idactv
                                      BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR ls_fmacact_temp.
            MOVE-CORRESPONDING <fs_upl_act> TO ls_fmacact_temp.
            ls_fmacact_temp-mandt = sy-mandt.
            IF  ls_fmacact_temp <> ls_fmacact.
              ls_fmacact-rstype = ls_fmacact_temp-rstype.
              ls_fmacact-bill   = ls_fmacact_temp-bill.
              ls_fmacact-actype = ls_fmacact_temp-actype.
              ls_fmacact-zzactcg = ls_fmacact_temp-zzactcg.
              APPEND ls_fmacact TO lt_update.
              CLEAR ls_fmacact.
              <fs_upl_act>-msgtyp =
              zcl_abs_abap_maintain=>c_icon_success. "'@08@'
              <fs_upl_act>-msgtxt = TEXT-008.
            ELSE.
              <fs_upl_act>-msgtyp =
              zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
              <fs_upl_act>-msgtxt = TEXT-009.
            ENDIF.
          ELSE.
*--Inserting records into /agri/fmacact
            ls_fmacact-idactv = <fs_upl_act>-idactv.
            ls_fmacact-rstype = <fs_upl_act>-rstype.
            ls_fmacact-bill   = <fs_upl_act>-bill.
            ls_fmacact-actype = <fs_upl_act>-actype.
            ls_fmacact-zzactcg = <fs_upl_act>-zzactcg.
            APPEND ls_fmacact TO lt_insert.
            CLEAR ls_fmacact.
            <fs_upl_act>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_success. "'@08@'
            <fs_upl_act>-msgtxt = TEXT-010.
          ENDIF.

*--Updating records into /agri/fmacactt
          CLEAR ls_fmacactt.
          READ TABLE gt_fmacactt INTO ls_fmacactt
                                    WITH KEY idactv =
                                    <fs_upl_act>-idactv
                                    BINARY SEARCH.
          IF sy-subrc EQ 0.

            READ TABLE gt_fmacactt INTO ls_fmacactt
            WITH KEY spras = zcl_abs_abap_maintain=>c_spras_english.
            "'E'
            IF sy-subrc = 0.
              IF <fs_upl_act>-endesc NE ls_fmacactt-description.
                ls_fmacactt-description = <fs_upl_act>-endesc.
                APPEND ls_fmacactt TO lt_update_tt.
                CLEAR ls_fmacactt.
                <fs_upl_act>-msgtyp =
              zcl_abs_abap_maintain=>c_icon_success. "'@08@'
                <fs_upl_act>-msgtxt = TEXT-008.
              ELSE.
                <fs_upl_act>-msgtyp =
             zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
                <fs_upl_act>-msgtxt = TEXT-009.
              ENDIF.
            ENDIF.

            READ TABLE gt_fmacactt INTO ls_fmacactt
            WITH KEY spras = zcl_abs_abap_maintain=>c_spras_portuguese.
            "'P'
            IF sy-subrc = 0.
              IF <fs_upl_act>-ptdesc NE ls_fmacactt-description.
                ls_fmacactt-description = <fs_upl_act>-ptdesc.
                APPEND ls_fmacactt TO lt_update_tt.
                CLEAR ls_fmacactt.
                <fs_upl_act>-msgtyp =
               zcl_abs_abap_maintain=>c_icon_success. "'@08@'
                <fs_upl_act>-msgtxt = TEXT-008.
              ELSE.
                <fs_upl_act>-msgtyp =
             zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
                <fs_upl_act>-msgtxt = TEXT-009.
              ENDIF.
            ENDIF.
          ELSE.
*--Inserting records into /agri/fmacactt
            IF <fs_upl_act>-ptdesc IS NOT INITIAL.
              ls_fmacactt-idactv      = <fs_upl_act>-idactv.
              ls_fmacactt-spras       =
              zcl_abs_abap_maintain=>c_spras_portuguese. "'P'
              ls_fmacactt-description = <fs_upl_act>-ptdesc.
              APPEND ls_fmacactt TO lt_insert_tt.
              CLEAR ls_fmacactt.
              <fs_upl_act>-msgtyp =
             zcl_abs_abap_maintain=>c_icon_success. "'@08@'
              <fs_upl_act>-msgtxt = TEXT-010.
            ENDIF.

            IF <fs_upl_act>-endesc IS NOT INITIAL.
              ls_fmacactt-idactv      = <fs_upl_act>-idactv.
              ls_fmacactt-spras       =
              zcl_abs_abap_maintain=>c_spras_english. "'E'
              ls_fmacactt-description = <fs_upl_act>-endesc.
              APPEND ls_fmacactt TO lt_insert_tt.
              CLEAR ls_fmacactt.
              <fs_upl_act>-msgtyp =
             zcl_abs_abap_maintain=>c_icon_success. "'@08@'
              <fs_upl_act>-msgtxt = TEXT-010.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*--Deleting record from Farm management activity table
    IF lt_delete IS NOT INITIAL.
      DELETE /agri/fmacact FROM TABLE lt_delete.
    ENDIF.

*--Inserting record from Farm management activity table
    IF lt_insert IS NOT INITIAL.
      INSERT /agri/fmacact FROM TABLE lt_insert.
    ENDIF.

*--Updating record from Farm management activity table
    IF lt_update IS NOT INITIAL.
      UPDATE /agri/fmacact FROM TABLE lt_update.
    ENDIF.

*--Deleting record from Farm management activity table
    IF lt_delete_tt IS NOT INITIAL.
      DELETE /agri/fmacactt FROM TABLE lt_delete_tt.
    ENDIF.

*--Inserting record from Farm management activity table
    IF lt_insert_tt IS NOT INITIAL.
      INSERT /agri/fmacactt FROM TABLE lt_insert_tt.
    ENDIF.

*--Updating record from Farm management activity table
    IF lt_update_tt IS NOT INITIAL.
      UPDATE /agri/fmacactt FROM TABLE lt_update_tt.
    ENDIF.

    COMMIT WORK.

  ENDIF.

*--Displaying data
  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = lt_upl_act ).
    CATCH  cx_salv_msg INTO lv_msg .
  ENDTRY.

  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  lr_table->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FILEPATH
*&---------------------------------------------------------------------*
*& F4_FILEPATH
*&---------------------------------------------------------------------*
FORM f4_filepath.

*--Local variable declaration
  DATA: lv_rcount TYPE i,

*--internal table declaration
        lt_path   TYPE filetable.

*--Calling method to get File path
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = lt_path
      rc         = lv_rcount.

  IF lt_path IS NOT INITIAL.
    READ TABLE lt_path INTO DATA(ls_path) INDEX 1.
    IF sy-subrc EQ 0.
      p_path = ls_path.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.

*--Internal table declaration
  DATA lt_exclude TYPE STANDARD TABLE OF sy-ucomm.

  IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display  "'A'
 AND r_acrea IS INITIAL.
    APPEND zcl_abs_abap_maintain=>c_ucomm_save TO lt_exclude. "'SAVE'
  ENDIF.

*--Status set for screen 100
  SET PF-STATUS 'S100' EXCLUDING lt_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set.

*--Local variable declaration
  DATA lv_text(10).

  IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display. "'A'
    lv_text = TEXT-021.
  ELSEIF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_change."'V'
    lv_text = TEXT-022.
  ENDIF.

*--Title set for screen 100
  SET TITLEBAR 'T100' WITH lv_text.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
FORM controls_display.

*--Workarea declaration
  DATA: ls_layout TYPE lvc_s_layo,

*--Internal table declaration
        lt_fcat   TYPE lvc_t_fcat.

*--Local variable declaration
  DATA: lv_input TYPE int4.

  IF gobj_docking IS INITIAL.

*--Create the instance of docking container
    CREATE OBJECT gobj_docking
      EXPORTING
        ratio = zcl_abs_abap_maintain=>c_doc_obj_ratio. "'95'

    IF gobj_docking IS BOUND.

*--Create the instance of ALV Grid
      CREATE OBJECT gobj_grid
        EXPORTING
          i_parent = gobj_docking.

      IF gobj_grid IS BOUND.

*--Fcodes Exclusions
        PERFORM exclude_tb_functions CHANGING i_exclude.

*--Building the fieldcatelog
        PERFORM fcat_prepare CHANGING lt_fcat.

*--Prepare layout
        PERFORM layout_prepare CHANGING ls_layout.

        gt_act_old = gt_act_display.
*--Display data on Grid
        CALL METHOD gobj_grid->set_table_for_first_display
          EXPORTING
            i_structure_name     =
                                   zcl_abs_abap_maintain=>c_str_act_display
            i_save               = zcl_abs_abap_maintain=>c_variant_save
            "'A'
            i_default            = abap_true
            is_layout            = ls_layout
            it_toolbar_excluding = i_exclude
          CHANGING
            it_outtab            = gt_act_display  "gt_final
            it_fieldcatalog      = lt_fcat.

*--Register Control Events
        PERFORM events_register.

      ENDIF.
    ENDIF.

  ELSE.

*--Prepare layout
    PERFORM layout_prepare CHANGING ls_layout.
    gobj_grid->set_frontend_layout( EXPORTING  is_layout = ls_layout ).
    gobj_grid->refresh_table_display( ).

  ENDIF.

  IF NOT gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display.
    "'A'
    lv_input = 1.
  ELSEIF r_acrea IS NOT INITIAL.
    lv_input = 1.
  ENDIF.

*--Make the Grid Editable
  gobj_grid->set_ready_for_input( lv_input ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed  USING p_er_data_changed
                    TYPE REF TO cl_alv_changed_data_protocol.

*--Local Data declarations.
  DATA :lwa_mod_cell TYPE lvc_s_modi,
        lwa_ins_rows TYPE lvc_s_moce,

        lwa_mod      TYPE ty_mod,

*--Internal table declaration
        lt_imod      TYPE TABLE OF ty_mod,
        lv_valid.

*--Field-symbol declaration
  FIELD-SYMBOLS: <ft_append_row> TYPE ANY TABLE.

*--When Create activity is active
  IF r_acrea IS NOT INITIAL.
    IF p_er_data_changed->mt_inserted_rows IS NOT INITIAL.
      ASSIGN p_er_data_changed->mp_mod_rows->* TO <ft_append_row>.
      IF <ft_append_row> IS ASSIGNED.
        APPEND LINES OF <ft_append_row> TO gt_act_display.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT p_er_data_changed->mt_good_cells INTO lwa_mod_cell.
    lwa_mod-row  = lwa_mod_cell-row_id.
    APPEND lwa_mod TO lt_imod.
  ENDLOOP.

  gs_variables-changes = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module TRANSACTION_INIT OUTPUT
*&---------------------------------------------------------------------*
*& TRANSACTION_INIT
*&---------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
  PERFORM transaction_init.
ENDMODULE.

FORM transaction_init.
  IF gs_variables-mode IS INITIAL.
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_display. "'A'
  ENDIF.
ENDFORM.                    " TRANSACTION_INIT

*&---------------------------------------------------------------------*
*& Form DISPLAY_ACTIVITY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_ACTIVITY_DATA
*&---------------------------------------------------------------------*
FORM display_activity_data.
*--Calling screen to display activity maintain data
  IF r_amain IS NOT INITIAL OR r_acrea IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.

*--Workarea Declaration
  DATA: lwa_exclude TYPE ui_func.

  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND lwa_exclude TO pt_exclude.
*--When Create Aactivity is active
  IF r_acrea IS INITIAL.
    lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND lwa_exclude TO pt_exclude.
  ENDIF.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND lwa_exclude TO pt_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCAT_PREPARE
*&---------------------------------------------------------------------*
*& FCAT_PREPARE
*&---------------------------------------------------------------------*
FORM fcat_prepare CHANGING ct_fcat TYPE lvc_t_fcat.

*--Workarea declaration
  DATA lwa_fcat TYPE lvc_s_fcat.

*--Preparing field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = zcl_abs_abap_maintain=>c_str_act_display
    CHANGING
      ct_fieldcat      = ct_fcat.

  lwa_fcat-edit = abap_true.

  MODIFY ct_fcat FROM lwa_fcat
         TRANSPORTING edit
         WHERE key = space.

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN zcl_abs_abap_maintain=>c_fieldname_updkz. "'UPDKZ'
        <fs_fcat>-tech = abap_true.
      WHEN zcl_abs_abap_maintain=>c_fieldname_idactv."'IDACTV'
        IF r_amain IS NOT INITIAL.
          <fs_fcat>-edit = space.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LAYOUT_PREPARE
*&---------------------------------------------------------------------*
*& LAYOUT_PREPARE
*&---------------------------------------------------------------------*
FORM layout_prepare  CHANGING cs_layout TYPE lvc_s_layo.

*--Preparing Layout
  cs_layout-cwidth_opt = abap_true.
  cs_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
  cs_layout-stylefname = zcl_abs_abap_maintain=>c_layout_stylefname.
  "'STYLE'

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EVENTS_REGISTER
*&---------------------------------------------------------------------*
*& EVENTS_REGISTER
*&---------------------------------------------------------------------*
FORM events_register.

*--creating an instance for the event handler
  CREATE OBJECT gr_event_handler.

  CALL METHOD gobj_grid->register_edit_event
    EXPORTING
      i_event_id = /irm/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

*--Set the handler for local event handler class
  SET HANDLER gr_event_handler->handle_user_command FOR gobj_grid.
  SET HANDLER gr_event_handler->handle_data_changed FOR gobj_grid.
  SET HANDLER gr_event_handler->handle_toolbar      FOR gobj_grid.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form STYLES_PREPARE
*&---------------------------------------------------------------------*
*& STYLES_PREPARE
*&---------------------------------------------------------------------*
FORM styles_prepare  CHANGING ls_act_display TYPE zabs_str_act_maintain.

  DATA: lwa_style  TYPE lvc_s_styl.

*--Disabling Activity ID field
  IF ls_act_display-idactv IS NOT INITIAL.
    lwa_style-fieldname = zcl_abs_abap_maintain=>c_fieldname_idactv.
    "'IDACTV'
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE ls_act_display-style.
    CLEAR lwa_style.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

*--Local variable declarations
  DATA: lv_tcode       TYPE sy-ucomm,
        lv_routine(32) TYPE c VALUE 'FCODE_'.

  lv_tcode = ok_code.
  CLEAR ok_code.
  CONCATENATE lv_routine lv_tcode INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (sy-repid) IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_BACK
*&---------------------------------------------------------------------*
*& FCODE_BACK
*----------------------------------------------------------------------*
FORM fcode_back.

  DATA: lv_ans(1),
        lv_valid,
        lv_error TYPE xfeld.

*--Local variable declaration
  DATA(lv_modified) = gobj_grid->data_modified_check( ).
  IF lv_modified IS NOT INITIAL.
    CALL METHOD gobj_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS NOT INITIAL.
      gs_variables-changes = abap_true.
    ELSE.
      CLEAR gs_variables-changes.
    ENDIF.
  ENDIF.

*--Calling check_changes subroutine
  PERFORM check_changes CHANGING lv_ans.

  CASE lv_ans.
    WHEN '1'.
      PERFORM data_save CHANGING lv_error.
    WHEN '2'.
      LEAVE TO SCREEN 0.
    WHEN '9'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_EXIT
*&---------------------------------------------------------------------*
*& FCODE_EXIT
*----------------------------------------------------------------------*
FORM fcode_exit.

  DATA: lv_ans(1),
        lv_valid,
        lv_error TYPE xfeld.

*--Local variable declaration
  DATA(lv_modified) = gobj_grid->data_modified_check( ).
  IF lv_modified IS NOT INITIAL.
    CALL METHOD gobj_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS NOT INITIAL.
      gs_variables-changes = abap_true.
    ELSE.
      CLEAR gs_variables-changes.
    ENDIF.
  ENDIF.

*--Calling check_changes subroutine
  PERFORM check_changes CHANGING lv_ans.

  CASE lv_ans.
    WHEN '1'.
      PERFORM data_save CHANGING lv_error.
    WHEN '2'.
      LEAVE TO SCREEN 0.
    WHEN '9'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_SAVE
*&---------------------------------------------------------------------*
*& FCODE_SAVE
*----------------------------------------------------------------------*
FORM fcode_save.

*--Local variable declaration
  DATA: lv_ans,
        lv_valid,
        lv_error TYPE xfeld.

  DATA(lv_modified) = gobj_grid->data_modified_check( ).
  IF lv_modified IS NOT INITIAL.
    CALL METHOD gobj_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    gs_variables-changes = abap_true.
  ENDIF.

  PERFORM fcode_entr.
  PERFORM check_changes CHANGING lv_ans.

  CASE lv_ans.
    WHEN 1.
      PERFORM data_save CHANGING lv_error.
    WHEN 2.
*--When create activities is active
      IF r_acrea IS NOT INITIAL.
        LOOP AT gt_act_display ASSIGNING FIELD-SYMBOL(<fs_act_display>).
          PERFORM styles_prepare CHANGING <fs_act_display>.
        ENDLOOP.
      ENDIF.
      RETURN.
    WHEN 9.
      MESSAGE i063(zabs_msgcls).
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  CHECK_CHANGES
*&---------------------------------------------------------------------*
*& CHECK_CHANGES
*----------------------------------------------------------------------*
FORM check_changes  CHANGING cv_ans.

  IF gs_variables-error IS NOT INITIAL.
    cv_ans = 2.
    RETURN.
  ENDIF.
  IF gs_variables-changes EQ abap_true.
*--Raise the pop for confirmation
    PERFORM pop_up_confirm USING TEXT-018   "<- Data Save
                                 TEXT-019
                                 "<- Do you want save the changes?
                        CHANGING cv_ans.
    CLEAR gs_variables-changes.
  ELSE.
    cv_ans = 9.
  ENDIF.

ENDFORM.                    " CHECK_CHANGES

*&---------------------------------------------------------------------*
*& Form  POP_UP_CONFIRM
*&---------------------------------------------------------------------*
*& POP_UP_CONFIRM
*----------------------------------------------------------------------*
FORM pop_up_confirm USING iv_title
                          iv_question
                 CHANGING cv_ans.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = iv_title
      text_question = iv_question
      text_button_1 = zcl_abs_abap_maintain=>c_popup_tb_yes "'Yes'
      text_button_2 = zcl_abs_abap_maintain=>c_popup_tb_no  "'No'
    IMPORTING
      answer        = cv_ans.

ENDFORM.                    " POP_UP_CONFIRM

*&---------------------------------------------------------------------*
*& Form  DATA_SAVE
*&---------------------------------------------------------------------*
*& DATA_SAVE
*----------------------------------------------------------------------*
FORM data_save CHANGING cv_error TYPE xfeld.

*--Internal table declaration
  DATA: lt_insert    TYPE STANDARD TABLE OF /agri/fmacact,
        lt_update    TYPE STANDARD TABLE OF /agri/fmacact,
        lt_delete    TYPE STANDARD TABLE OF /agri/fmacact,
        lt_insert_tt TYPE STANDARD TABLE OF /agri/fmacactt,
        lt_update_tt TYPE STANDARD TABLE OF /agri/fmacactt,
        lt_delete_tt TYPE STANDARD TABLE OF /agri/fmacactt,
        lt_msgtab    TYPE esp1_message_tab_type,

*--Workarea declaration
        ls_act_data  TYPE /agri/fmacact,
        ls_msgtab    TYPE esp1_message_wa_type,
        ls_fmacact   TYPE /agri/fmacact,
        ls_fmacactt  TYPE /agri/fmacactt.

  CHECK gs_variables-error IS INITIAL.
  IF r_amain IS NOT INITIAL.

*--Processing the data to delete and update the data
    LOOP AT gt_act_old INTO DATA(ls_act_old).

      READ TABLE gt_act_display INTO DATA(ls_act_display)
        WITH KEY idactv = ls_act_old-idactv.
      IF sy-subrc = 0.
*--Updating records into /AGRI/FMACACTT
        IF ls_act_old NE ls_act_display.
          IF ls_act_old-endesc <> ls_act_display-endesc.
            ls_fmacactt-idactv = ls_act_display-idactv.
            ls_fmacactt-spras  = zcl_abs_abap_maintain=>c_spras_english
            . "'E'
            ls_fmacactt-description = ls_act_display-endesc.
            APPEND ls_fmacactt TO lt_update_tt.
            ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class
               . "'ZABS_MSGCLS'
            ls_msgtab-msgno = '075'.
            ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_success.
            "'S'
            ls_msgtab-msgv1 = ls_fmacactt-description.
            ls_msgtab-msgv2 = ls_fmacactt-idactv.
            ls_msgtab-msgv3 = ls_fmacactt-spras.
            APPEND ls_msgtab TO lt_msgtab.
            CLEAR: ls_fmacactt, ls_msgtab.
          ENDIF.

          IF ls_act_old-ptdesc <> ls_act_display-ptdesc.
            ls_fmacactt-idactv = ls_act_display-idactv.
            ls_fmacactt-spras  =
            zcl_abs_abap_maintain=>c_spras_portuguese. "'P'
            ls_fmacactt-description = ls_act_display-ptdesc.
            APPEND ls_fmacactt TO lt_update_tt.
            ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class
             . "'ZABS_MSGCLS'
            ls_msgtab-msgno = '075'.
            ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_success.
            "'S'
            ls_msgtab-msgv1 = ls_fmacactt-description.
            ls_msgtab-msgv2 = ls_fmacactt-idactv.
            ls_msgtab-msgv3 = ls_fmacactt-spras.
            APPEND ls_msgtab TO lt_msgtab.
            CLEAR: ls_fmacactt, ls_msgtab.
          ENDIF.

*--Updating records into /AGRI/FMACACT
          MOVE-CORRESPONDING ls_act_display TO ls_fmacact.
          MOVE-CORRESPONDING ls_act_old TO ls_act_data.
          IF ls_act_data <> ls_fmacact.
            APPEND ls_fmacact TO lt_update.
            ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class
            . "'ZABS_MSGCLS'
            ls_msgtab-msgno = '076'.
            ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_success.
            "'S'
            ls_msgtab-msgv1 = ls_fmacact-idactv.
            APPEND ls_msgtab TO lt_msgtab.
            CLEAR: ls_fmacact, ls_act_data, ls_msgtab.
          ENDIF.
          CLEAR: ls_act_display.
        ENDIF.

      ELSE.

*--Deleting records from /AGRI/FMACACT
        MOVE-CORRESPONDING ls_act_old TO ls_fmacact.
        APPEND ls_fmacact TO lt_delete.
*--Deleting records from /AGRI/FMACACTT
        READ TABLE gt_fmacactt TRANSPORTING NO FIELDS
        WITH KEY idactv = ls_act_old-idactv.
        IF sy-subrc = 0.
          ls_fmacactt-idactv = ls_act_old-idactv.
          ls_fmacactt-spras  = zcl_abs_abap_maintain=>c_spras_english.
          ls_fmacactt-description = ls_act_old-endesc.
          APPEND ls_fmacactt TO lt_delete_tt.

          ls_fmacactt-idactv = ls_act_old-idactv.
          ls_fmacactt-spras  = zcl_abs_abap_maintain=>c_spras_portuguese.
          ls_fmacactt-description = ls_act_old-ptdesc.
          APPEND ls_fmacactt TO lt_delete_tt.

          ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
          "'ZABS_MSGCLS'
          ls_msgtab-msgno = '077'.
          ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
          ls_msgtab-msgv1 = ls_fmacact-idactv.
          APPEND ls_msgtab TO lt_msgtab.
          CLEAR : ls_act_data, ls_fmacact, ls_fmacactt,ls_msgtab.

        ENDIF.

      ENDIF.

    ENDLOOP.

*--Deleting the records from /AGRI/FMACACT
    IF NOT lt_delete IS INITIAL.
      DELETE /agri/fmacact FROM TABLE lt_delete.
    ENDIF.

*--Deleting the records from /AGRI/FMACACTT
    IF lt_delete_tt IS NOT INITIAL.
      DELETE /agri/fmacactt FROM TABLE lt_delete_tt.
    ENDIF.

*--Updating records /AGRI/FMACACT
    IF NOT lt_update IS INITIAL.
      UPDATE /agri/fmacact FROM TABLE lt_update.
    ENDIF.

*--Updating records /AGRI/FMACACTT
    IF NOT lt_update_tt IS INITIAL.
*      UPDATE /agri/fmacactt FROM TABLE lt_update_tt.
      MODIFY /agri/fmacactt FROM TABLE lt_update_tt.
    ENDIF.

*--When Create activities is active
  ELSEIF r_acrea IS NOT INITIAL.
*--Inserting records into /AGRI/FMACACT
    CLEAR ls_act_display.
    LOOP AT gt_act_display INTO ls_act_display.
      MOVE-CORRESPONDING ls_act_display TO ls_fmacact.
      APPEND ls_fmacact TO lt_insert.
      ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      "'ZABS_MSGCLS'
      ls_msgtab-msgno = '072'.
      ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      ls_msgtab-msgv1 = ls_fmacact-idactv.
      APPEND ls_msgtab TO lt_msgtab.
      CLEAR : ls_fmacact, ls_msgtab.
    ENDLOOP.

*--insert the data
    IF NOT lt_insert IS INITIAL.
      INSERT /agri/fmacact FROM TABLE lt_insert.
    ENDIF.

*--Inserting records into /AGRI/FMACACTT
    CLEAR ls_act_display.
    LOOP AT gt_act_display INTO ls_act_display.
      IF ls_act_display-ptdesc IS NOT INITIAL.
        ls_fmacactt-idactv      = ls_act_display-idactv.
        ls_fmacactt-spras       =
        zcl_abs_abap_maintain=>c_spras_portuguese. "'P'
        ls_fmacactt-description = ls_act_display-ptdesc.
        APPEND ls_fmacactt TO lt_insert_tt.
        CLEAR ls_fmacactt.
      ENDIF.

      IF ls_act_display-endesc IS NOT INITIAL.
        ls_fmacactt-idactv      = ls_act_display-idactv.
        ls_fmacactt-spras       = zcl_abs_abap_maintain=>c_spras_english
        . "'E'
        ls_fmacactt-description = ls_act_display-endesc.
        APPEND ls_fmacactt TO lt_insert_tt.
        CLEAR : ls_fmacactt, ls_msgtab.
      ENDIF.
    ENDLOOP.

    REFRESH gt_act_display.

*--insert the data
    IF NOT lt_insert_tt IS INITIAL.
      INSERT /agri/fmacactt FROM TABLE lt_insert_tt.
    ENDIF.
  ENDIF.

*--Commit
  IF NOT lt_update    IS INITIAL OR
     NOT lt_delete    IS INITIAL OR
     NOT lt_insert    IS INITIAL OR
     NOT lt_insert_tt IS INITIAL OR
     NOT lt_update_tt IS INITIAL OR
     NOT lt_delete_tt IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.
  gt_act_old = gt_act_display.

*--Message display sub-routine
  PERFORM messages_display USING lt_msgtab.

ENDFORM.                    " DATA_SAVE

*&---------------------------------------------------------------------*
*& Form  FCODE_CHANGE
*&---------------------------------------------------------------------*
*& FCODE_CHANGE
*----------------------------------------------------------------------*
FORM fcode_change.

  IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display.
    "'A' "<-Display
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_change.
    "'V' "<-Changes
  ELSE.
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_display. "'A'
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATIONS
*&---------------------------------------------------------------------*
*& VALIDATIONS
*&---------------------------------------------------------------------*
FORM validations CHANGING ls_act_display TYPE zabs_str_act_maintain.

  CLEAR gs_variables-error.

*--When Create resource is active
  IF r_acrea IS NOT INITIAL.
    READ TABLE gt_fmacact TRANSPORTING NO FIELDS
                         WITH KEY idactv = ls_act_display-idactv
                         BINARY SEARCH.
    IF sy-subrc = 0.
      MESSAGE i071(zabs_msgcls) WITH ls_act_display-idactv
     DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      gs_variables-error = abap_true.
      RETURN.
    ENDIF.
  ENDIF.

*--Validating whether Activity ID is mandatory or not
  IF  ls_act_display-idactv IS INITIAL.
    MESSAGE i060(zabs_msgcls) DISPLAY LIKE
    zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

*--Validating whether description(PT) is mandatory or not
  IF  ls_act_display-ptdesc IS INITIAL.
    MESSAGE i069(zabs_msgcls) WITH ls_act_display-idactv
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

*--Checking whether Activity Type is given for Bill YES
  IF ls_act_display-bill = zcl_abs_abap_maintain=>c_bill_yes AND  "'YES'
  ls_act_display-actype IS INITIAL.
    MESSAGE i070(zabs_msgcls) WITH ls_act_display-idactv
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*& MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
FORM messages_display  USING lt_msgtab TYPE esp1_message_tab_type.

  IF lt_msgtab IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_msgtab.
  ENDIF.
  LEAVE LIST-PROCESSING.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_ACT_DATA
*&---------------------------------------------------------------------*
*& BUILD_ACT_DATA
*&---------------------------------------------------------------------*
FORM build_act_data.

*--Workarea declaration
  DATA : ls_act_display TYPE zabs_str_act_maintain.

*--Getting activities data
  PERFORM get_act_data.

*--Processing data to fill final table
  LOOP AT gt_fmacact INTO DATA(ls_fmacact).

    ls_act_display-idactv      = ls_fmacact-idactv.
    ls_act_display-rstype      = ls_fmacact-rstype.
    ls_act_display-bill        = ls_fmacact-bill.
    ls_act_display-actype      = ls_fmacact-actype.
    ls_act_display-zzactcg     = ls_fmacact-zzactcg.

    READ TABLE gt_fmacactt INTO DATA(ls_fmacactt)
    WITH KEY idactv = ls_fmacact-idactv
             spras  = zcl_abs_abap_maintain=>c_spras_portuguese "'P'
             BINARY SEARCH.
    IF sy-subrc = 0.
      ls_act_display-ptdesc    = ls_fmacactt-description.
    ENDIF.

    CLEAR ls_fmacactt.
    READ TABLE gt_fmacactt INTO ls_fmacactt
    WITH KEY idactv = ls_fmacact-idactv
             spras  = zcl_abs_abap_maintain=>c_spras_english "'E'
             BINARY SEARCH.
    IF sy-subrc = 0.
      ls_act_display-endesc    = ls_fmacactt-description.
    ENDIF.

    APPEND ls_act_display TO gt_act_display.
    CLEAR ls_act_display.

  ENDLOOP.

ENDFORM.
