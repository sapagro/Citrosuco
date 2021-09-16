************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_RESOURCE_MAINTAIN_SUB                      *
* Tcode          : ZABS_FMRESOURCES                                    *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.18.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Resource Maintainence data preparation and          *
*                  diaplay data                                        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
* Class Implementation used to get changed data
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_toolbar.

*--Workarea declaration
    DATA: ls_toolbar  TYPE stb_button.

    IF r_rmain IS NOT INITIAL.
*--Edit
      MOVE zcl_abs_abap_maintain=>c_tb_fncode_change TO ls_toolbar-function.      "'CHANGE'
      IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display.
        MOVE icon_change     TO ls_toolbar-icon.
      ELSE.
        MOVE icon_display      TO ls_toolbar-icon.
      ENDIF.
      MOVE zcl_abs_abap_maintain=>c_tb_quickinfo     TO ls_toolbar-quickinfo.     "'DISPLAY<->CHANGE'
      MOVE space                                     TO ls_toolbar-text.
      MOVE space                                     TO ls_toolbar-disabled.
      APPEND ls_toolbar                              TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.

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

*--Internal table declarations
  DATA : lt_lifnr   TYPE /agri/t_glifnr,
         lt_fmacres TYPE /agri/t_fmacres.

*--Fetching vendor data for validation
  SELECT lifnr FROM lfa1
  INTO TABLE lt_lifnr
  FOR ALL ENTRIES IN gt_res_display
  WHERE lifnr = gt_res_display-lifnr.

*--Fetching Farm Management Resource data
  SELECT *
    FROM /agri/fmacres
  INTO TABLE lt_fmacres
  FOR ALL ENTRIES IN gt_res_display
  WHERE idresource = gt_res_display-idresource
    AND arbpl      = gt_res_display-arbpl.
  IF sy-subrc = 0.
    SORT lt_fmacres BY idresource arbpl.
  ENDIF.
  CLEAR gs_variables-error.

*--Processing resource data for validations
  LOOP AT gt_res_display ASSIGNING FIELD-SYMBOL(<fs_res_display>).
    IF gs_variables-error IS INITIAL.
      PERFORM validations USING    lt_lifnr
                                   lt_fmacres
                          CHANGING <fs_res_display>.
    ENDIF.
    IF r_rcrea IS NOT INITIAL.
      PERFORM styles_prepare CHANGING <fs_res_display>.
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
*--When Upload resources is active
    IF r_rupl = abap_true.
      REFRESH: s_arbpl, s_idrsc.
      IF screen-group1 = 'K1' OR screen-group1 = 'K2'.
        screen-active = '0'.
      ENDIF.
*--When maintain resources is active
    ELSEIF r_rmain = abap_true.
      CLEAR p_path.
      IF screen-group1 = 'P'.
        screen-active = '0'.
      ENDIF.
*--When Create resources is active
    ELSEIF r_rcrea = abap_true.
      IF screen-group1 IS NOT INITIAL.
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
  REFRESH: gt_res_display, gt_res_old.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_RESOURCE_DATA
*&---------------------------------------------------------------------*
*& GET_RESOURCE_DATA
*&---------------------------------------------------------------------*
FORM get_resource_data.

*--Fetching Farm Management Resource data
  SELECT idresource
         arbpl
         description
         rstype
         equnr
         intext
         lifnr
         hr
         pernr
         zabsausencia
    FROM /agri/fmacres
    INTO CORRESPONDING FIELDS OF TABLE gt_res_display
    WHERE idresource IN s_idrsc
      AND arbpl      IN s_arbpl.
  IF sy-subrc <> 0.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
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
*--Validating resource id and work center fields when display resources
*is active
    IF r_rmain IS NOT INITIAL.
      IF s_idrsc IS INITIAL AND s_arbpl IS INITIAL.
        MESSAGE e044(zabs_msgcls).
      ENDIF.
    ENDIF.
*--Validating file path when maintain resources is active
    IF r_rupl IS NOT INITIAL.
      IF p_path IS INITIAL.
        MESSAGE e048(zabs_msgcls).
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
  DATA : ls_fmacres       TYPE /agri/fmacres,
         ls_layout        TYPE slis_layout_alv,
         ls_fmacres_temp  TYPE /agri/fmacres,

*--Internal table declaration
         lt_tab_raw       TYPE truxs_t_text_data,
         lt_fmacres       TYPE STANDARD TABLE OF /agri/fmacres,
         lt_resource_main TYPE STANDARD TABLE OF
                            zabs_str_resxls_maintain,
         lt_update        TYPE STANDARD TABLE OF /agri/fmacres,
         lt_insert        TYPE STANDARD TABLE OF /agri/fmacres,
         lt_delete        TYPE STANDARD TABLE OF /agri/fmacres,
         lt_data          TYPE STANDARD TABLE OF alsmex_tabline,
         lt_fcat          TYPE STANDARD TABLE OF slis_t_fieldcat_alv,
         lr_table         TYPE REF TO cl_salv_table,
         lr_columns       TYPE REF TO cl_salv_columns_table,

*--Local variable
         lv_msg           TYPE REF TO cx_salv_msg,
         lv_errors        TYPE char1,
         lv_file          TYPE rlgrap-filename,
         lv_tabix         TYPE sy-tabix.

*--FM to convert excel data format to sap format
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = abap_true
      i_tab_raw_data       = lt_tab_raw
      i_filename           = p_path
    TABLES
      i_tab_converted_data = lt_resource_main
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.

    LEAVE LIST-PROCESSING.
    LEAVE TO SCREEN 0.

  ELSE.

*--Processing excel data to append zeros to the vendor
    LOOP AT lt_resource_main ASSIGNING FIELD-SYMBOL(<ls_res_main>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <ls_res_main>-lifnr
        IMPORTING
          output = <ls_res_main>-lifnr.
    ENDLOOP.

*--Fetching vendor data for validation
    SELECT lifnr FROM lfa1
    INTO TABLE @DATA(lt_lifnr)
    FOR ALL ENTRIES IN @lt_resource_main
    WHERE lifnr = @lt_resource_main-lifnr.

*--Fetch Farm Management Resource data
    SELECT * FROM /agri/fmacres
    INTO TABLE lt_fmacres
    FOR ALL ENTRIES IN lt_resource_main
    WHERE idresource = lt_resource_main-idresource
      AND arbpl      = lt_resource_main-arbpl.
    IF sy-subrc = 0.
      SORT lt_fmacres BY idresource arbpl.
    ENDIF.

*--Processing excel data with validations to fill the final table
    LOOP AT lt_resource_main ASSIGNING
    FIELD-SYMBOL(<fs_resource_main>).

      lv_tabix = sy-tabix + 1.
      LOOP AT lt_resource_main INTO DATA(ls_resource_main) FROM lv_tabix.
        IF <fs_resource_main>-idresource EQ ls_resource_main-idresource AND
           <fs_resource_main>-arbpl      EQ ls_resource_main-arbpl.
          <fs_resource_main>-msgtxt = TEXT-020.
          <fs_resource_main>-msgtyp =  zcl_abs_abap_maintain=>c_icon_warning. "'@09@' .
        ENDIF.
      ENDLOOP.

      IF <fs_resource_main>-msgtxt IS NOT INITIAL.
        CONTINUE.
      ENDIF.
*--Validating whether Resource ID is mandatory or not
      IF  <fs_resource_main>-idresource IS INITIAL.
        <fs_resource_main>-msgtxt = TEXT-002 .
        <fs_resource_main>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error. "'@0A@'.
        CONTINUE.
      ENDIF.

*--Validating whether Work Center is mandatory or not
      IF  <fs_resource_main>-arbpl IS INITIAL.
        <fs_resource_main>-msgtxt = TEXT-003.
        <fs_resource_main>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error. "'@0A@'.
        CONTINUE.
      ENDIF.

      IF  <fs_resource_main>-delete NE abap_true.

        <fs_resource_main>-msgtyp =
       zcl_abs_abap_maintain=>c_icon_error. "'@0A@'

*--Validating whether description is mandatory or not
        IF  <fs_resource_main>-description IS INITIAL.
          <fs_resource_main>-msgtxt = TEXT-004.
          CONTINUE.
        ENDIF.

**--Checking whether Equipment is given for Resource Type B
*        IF <fs_resource_main>-rstype =
*        zcl_abs_abap_maintain=>c_rstyp_equip AND
*        <fs_resource_main>-equnr IS INITIAL.
*          <fs_resource_main>-msgtxt = TEXT-005.
*          CONTINUE.
*        ENDIF.

        IF <fs_resource_main>-rstype NE
                zcl_abs_abap_maintain=>c_rstyp_equip.
*--Validating the External and Hr fields
*--Displaying error message when both External and Hr fiels are active
          IF <fs_resource_main>-intext = abap_true AND
          <fs_resource_main>-hr =
          abap_true.
            <fs_resource_main>-msgtxt = TEXT-006.
            CONTINUE.

*--Displaying error message when external field is checked and vendor is
*empty and Personnel Number is given
          ELSEIF <fs_resource_main>-intext = abap_true.
            IF <fs_resource_main>-lifnr IS INITIAL.
              <fs_resource_main>-msgtxt = TEXT-007.
              CONTINUE.
            ELSEIF <fs_resource_main>-pernr IS NOT INITIAL.
              <fs_resource_main>-msgtxt = TEXT-008.
              CONTINUE.
            ENDIF.

*--Displaying error message when Hr field is checked and vendor is not
*given and Personnel Number is empty
          ELSEIF <fs_resource_main>-hr = abap_true.
            IF <fs_resource_main>-pernr IS INITIAL.
              <fs_resource_main>-msgtxt = TEXT-009.
              CONTINUE.
            ELSEIF <fs_resource_main>-lifnr IS NOT INITIAL.
              <fs_resource_main>-msgtxt = TEXT-010.
              CONTINUE.
            ENDIF.

*--Displaying error message when both External and Hr fields are empty
          ELSEIF <fs_resource_main>-intext IS INITIAL AND
          <fs_resource_main>-hr IS
          INITIAL.
            <fs_resource_main>-msgtxt = TEXT-011.
            CONTINUE.
          ENDIF.
        ENDIF.

*--Validating Vendor field
        IF <fs_resource_main>-lifnr IS NOT INITIAL.
          READ TABLE lt_lifnr INTO DATA(ls_lifnr)
                              WITH KEY lifnr =
                              <fs_resource_main>-lifnr.
          IF sy-subrc NE 0.
            <fs_resource_main>-msgtxt = TEXT-017.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF <fs_resource_main>-msgtxt IS INITIAL.
          CLEAR <fs_resource_main>-msgtyp.
        ENDIF.
      ENDIF.

*--Inserting, Updating, deleting records from Farm management Resource
*table
      IF <fs_resource_main>-msgtyp IS INITIAL.
        IF  <fs_resource_main>-delete EQ abap_true.
*--Deleting records from standard table
          CLEAR ls_fmacres.
          READ TABLE lt_fmacres INTO ls_fmacres
                            WITH KEY idresource =
                                <fs_resource_main>-idresource
                                      arbpl     =
                                      <fs_resource_main>-arbpl
                            BINARY SEARCH.
          IF sy-subrc = 0.
            APPEND ls_fmacres TO lt_delete.
            <fs_resource_main>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_success. "'@08@'
            <fs_resource_main>-msgtxt = TEXT-012.
          ELSE.
            <fs_resource_main>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
            <fs_resource_main>-msgtxt = TEXT-013.
          ENDIF.
        ELSE.
*--Updating recprds into standard table
          CLEAR ls_fmacres.
          READ TABLE lt_fmacres INTO ls_fmacres
                                      WITH KEY idresource =
                                      <fs_resource_main>-idresource
                                               arbpl      =
                                      <fs_resource_main>-arbpl
                                      BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR ls_fmacres_temp.
            MOVE-CORRESPONDING <fs_resource_main> TO ls_fmacres_temp.
            ls_fmacres_temp-mandt = sy-mandt.
            IF ls_fmacres_temp <> ls_fmacres.
              MOVE-CORRESPONDING <fs_resource_main> TO ls_fmacres.
              APPEND ls_fmacres TO lt_update.
              <fs_resource_main>-msgtyp =
              zcl_abs_abap_maintain=>c_icon_success. "'@08@'
              <fs_resource_main>-msgtxt = TEXT-014.
            ELSE.
              <fs_resource_main>-msgtyp =
              zcl_abs_abap_maintain=>c_icon_warning. "'@09@'
              <fs_resource_main>-msgtxt = TEXT-015.
            ENDIF.
          ELSE.
*--Inserting records into standard table
            MOVE-CORRESPONDING <fs_resource_main> TO ls_fmacres.
            APPEND ls_fmacres TO lt_insert.
            CLEAR ls_fmacres.
            <fs_resource_main>-msgtyp =
            zcl_abs_abap_maintain=>c_icon_success. "'@08@'
            <fs_resource_main>-msgtxt = TEXT-016.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*--Deleting record from Farm management resource table
    IF lt_delete IS NOT INITIAL.
      DELETE /agri/fmacres FROM TABLE lt_delete.
    ENDIF.

*--Inserting record from Farm management resource table
    IF lt_insert IS NOT INITIAL.
      INSERT /agri/fmacres FROM TABLE lt_insert.
    ENDIF.

*--Updating record from Farm management resource table
    IF lt_update IS NOT INITIAL.
      UPDATE /agri/fmacres FROM TABLE lt_update.
    ENDIF.

    COMMIT WORK.

  ENDIF.

*--Displaying data
  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = lt_resource_main ).
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
 AND r_rcrea IS INITIAL.
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
        lt_fcat   TYPE lvc_t_fcat,

*--Local variable declaration
        lv_input  TYPE int4.

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

        gt_res_old = gt_res_display.
*--Display data on Grid
        CALL METHOD gobj_grid->set_table_for_first_display
          EXPORTING
            i_structure_name     = zcl_abs_abap_maintain=>c_str_res_display
            i_save               = zcl_abs_abap_maintain=>c_variant_save "'A'
            i_default            = abap_true
            is_layout            = ls_layout
            it_toolbar_excluding = i_exclude
          CHANGING
            it_outtab            = gt_res_display "gt_final
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

  IF NOT gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display. "'A'
    lv_input = 1.
  ELSEIF r_rcrea IS NOT INITIAL.
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
        lv_valid,

*--Internal table declaration
        lt_imod      TYPE TABLE OF ty_mod.

*--Field-symbol declaration
  FIELD-SYMBOLS: <ft_append_row> TYPE ANY TABLE.

*--When Create resource is active
  IF r_rcrea IS NOT INITIAL.
    IF p_er_data_changed->mt_inserted_rows IS NOT INITIAL.
      ASSIGN p_er_data_changed->mp_mod_rows->* TO <ft_append_row>.
      IF <ft_append_row> IS ASSIGNED.
        APPEND LINES OF <ft_append_row> TO gt_res_display.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT p_er_data_changed->mt_good_cells INTO lwa_mod_cell.
    lwa_mod-row  = lwa_mod_cell-row_id.
    APPEND lwa_mod TO lt_imod.
  ENDLOOP.

  gs_variables-changes = abap_true.

*&--Refreshing the ALV.
  CALL METHOD gobj_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module TRANSACTION_INIT OUTPUT
*&---------------------------------------------------------------------*
*& TRANSACTION_INIT
*&---------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
  PERFORM transaction_init.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TRANSACTION_INIT
*&---------------------------------------------------------------------*
*& TRANSACTION_INIT
*&---------------------------------------------------------------------*
FORM transaction_init.
  IF gs_variables-mode IS INITIAL.
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_display. "'A'
  ENDIF.
ENDFORM.                    " TRANSACTION_INIT

*&---------------------------------------------------------------------*
*& Form DISPLAY_RESOURCE_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_RESOURCE_DATA
*&---------------------------------------------------------------------*
FORM display_resource_data.
*--Calling screen to display resource maintain data
  IF r_rmain IS NOT INITIAL OR r_rcrea IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_tb_functions  CHANGING pt_exclude TYPE ui_functions.

*--Workarea Declarations
  DATA: lwa_exclude TYPE ui_func.

  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND lwa_exclude TO pt_exclude.
  lwa_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND lwa_exclude TO pt_exclude.
*--When Create resource is empty
  IF r_rcrea IS INITIAL.
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
      i_structure_name = zcl_abs_abap_maintain=>c_str_res_display
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
      WHEN zcl_abs_abap_maintain=>c_fieldname_idres "'IDRESOURCE'
        OR zcl_abs_abap_maintain=>c_fieldname_wc.   "'ARBPL'
        IF r_rmain IS NOT INITIAL.
          <fs_fcat>-edit = space.
        ENDIF.
      WHEN zcl_abs_abap_maintain=>c_fieldname_intext "'INTEXT'
        OR zcl_abs_abap_maintain=>c_fieldname_hr    "'HR'
        OR 'ZABSAUSENCIA'.
        <fs_fcat>-checkbox = abap_true.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form LAYOUR_PREPARE
*&---------------------------------------------------------------------*
*& LAYOUR_PREPARE
*&---------------------------------------------------------------------*
FORM layout_prepare  CHANGING cs_layout TYPE lvc_s_layo.

*--Preparing Layout
  cs_layout-cwidth_opt = abap_true.
  cs_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
  cs_layout-stylefname = zcl_abs_abap_maintain=>c_layout_stylefname."'STYLE'

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
FORM styles_prepare  CHANGING ls_res_display TYPE zabs_str_resource_maintain.

*--Workarae declaration
  DATA: lwa_style  TYPE lvc_s_styl.

*--Disabling Resource ID field
  IF ls_res_display-idresource IS NOT INITIAL.
    lwa_style-fieldname = zcl_abs_abap_maintain=>c_fieldname_idres. "'IDRESOURCE'
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE ls_res_display-style.
    CLEAR lwa_style.
  ENDIF.

*--Disabling Work Center field
  IF ls_res_display-arbpl IS NOT INITIAL.
    lwa_style-fieldname = zcl_abs_abap_maintain=>c_fieldname_wc. "'ARBPL'
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE ls_res_display-style.
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

*--Local variable declaration
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

*--Local variable declaration
  DATA: lv_ans(1),
        lv_valid,
        lv_error TYPE xfeld.

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

  CHECK lv_ans CO zcl_abs_abap_maintain=>c_numbers. "'123456789'
  CASE lv_ans.
    WHEN 1.
      PERFORM data_save CHANGING lv_error.
    WHEN 2.
      LEAVE TO SCREEN 0.
    WHEN 9.
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

*--Local variable declaration
  DATA: lv_ans(1),
        lv_valid,
        lv_error TYPE xfeld.

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

  CHECK lv_ans CO zcl_abs_abap_maintain=>c_numbers. "'123456789'
  CASE lv_ans.
    WHEN 1.
      PERFORM data_save CHANGING lv_error.
    WHEN 2.
      LEAVE TO SCREEN 0.
    WHEN 9.
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
*--When create resource is active
      IF r_rcrea IS NOT INITIAL.
        LOOP AT gt_res_display ASSIGNING FIELD-SYMBOL(<fs_res_display>).
          PERFORM styles_prepare CHANGING <fs_res_display>.
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
                                 TEXT-019  "<- Do you want save the changes?
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

*--Calling FM to get Popup
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
  DATA: lt_delete   TYPE STANDARD TABLE OF /agri/fmacres,
        lt_insert   TYPE STANDARD TABLE OF /agri/fmacres,
        lt_update   TYPE STANDARD TABLE OF /agri/fmacres,
        lt_msgtab   TYPE  esp1_message_tab_type,

*--Workarea declaration
        ls_res_data TYPE /agri/fmacres,
        ls_msgtab   TYPE esp1_message_wa_type.

  CHECK gs_variables-error IS INITIAL.
  IF r_rmain IS NOT INITIAL.

*--Processing the data to delete and update the data
    LOOP AT gt_res_old INTO DATA(ls_res_old).

*--Updating records into standard table
      READ TABLE gt_res_display INTO DATA(ls_res_display)
        WITH KEY idresource = ls_res_old-idresource
                 arbpl      = ls_res_old-arbpl.
      IF sy-subrc = 0.
        IF ls_res_old NE ls_res_display.
          MOVE-CORRESPONDING ls_res_display TO ls_res_data.
          APPEND ls_res_data TO lt_update.
          ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
          ls_msgtab-msgno = '064'.
          ls_msgtab-msgty  = zcl_abs_abap_maintain=>c_msgty_success. "'S'
          ls_msgtab-msgv1  = ls_res_data-idresource.
          ls_msgtab-msgv2  = ls_res_data-arbpl.
          APPEND ls_msgtab TO lt_msgtab.
          CLEAR : ls_res_data, ls_msgtab.
        ENDIF.
      ELSE.
*--Deleting records from standard table
        MOVE-CORRESPONDING ls_res_old TO ls_res_data.
        APPEND ls_res_data TO lt_delete.
        ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
        ls_msgtab-msgno = '065'.
        ls_msgtab-msgty  = zcl_abs_abap_maintain=>c_msgty_success. "'S'
        ls_msgtab-msgv1  = ls_res_data-idresource.
        ls_msgtab-msgv2  = ls_res_data-arbpl.
        APPEND ls_msgtab TO lt_msgtab.
        CLEAR : ls_res_data, ls_msgtab.
      ENDIF.

    ENDLOOP.

*--delete the data
    IF NOT lt_delete IS INITIAL.
      DELETE /agri/fmacres FROM TABLE lt_delete.
    ENDIF.

*--update the data
    IF NOT lt_update IS INITIAL.
      UPDATE /agri/fmacres FROM TABLE lt_update.
    ENDIF.

  ELSEIF r_rcrea IS NOT INITIAL.
    MOVE-CORRESPONDING gt_res_display TO lt_insert.
    REFRESH gt_res_display.
*--insert the data
    IF NOT lt_insert IS INITIAL.
      INSERT /agri/fmacres FROM TABLE lt_insert.
      LOOP AT lt_insert INTO DATA(lwa_insert).
        ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
        ls_msgtab-msgno = '066'.
        ls_msgtab-msgty  = zcl_abs_abap_maintain=>c_msgty_success. "'S'
        ls_msgtab-msgv1  = lwa_insert-idresource.
        ls_msgtab-msgv2  = lwa_insert-arbpl.
        APPEND ls_msgtab TO lt_msgtab.
        CLEAR ls_msgtab.
      ENDLOOP.
    ENDIF.

  ENDIF.

*--Commit
  IF NOT lt_update IS INITIAL OR
     NOT lt_delete IS INITIAL OR
     NOT lt_insert IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.
  gt_res_old = gt_res_display.

*--Message display sub-routine
  PERFORM messages_display USING lt_msgtab.

ENDFORM.                    " DATA_SAVE

*&---------------------------------------------------------------------*
*& Form  FCODE_CHANGE
*&---------------------------------------------------------------------*
*& FCODE_CHANGE
*----------------------------------------------------------------------*
FORM fcode_change.

  IF gs_variables-mode EQ zcl_abs_abap_maintain=>c_mode_display. "'A' "<-Display
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_change.    "'V' "<-Changes
  ELSE.
    gs_variables-mode = zcl_abs_abap_maintain=>c_mode_display. "'A'
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATIONS
*&---------------------------------------------------------------------*
*& VALIDATIONS
*&---------------------------------------------------------------------*
FORM validations USING    pt_lifnr       TYPE /agri/t_glifnr
                          lt_fmacres     TYPE /agri/t_fmacres
                 CHANGING ls_res_display TYPE zabs_str_resource_maintain.

*--When Create resource is active
  IF r_rcrea IS NOT INITIAL.
    READ TABLE lt_fmacres INTO DATA(ls_fmacres)
                         WITH KEY idresource = ls_res_display-idresource
                                  arbpl      = ls_res_display-arbpl BINARY SEARCH.
    IF sy-subrc = 0.
      MESSAGE i068(zabs_msgcls) WITH ls_fmacres-idresource
                                     ls_fmacres-arbpl
     DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      gs_variables-error = abap_true.
      RETURN.
    ENDIF.
  ENDIF.

*--Validating whether Resource ID is mandatory or not
  IF  ls_res_display-idresource IS INITIAL.
    MESSAGE i052(zabs_msgcls) WITH ls_res_display-arbpl
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

*--Validating whether Work Center is mandatory or not
  IF  ls_res_display-arbpl IS INITIAL.
    MESSAGE i053(zabs_msgcls) WITH ls_res_display-idresource
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

*--Validating whether description is mandatory or not
  IF  ls_res_display-description IS INITIAL.
    MESSAGE i054(zabs_msgcls) WITH ls_res_display-idresource
                                   ls_res_display-arbpl
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
    gs_variables-error = abap_true.
    RETURN.
  ENDIF.

**--Checking whether Equipment is given for Resource Type B
*  IF ls_res_display-rstype =
*  zcl_abs_abap_maintain=>c_rstyp_equip AND
*  ls_res_display-equnr IS INITIAL.
*    MESSAGE i055(zabs_msgcls) WITH ls_res_display-idresource
*                                   ls_res_display-arbpl
*    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
*    gs_variables-error = abap_true.
*    RETURN.
*  ENDIF.

*--Validating the External and Hr fields
*--Displaying error message when both External and Hr fiels are active
  IF ls_res_display-rstype NE
    zcl_abs_abap_maintain=>c_rstyp_equip.

    IF ls_res_display-intext = abap_true AND
    ls_res_display-hr =
    abap_true.
      MESSAGE i056(zabs_msgcls) WITH ls_res_display-idresource
                                     ls_res_display-arbpl
      DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      gs_variables-error = abap_true.
      RETURN.

*--Displaying error message when external field is checked and vendor is
*empty and Personnel Number is given
    ELSEIF ls_res_display-intext = abap_true.
      IF ls_res_display-lifnr IS INITIAL.
        MESSAGE i057(zabs_msgcls) WITH ls_res_display-idresource
                                       ls_res_display-arbpl
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
        gs_variables-error = abap_true.
        RETURN.
      ELSEIF ls_res_display-pernr IS NOT INITIAL.
        MESSAGE i057(zabs_msgcls) WITH ls_res_display-idresource
                                       ls_res_display-arbpl
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
        gs_variables-error = abap_true.
        RETURN.
      ENDIF.

*--Displaying error message when Hr field is checked and vendor is not
*given and Personnel Number is empty
    ELSEIF ls_res_display-hr = abap_true.
      IF ls_res_display-pernr IS INITIAL.
        MESSAGE i058(zabs_msgcls) WITH ls_res_display-idresource
                                       ls_res_display-arbpl
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
        gs_variables-error = abap_true.
        RETURN.
      ELSEIF ls_res_display-lifnr IS NOT INITIAL.
        MESSAGE i058(zabs_msgcls) WITH ls_res_display-idresource
                                       ls_res_display-arbpl
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
        gs_variables-error = abap_true.
        RETURN.
      ENDIF.

*--Displaying error message when both External and Hr fields are empty
    ELSEIF ls_res_display-intext IS INITIAL AND
    ls_res_display-hr IS
    INITIAL.
      MESSAGE i056(zabs_msgcls) WITH ls_res_display-idresource
                                     ls_res_display-arbpl
      DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      gs_variables-error = abap_true.
      RETURN.
    ENDIF.
  ENDIF.

*--Validating Vendor field
  IF ls_res_display-lifnr IS NOT INITIAL.
    READ TABLE pt_lifnr INTO DATA(ls_lifnr)
                        WITH KEY lifnr =
                        ls_res_display-lifnr.
    IF sy-subrc NE 0.
      MESSAGE e059(zabs_msgcls) WITH ls_res_display-idresource
                                     ls_res_display-arbpl.
      gs_variables-error = abap_true.
    ENDIF.
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
