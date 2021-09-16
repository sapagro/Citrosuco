*&---------------------------------------------------------------------*
*& Report  /AGRI/FMDFP_PROCESS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfmfp_taskorder_notes.

INCLUDE: /agri/global_constants,
         /agri/global_macros,
         /agri/gprolog_macros.

TABLES: /agri/fmfphdr,
        /agri/fmfpitm,
        /agri/fmfpcom,
        /agri/glflot.

DATA: gt_fphdr   TYPE /agri/t_fmfphdr_wl,
      gt_fmfpitm TYPE STANDARD TABLE OF /agri/fmfpitm INITIAL SIZE 0,
      lwa_fphdr  TYPE /agri/s_fmfphdr_wl,
      lv_subrc   TYPE sy-subrc.

****Program
CONSTANTS: c_program_fp_process TYPE sy-repid VALUE '/AGRI/FMFP_PROCESS'.

PARAMETERS: p_excall TYPE c NO-DISPLAY,
            p_nusry  TYPE c NO-DISPLAY,
            p_repid  LIKE sy-repid NO-DISPLAY.

*** Background Job
PARAMETERS: p_jbcnt TYPE tbtcjob-jobcount NO-DISPLAY,
            p_jobnr TYPE guid_16 NO-DISPLAY.

*** Report Constants
CONSTANTS: BEGIN OF c_slnam,
             external_call TYPE fieldname VALUE 'P_EXCALL',
             calling_rep   TYPE fieldname VALUE 'P_REPID',
           END OF c_slnam.

DATA: gt_selection   TYPE /agri/t_bsel_values,
      gv_varid       TYPE raldb_vari,
      gv_skip_screen.

DATA: r_table      TYPE REF TO cl_salv_table,
      r_selections TYPE REF TO cl_salv_selections,
      r_events     TYPE REF TO cl_salv_events_table.

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       Define eventos para relatório
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

ENDCLASS. "lcl_handle_events DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_note       TYPE STANDARD TABLE OF txw_note INITIAL SIZE 0,
          t_rows        TYPE salv_t_row,
          wa_row        TYPE i,
          v_rows        TYPE i,
          lv_note       TYPE zabs_note,
          message       TYPE string.

    CASE e_salv_function.
      WHEN 'NOTA'.
        lr_selections = r_table->get_selections( ).
        t_rows = lr_selections->get_selected_rows( ).
        v_rows = lines( t_rows ).
        IF v_rows IS INITIAL.
          "Nenhuma Ordem selecionada!
          MESSAGE ID 'ZFMFP'
            TYPE c_msg_type-info NUMBER '004'.
        ELSEIF v_rows EQ 1.
          READ TABLE t_rows INTO wa_row INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_fmfpitm INTO DATA(lwa_fmfpitm) INDEX wa_row.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
                EXPORTING
                  edit_mode = 'X'
                TABLES
                  t_txwnote = lt_note.
              LOOP AT lt_note INTO DATA(lwa_note).
                IF sy-tabix EQ 1.
                  lv_note = lwa_note.
                ELSE.
                  CONCATENATE lwa_note space INTO lv_note.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSEIF v_rows GT 1.
          "Selecione somente uma Ordem!
          MESSAGE ID 'ZFMFP'
            TYPE c_msg_type-info NUMBER '005'.
        ENDIF.
    ENDCASE.

  ENDMETHOD. "on_user_command

ENDCLASS. "lcl_handle_events IMPLEMENTATION

DATA: r_functions   TYPE REF TO cl_salv_functions,
      r_display     TYPE REF TO cl_salv_display_settings,
      r_columns     TYPE REF TO cl_salv_columns_table,
      r_column      TYPE REF TO cl_salv_column_table,
      r_sorts       TYPE REF TO cl_salv_sorts,
      event_handler TYPE REF TO lcl_handle_events,
      color         TYPE lvc_s_colo.

AT SELECTION-SCREEN OUTPUT.
  IF p_nusry IS NOT INITIAL.
    SET TITLEBAR 'T100'.
  ENDIF.

INITIALIZATION.
  p_class = '1'.

*SELECTION-SCREEN BEGIN OF BLOCK plan WITH FRAME TITLE TEXT-t01.
*PARAMETERS: planej  AS CHECKBOX,
*            nplanej AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK plan.

START-OF-SELECTION.

GET /agri/fmfphdr.

  MOVE-CORRESPONDING /agri/fmfphdr TO lwa_fphdr.
  APPEND lwa_fphdr TO gt_fphdr.

END-OF-SELECTION.

*  IF p_excall IS INITIAL.
  IF gt_fphdr IS NOT INITIAL.
    PERFORM data_display.
  ELSE.
    MESSAGE s751(/agri/global).
  ENDIF.
*  ELSE.
*
*    CALL FUNCTION '/AGRI/FMFP_SEARCH_RESULTS_GET'
*      CHANGING
*        ct_fphdr = gt_fphdr.
*
*    LEAVE PROGRAM.
*  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_display .

  IF gt_fphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpitm
      INTO TABLE gt_fmfpitm
      FOR ALL ENTRIES IN gt_fphdr
     WHERE aufnr = gt_fphdr-aufnr.
  ENDIF.

  IF gt_fmfpitm[] IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = r_table
          CHANGING
            t_table      = gt_fmfpitm ).
      CATCH cx_salv_msg.
    ENDTRY.

    r_table->set_screen_status(
              pfstatus      = 'ZSALV_TABLE_STANDARD'
              report        = sy-repid
              set_functions = r_table->c_functions_all ).

* Set up selections.
    r_events = r_table->get_event( ).
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->on_user_command FOR r_events.
    r_selections = r_table->get_selections( ).
    r_selections->set_selection_mode( 1 ). "Single

*    r_display = r_table->get_display_settings( ).
*    r_display->set_striped_pattern( cl_salv_display_settings=>true ).
*    r_columns = r_table->get_columns( ).
*    r_sorts = r_table->get_sorts( ).

* Display
    r_table->display( ).
  ENDIF.

ENDFORM.                    " DATA_DISPLAY
