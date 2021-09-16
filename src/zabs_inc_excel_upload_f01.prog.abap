*----------------------------------------------------------------------*
***INCLUDE ZABS_INC_EXCEL_UPLOAD_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FILE_PATH
*&---------------------------------------------------------------------*
FORM validate_file_path USING p_file.

  DATA: lv_dir       TYPE string,
        lv_file      TYPE string,
        lv_result(1) TYPE c,
        lv_filename  TYPE string.

  lv_filename = p_file.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = lv_file "file name
      file_path     = lv_dir "directory path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-- check existence of directory
  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = lv_dir
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

*-- lv_result is X if valid directory
  IF lv_result IS INITIAL.
*-- Diretório inválido!
    MESSAGE e151(zfmfp).
  ENDIF.

  CLEAR lv_result.

*-- Check file existence
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_filename
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

*-- lv_result is X if valid file path
  IF lv_result IS INITIAL.
*-- Arquivo inválido!
    MESSAGE e152(zfmfp).
  ENDIF.

ENDFORM.                    " VALIDATE_FILE_PATH

*&---------------------------------------------------------------------*
*& Form PREPARE_TO_IMPORT_EXCEL
*&---------------------------------------------------------------------*
FORM prepare_to_import_excel .

  CLASS c_oi_errors DEFINITION LOAD.

*-- Create Instance control for container
  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = o_control
      error   = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

*-- Create generic container linked to container in screen 100
  CREATE OBJECT obj_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE e208(00) WITH 'Error creating container'.       "#EC NOTEXT
  ENDIF.

*-- Establish connection to GUI Control
  CALL METHOD o_control->init_control
    EXPORTING
      r3_application_name = 'Excel Document Container'      "#EC NOTEXT
      inplace_enabled     = 'X'
      parent              = obj_container
    IMPORTING
      error               = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

*-- Create Document Proxy
  CALL METHOD o_control->get_document_proxy
    EXPORTING
      document_type  = soi_doctype_excel_sheet
    IMPORTING
      document_proxy = o_document
      error          = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM read_excel_file .

  CONCATENATE 'FILE://' p_file INTO v_doc_name.

*-- Open Spreadsheet in SAPWORKDIR
  CALL METHOD o_document->open_document
    EXPORTING
      open_inplace   = 'X'
      document_title = 'Excel'                              "#EC NOTEXT
      document_url   = v_doc_name
      no_flush       = ''
    IMPORTING
      error          = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

*-- Open Spreadsheet interface
  CALL METHOD o_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ''
    IMPORTING
      sheet_interface = o_spreadsheet
      error           = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

*-- Set selection
  CALL METHOD o_spreadsheet->set_selection
    EXPORTING
      top     = 1
      left    = 1
      rows    = v_rows
      columns = v_columns.

*-- Define Range in spreadsheet
  CALL METHOD o_spreadsheet->insert_range
    EXPORTING
      name     = 'Test'                                     "#EC NOTEXT
      rows     = v_rows
      columns  = v_columns
      no_flush = ''
    IMPORTING
      error    = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  s_ranges-name    = 'Test'.                                "#EC NOTEXT
  s_ranges-rows    = v_rows.
  s_ranges-columns = v_columns.
  APPEND s_ranges TO t_ranges.

*-- Get data
  CALL METHOD o_spreadsheet->get_ranges_data
    EXPORTING
      all      = ''
      no_flush = ''
    IMPORTING
      contents = t_data
      error    = o_error
    CHANGING
      ranges   = t_ranges.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  DELETE t_data WHERE value IS INITIAL.

*-- Convert xls cells to itab
  LOOP AT t_data ASSIGNING FIELD-SYMBOL(<s_data>).
    s_sheets_out-sheet = 1.
    MOVE-CORRESPONDING <s_data> TO s_sheets_out.
    APPEND s_sheets_out TO t_sheets_out.
  ENDLOOP.

  CALL METHOD o_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ' '
    IMPORTING
      error           = o_error
      sheet_interface = o_spreadsheet.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  CALL METHOD o_spreadsheet->get_sheets
    EXPORTING
      no_flush = ' '
    IMPORTING
      sheets   = t_sheets
      error    = o_error.

  IF o_error->has_failed = 'X'.
    CALL METHOD o_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  IF lines( t_sheets ) GT 1.
    LOOP AT t_sheets ASSIGNING FIELD-SYMBOL(<s_sheet>).
      DATA(v_tabix) = sy-tabix.

      IF v_tabix EQ 1.
        CONTINUE.
      ENDIF.

      CALL METHOD o_spreadsheet->select_sheet
        EXPORTING
          name  = <s_sheet>-sheet_name
        IMPORTING
          error = o_error.

      IF o_error->has_failed = 'X'.
        EXIT.
      ENDIF.

      CALL METHOD o_spreadsheet->set_selection
        EXPORTING
          top     = 1
          left    = 1
          rows    = v_rows
          columns = v_columns.

      CALL METHOD o_spreadsheet->insert_range
        EXPORTING
          name     = 'Test'(003)
          rows     = v_rows
          columns  = v_columns
          no_flush = ''
        IMPORTING
          error    = o_error.

      IF o_error->has_failed = 'X'.
        EXIT.
      ENDIF.

      REFRESH: t_data.
      CALL METHOD o_spreadsheet->get_ranges_data
        EXPORTING
          all      = ''
        IMPORTING
          contents = t_data
          error    = o_error
        CHANGING
          ranges   = t_ranges.

*-- Remove ranges not to be processed else the data keeps on adding up
      CALL METHOD o_spreadsheet->delete_ranges
        EXPORTING
          ranges = t_ranges.

      DELETE t_data WHERE value IS INITIAL.

*-- Convert xls cells to itab
      LOOP AT t_data ASSIGNING <s_data>.
        s_sheets_out-sheet = v_tabix.
        MOVE-CORRESPONDING <s_data> TO s_sheets_out.
        APPEND s_sheets_out TO t_sheets_out.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

*-- Final clean-up
  FREE o_spreadsheet.
*-- Close the document
  CALL METHOD o_document->close_document.
*-- Clear Document Resources
  CALL METHOD o_document->release_document.
  FREE o_document.
  CALL METHOD o_control->release_all_documents.
  CALL METHOD o_control->destroy_control.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM authorization_check .

  config_tcode_authority_check '/AGRI/GLUPMD21' '01' v_subrc.
  IF v_subrc <> 0.
    MESSAGE ID '/AGRI/FMUPM' TYPE 'E' NUMBER '010' INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUDGET_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_SHEETS_OUT
*&---------------------------------------------------------------------*
FORM budget_update USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: lcl_upload TYPE REF TO zcl_abs_glupload_budget.
  CREATE OBJECT lcl_upload.
  lcl_upload->agriplan_budget_update( EXPORTING it_table    = lt_sheet
                                      IMPORTING et_messages = gt_message ).

ENDFORM.                    " BUDGET_UPDATE

*&---------------------------------------------------------------------*
*& Form SCREEN_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_update .

  IF sy-uname EQ 'SYS_VISTEX'
  OR sy-uname EQ 'T30592165'
  OR sy-uname EQ 'T_H.KABABE'.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_BUD'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZABS_INC_EXCEL_UPLOAD_F01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_SHEETS_OUT
*&---------------------------------------------------------------------*
FORM zabs_crop_season_create USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: lcl_upload TYPE REF TO zcl_abs_glupload_crop_season.
  CREATE OBJECT lcl_upload.
  lcl_upload->crop_season_create( EXPORTING it_table    = lt_sheet
                                  IMPORTING et_messages = gt_message ).

ENDFORM.
