**&---------------------------------------------------------------------*
**& Report ZFMFP_TASK_MONITOR_REPROC
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zfmfp_task_monitor_reproc.

TABLES: zfmfp_task_sheet.

TYPES: BEGIN OF ts_zfmfp_task_sheet,
         zcount    TYPE zfmfp_task_sheet-zcount,
         zrow      TYPE zfmfp_task_sheet-zrow,
         zcolumn   TYPE zfmfp_task_sheet-zcolumn,
         talhao    TYPE char15,
         ernam     TYPE zfmfp_task_sheet-ernam,
         erdat     TYPE zfmfp_task_sheet-erdat,
         erzet     TYPE zfmfp_task_sheet-erzet,
         aenam     TYPE zfmfp_task_sheet-aenam,
         aedat     TYPE zfmfp_task_sheet-aedat,
         aezet     TYPE zfmfp_task_sheet-aezet,
         processed TYPE zfmfp_task_sheet-processed,
         value_len TYPE zfmfp_task_sheet-value_len,
         value     TYPE zfmfp_task_sheet-value,
         message   TYPE zfmfp_task_sheet-message,
         xmes      TYPE string,
       END OF ts_zfmfp_task_sheet.

DATA: gt_task_sheet TYPE TABLE OF ts_zfmfp_task_sheet.


*-- cl_salv_table data
DATA: gr_table      TYPE REF TO cl_salv_table.
DATA: gr_columns    TYPE REF TO cl_salv_columns_table.
DATA: gr_column     TYPE REF TO cl_salv_column_table.
DATA: gr_events     TYPE REF TO cl_salv_events_table.
DATA: gr_selections TYPE REF TO cl_salv_selections.

DATA:gt_rows        TYPE salv_t_row.
DATA:wa_rows        TYPE int4.
DATA: v_nb          TYPE i.
DATA: v_nb_lines(3) TYPE c.
DATA: v_message     TYPE string.

*-----------------
* class definition
*

*-----------------------------------
* class lcl_handle_events
*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_click FOR EVENT added_function OF cl_salv_events.
ENDCLASS.                    "lcl_handle_events DEFINITION

DATA: event_handler TYPE REF TO lcl_handle_events.

*----- selection-screen
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  s_ernam     FOR zfmfp_task_sheet-ernam,
                 s_erdat     FOR zfmfp_task_sheet-erdat,
                 s_zcount    FOR zfmfp_task_sheet-zcount,
                 s_erzet     FOR zfmfp_task_sheet-erzet,
                 s_aenam     FOR zfmfp_task_sheet-aenam,
                 s_aedat     FOR zfmfp_task_sheet-aedat,
                 s_aezet     FOR zfmfp_task_sheet-aezet,
                 s_proc      FOR zfmfp_task_sheet-processed.
PARAMETERS: p_det AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK blk1.


*---- start-of-selection
START-OF-SELECTION.

*-- read data into internal table
  PERFORM get_data.

  "CHECK gt_task_sheet[] IS NOT INITIAL.

*-- display the table itab with CL_SALV_TABLE

  PERFORM display_output.


END-OF-SELECTION.
*&---------------------------------------------------------------------

*&      Form  GET_DATA
*&---------------------------------------------------------------------

*       text
*----------------------------------------------------------------------

*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------

FORM get_data.

  SELECT
          zcount
          zrow
          zcolumn
          ernam
          erdat
          erzet
          aenam
          aedat
          aezet
          processed
          message
          value_len
          value
    FROM zfmfp_task_sheet
    INTO CORRESPONDING FIELDS OF TABLE gt_task_sheet
    WHERE ernam  IN s_ernam  AND
          erdat  IN s_erdat  AND
          zcount IN s_zcount AND
          erzet  IN s_erzet  AND
          aenam  IN s_aenam  AND
          aedat  IN s_aedat  AND
          aezet  IN s_aezet  AND
          processed IN s_proc.

  SORT gt_task_sheet BY ernam erdat  zcount zrow zcolumn.

    DATA(lt_task_sheet) = gt_task_sheet.

  IF p_det IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM gt_task_sheet COMPARING ernam erdat zrow.

    LOOP AT lt_task_sheet INTO DATA(wa_task_sheet).
      READ TABLE gt_task_sheet ASSIGNING FIELD-SYMBOL(<fwa_task_sheet>)
        WITH KEY ernam = wa_task_sheet-ernam
                 erdat = wa_task_sheet-erdat
                 zrow = wa_task_sheet-zrow.
      IF sy-subrc = 0 AND wa_task_sheet-message IS NOT INITIAL.
        <fwa_task_sheet>-xmes = <fwa_task_sheet>-xmes && wa_task_sheet-message.
      ENDIF.
    ENDLOOP.

  ENDIF.

  LOOP AT gt_task_sheet ASSIGNING <fwa_task_sheet>.
    READ TABLE lt_task_sheet INTO wa_task_sheet
      WITH KEY ernam = <fwa_task_sheet>-ernam
               erdat = <fwa_task_sheet>-erdat
               zrow = <fwa_task_sheet>-zrow
               zcolumn = '5'.
    IF sy-subrc = 0 AND wa_task_sheet-value IS NOT INITIAL.
      SHIFT wa_task_sheet-value LEFT DELETING LEADING ' '.
      <fwa_task_sheet>-talhao = wa_task_sheet-value.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------

*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------

*       text
*----------------------------------------------------------------------

*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------

FORM display_output .


data: lv_col_m            TYPE          scrtext_m.

  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = gr_table
    CHANGING
      t_table      = gt_task_sheet[].

*- pf_status
  SET PF-STATUS 'ZFMFP_SALV_STANDARD'.
  gr_table->set_screen_status(
      pfstatus      =  'ZFMFP_SALV_STANDARD'
      report        =  sy-repid
      set_functions = gr_table->c_functions_all ).

*-- events
  gr_events = gr_table->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_click FOR gr_events.

*-- Selection
  gr_selections = gr_table->get_selections( ).
  gr_selections->set_selection_mode(
                     if_salv_c_selection_mode=>row_column ).



  gr_columns = gr_table->get_columns( ).

  gr_columns->set_optimize( 'X' ).

  gr_column ?= gr_columns->get_column( 'ZCOUNT' ).
  gr_column->set_visible( p_det ).

  gr_column ?= gr_columns->get_column( 'ZCOLUMN' ).
  gr_column->set_visible( p_det ).

  gr_column ?= gr_columns->get_column( 'VALUE' ).
  gr_column->set_visible( p_det ).

  gr_column ?= gr_columns->get_column( 'MESSAGE' ).
  gr_column->set_visible( p_det ).


  gr_column ?= gr_columns->get_column( 'XMES' ).
  IF p_det IS INITIAL.
    gr_column->set_visible( abap_true ).
  ELSE.
    gr_column->set_visible( abap_false ).
  ENDIF.

  gr_column ?= gr_columns->get_column( 'VALUE_LEN' ).
  gr_column->set_visible( abap_false ).

lv_col_m = 'Talhão'.
  gr_column ?= gr_columns->get_column( 'TALHAO' ).
  gr_column->SET_medium_TEXT( 'Talhão' ).
  gr_column->SET_long_TEXT( 'Talhão').
  gr_column->SET_short_TEXT( 'Talhão' ).
"  gr_column->set_output_length( 25 ).

*--Display
  gr_table->display( ).



ENDFORM.                    " DISPLAY_OUTPUT
*---------------------
* class implimentation
*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_click.

    "Get Selected lines
    CLEAR gt_rows[].
    gt_rows = gr_selections->get_selected_rows( ).

    DATA wa_task_sheet LIKE LINE OF gt_task_sheet.

    "Check there is at least one line selected
    READ TABLE gt_rows INTO wa_rows INDEX 1.
    IF sy-subrc <> 0.
      MESSAGE i000(zf) WITH 'Select at least one line'.
    ENDIF.

    CLEAR v_nb.
    LOOP AT gt_rows INTO wa_rows.
      READ TABLE gt_task_sheet INTO wa_task_sheet INDEX wa_rows.
      CHECK sy-subrc = 0.
      v_nb = v_nb + 1.
    ENDLOOP. "lt_rows

    v_nb_lines = v_nb.

    CLEAR v_message.
    CONCATENATE 'Number of selected lines:' v_nb_lines INTO v_message SEPARATED BY space.

    MESSAGE  v_message TYPE 'I'.

  ENDMETHOD.                    "on_click
ENDCLASS.
