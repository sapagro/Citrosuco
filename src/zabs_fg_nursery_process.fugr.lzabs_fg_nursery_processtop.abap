FUNCTION-POOL zabs_fg_nursery_process.      "MESSAGE-ID ..

* INCLUDE LZABS_FG_NURSERY_PROCESSD...       " Local class definition

INCLUDE: /agri/global_macros.

***Class
CLASS: lcl_event_handler DEFINITION DEFERRED.

TABLES rmclf.

CONSTANTS: c_str_batch_char TYPE dd02l-tabname  VALUE 'ZABST_BTCHCHR'.

CONSTANTS: BEGIN OF c_fcode,
             continue                TYPE sy-ucomm VALUE 'CONT',
             enter                   TYPE sy-ucomm VALUE 'ENTR',
             insert_batch_char       TYPE sy-ucomm VALUE 'BINS',
             delete_batch_char       TYPE sy-ucomm VALUE 'BDEL',
             change_colum_batch_char TYPE sy-ucomm VALUE 'BCOL',
             create_cat              TYPE sy-ucomm VALUE 'CRCA',
           END OF c_fcode.

TYPES: tty_batch_char TYPE STANDARD TABLE OF zabst_btchchr.

DATA: ref_grid_mat_char      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_mat_char TYPE REF TO cl_gui_custom_container,
      ref_event_handler      TYPE REF TO lcl_event_handler.

DATA: fcode           TYPE sy-ucomm,
      ok_code         TYPE sy-ucomm,
      gv_grid_refresh TYPE xfeld.

DATA: BEGIN OF gs_variables,
        data_change,
        data_saved,
        refresh_batch_char,
        refresh_colum_batch_char,
        overview_mode,
        aufnr                    TYPE aufnr,
      END OF gs_variables.

DATA: gt_batch_char TYPE tty_batch_char,
      gt_char       TYPE STANDARD TABLE OF api_char,
      gv_classnum   TYPE bapi1003_key-classnum.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:
      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed_grid FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed sender,

      on_f4_request FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display.

ENDCLASS.                    "lcl_event_handler DEFINITION
