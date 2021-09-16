FUNCTION-POOL zabs_fg_disp_ord.             "MESSAGE-ID ..

* INCLUDE LZABS_FG_DISP_ORDD...              " Local class definition

*-----------------------------------------------------------------------*
*                              INCLUDES                                 *
*-----------------------------------------------------------------------*
INCLUDE: /agri/global_macros,
         /agri/abgl_constants,
         /agri/global_constants,
         /agri/gprolog_macros.

*-----------------------------------------------------------------------*
*                                  TABLES                               *
*-----------------------------------------------------------------------*
TABLES: /agri/s_glflcma, zabs_str_batch.

*-----------------------------------------------------------------------*
*                                  CONSTANTS                            *
*-----------------------------------------------------------------------*
CONSTANTS : c_str_dispord_fcat TYPE dd02l-tabname
                               VALUE 'ZABS_STR_DISPORD_FCAT'.

****Log Sub-object
CONSTANTS : BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_fcode,
             create                TYPE sy-ucomm VALUE 'CREA',
             sc_set_values         TYPE sy-ucomm VALUE 'SETVAL',
             enter                 TYPE sy-ucomm VALUE 'ENTR',
             insert_disp_ord       TYPE sy-ucomm VALUE 'BINS',
             delete_disp_ord       TYPE sy-ucomm VALUE 'BDEL',
             change_colum_disp_ord TYPE sy-ucomm VALUE 'BCOL',
             create_cat            TYPE sy-ucomm VALUE 'CRCA',
           END OF c_fcode.

CONSTANTS: BEGIN OF c_screen,
             disp_ord     TYPE sy-dynnr VALUE '0100',
             create_batch TYPE sy-dynnr VALUE '0201',
           END OF c_screen.

*-----------------------------------------------------------------------*
*                                  TYPES                                *
*-----------------------------------------------------------------------*
TYPES: tty_tab_dispord TYPE STANDARD TABLE OF zabs_tab_dispord.

TYPES : BEGIN OF ty_var,
          next_no TYPE  zabs_del_batch,
          ctext   TYPE  zabs_del_cnval,
          contr   TYPE  nrnr,
        END OF ty_var.

*-----------------------------------------------------------------------*
*                                 CLASS                                 *
*-----------------------------------------------------------------------*
CLASS: lcl_event_handler DEFINITION DEFERRED.

*-----------------------------------------------------------------------*
*                            INTERNAL TABLES                            *
*-----------------------------------------------------------------------*
DATA: gt_disp_ord_fcat TYPE zabs_tty_dispord_fcat,
      gt_csdoc_infocus TYPE /agri/t_glcs_doc.

*-----------------------------------------------------------------------*
*                             WORKAREA                                  *
*-----------------------------------------------------------------------*
DATA : gs_csdoc_infocus TYPE /agri/s_glcs_doc.

*-----------------------------------------------------------------------*
*                                VARIABLES                              *
*-----------------------------------------------------------------------*
DATA: BEGIN OF gs_variables,
        schedule,
        refresh_disp_ord,
        refresh_colum_disp_ord,
        nustn_txt              TYPE /agri/glpltxt,
        task_material          TYPE matnr,
        task_description       TYPE maktx,
        refresh_items_grid,
        initiator              TYPE /agri/gdescr,
        document_mode,
        manual_changes,
        display_mode           TYPE xfeld,
        error,
      END OF gs_variables.

DATA: fcode           TYPE sy-ucomm,
      ok_code         TYPE sy-ucomm,
      gv_canc         TYPE abap_bool VALUE abap_false,
      gv_grid_refresh TYPE xfeld.

*-----------------------------------------------------------------------*
*                             OBJECT VARIABLES                          *
*-----------------------------------------------------------------------*
DATA: ref_items_grid         TYPE REF TO /agri/cl_gui_alv_grid,
      ref_items_container    TYPE REF TO cl_gui_custom_container,
      ref_event_handler      TYPE REF TO lcl_event_handler,
      ref_badi_glfl_all      TYPE REF TO /agri/badi_glfl_all,
      ref_grid_disp_ord      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_disp_ord TYPE REF TO cl_gui_custom_container.

*-----------------------------------------------------------------------*
* CLASS lcl_event_handler DEFINITION
*-----------------------------------------------------------------------*
*lcl_event_handler
*-----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:  on_toolbar_grid      FOR EVENT toolbar
                  OF /agri/cl_gui_alv_grid
      IMPORTING e_object e_interactive sender,

      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,
*
*      on_hotspot_click     FOR EVENT hotspot_click
*                    OF /agri/cl_gui_alv_grid
*        IMPORTING e_row_id e_column_id es_row_no,

      on_data_changed_grid FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed sender.
*
*      on_f4_request_grid   FOR EVENT onf4
*                    OF /agri/cl_gui_alv_grid
*        IMPORTING e_fieldname e_fieldvalue
*                    es_row_no er_event_data
*                    et_bad_cells e_display
*                    sender.

ENDCLASS.                    "lcl_event_handler DEFINITION
