************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRRORD_INFO_TOP                        *
* Tcode             :  ZABS_TRN_IRRord_info                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.04.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Order Information                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Tables declaartion
TABLES : /agri/s_fmirhdr,
         /agri/fmwoopr.

*--Constant declarations
CONSTANTS: c_irr_info_0100_cc(30) TYPE c VALUE 'IRR_INFO_0100_CC'.

*--Types declaration
TYPES : BEGIN OF ty_shift,
          vornr TYPE vornr,
          ltxa1 TYPE ltxa1,
        END OF ty_shift,
        tty_shift TYPE TABLE OF ty_shift,
        tty_final TYPE TABLE OF zabs_str_irrord_info.

****Class
CLASS: lcl_event_handler DEFINITION DEFERRED.
*       lcl_log_handler   DEFINITION DEFERRED.

*--Global table declarations
DATA: gt_final          TYPE tty_final,
      gt_shift          TYPE tty_shift,
      ref_event_handler TYPE REF TO lcl_event_handler,
      gobj_cont         TYPE REF TO cl_gui_custom_container,
      gobj_alv          TYPE REF TO /agri/cl_gui_alv_grid,
      fcode             TYPE sy-ucomm,
      ok_code           TYPE sy-ucomm.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_werks TYPE werks_d OBLIGATORY,
            p_equnr TYPE /agri/s_fmirhdr-equnr OBLIGATORY.
SELECT-OPTIONS: so_shift FOR /agri/fmwoopr-vornr,
                so_date  FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:
* on_toolbar_grid      FOR EVENT toolbar
*                  OF /agri/cl_gui_alv_grid
*      IMPORTING e_object e_interactive sender,
*
*      on_user_command_grid FOR EVENT user_command
*                    OF /agri/cl_gui_alv_grid
*        IMPORTING e_ucomm,

      on_hotspot_click     FOR EVENT hotspot_click
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION
