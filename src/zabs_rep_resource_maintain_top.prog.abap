************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_RESOURCE_MAINTAIN_TOP                      *
* Tcode          : ZABS_FMRESOURCES                                    *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.18.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Resource Maintainence data declaration and          *
*                  Selection Screen                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Table declarations
TABLES : /agri/fmacres.

*--Types declaration
TYPES: BEGIN OF ty_mod,
         row TYPE i,
       END OF ty_mod.

*--Varaibles declaration
DATA : BEGIN OF gs_variables,
         mode,
         manual_changes,
         changes,
         error,
         initiator      TYPE /agri/gdescr,
       END OF gs_variables,

       lv_ucomm     TYPE sy-ucomm,
       i_exclude    TYPE ui_functions,
       ok_code      TYPE sy-ucomm,

*--Objects declaration
       gobj_grid    TYPE REF TO /agri/cl_gui_alv_grid,
       gobj_docking TYPE REF TO cl_gui_docking_container.

*--Table type declarations
TYPES: tty_res_display TYPE STANDARD TABLE OF zabs_str_resource_maintain.

*--Global table declarations
DATA : gt_res_display TYPE tty_res_display,
       gt_res_old     TYPE tty_res_display.

CLASS: lcl_event_handler DEFINITION DEFERRED.

DATA : gr_event_handler TYPE REF TO lcl_event_handler.

*&---------------------------------------------------------------------*
* Class Definition used to get changed data
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: handle_data_changed FOR EVENT data_changed
                  OF cl_gui_alv_grid
      IMPORTING er_data_changed
                  e_onf4
                  e_onf4_before
                  e_onf4_after,

      handle_toolbar      FOR EVENT toolbar
                    OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command
                    OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_handler DEFINITION

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : r_rupl  RADIOBUTTON GROUP rb1 USER-COMMAND xy DEFAULT 'X',
             r_rcrea RADIOBUTTON GROUP rb1,
             r_rmain RADIOBUTTON GROUP rb1,
             p_path  TYPE rlgrap-filename MODIF ID p.

SELECT-OPTIONS: s_idrsc FOR /agri/fmacres-idresource MODIF ID k1,
                s_arbpl FOR /agri/fmacres-arbpl      MODIF ID k2.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
*--Screen validation
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*--File path
  PERFORM f4_filepath.
