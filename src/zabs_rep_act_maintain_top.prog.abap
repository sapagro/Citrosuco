************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_ACT_MAINTAIN_TOP                           *
* Tcode          : ZABS_FMACTIVITIES                                   *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.23.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Activities Maintainence data declaration and        *
*                  Selection Screen                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Table declarations
TABLES : /agri/fmacact.

*--Types declaration
TYPES: BEGIN OF ty_mod,
         row TYPE i,
       END OF ty_mod.

*--Variables declaration
DATA : BEGIN OF gs_variables,
         mode,
         manual_changes,
         changes,
         error,
         initiator      TYPE /agri/gdescr,
       END OF gs_variables.

*--Table type declarations
TYPES: tty_act_display TYPE STANDARD TABLE OF zabs_str_act_maintain.

*--Global table declarations
DATA : gt_act_display TYPE tty_act_display,
       gt_act_old     TYPE tty_act_display,
       gt_fmacact     TYPE STANDARD TABLE OF /agri/fmacact,
       gt_fmacactt    TYPE STANDARD TABLE OF /agri/fmacactt.

CLASS: lcl_event_handler DEFINITION DEFERRED.

DATA : gr_event_handler TYPE REF TO lcl_event_handler,
       gv_ucomm         TYPE sy-ucomm,
       i_exclude        TYPE ui_functions,
       ok_code          TYPE sy-ucomm,
*--Object declaration
       gobj_grid        TYPE REF TO /agri/cl_gui_alv_grid,
       gobj_docking     TYPE REF TO cl_gui_docking_container.

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
PARAMETERS : r_aupl  RADIOBUTTON GROUP rb1 USER-COMMAND xy DEFAULT 'X',
             r_acrea RADIOBUTTON GROUP rb1,
             r_amain RADIOBUTTON GROUP rb1,
             p_path  TYPE rlgrap-filename MODIF ID p.

SELECT-OPTIONS: s_idactv FOR /agri/fmacact-idactv MODIF ID k1.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
*--Screen validation
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*--File path
  PERFORM f4_filepath.
