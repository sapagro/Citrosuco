************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PARAM_HCM_EMPREC                           *
* Tcode          : ZABS_PARAM_HCMEMP                                   *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mauricio J. Pereira                                 *
* Created on     : 12.07.2021                                          *
* Description    : Maitenance for ZABS_EMP_HCM_UPD parameter HCM table *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

TABLES: zabs_emp_hcm_upd.

INCLUDE <color>.

INCLUDE <icon>.
INCLUDE <symbol>.


CONSTANTS: gc_true TYPE sap_bool VALUE 'X'.

*... §5 Definition is later
CLASS lcl_handle_events DEFINITION DEFERRED.


TYPES: BEGIN OF ty_outtab,
         status TYPE char10.
    INCLUDE TYPE zabs_emp_hcm_upd.
TYPES: END OF ty_outtab.

DATA: gt_outtab TYPE STANDARD TABLE OF ty_outtab.

DATA: gr_table   TYPE REF TO cl_salv_table.

DATA: gr_events TYPE REF TO lcl_handle_events.

DATA: gv_upd   TYPE sap_bool.


*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

*      on_double_click for event double_click of cl_salv_events_table
*        importing row column,
*
*      on_link_click for event link_click of cl_salv_events_table
*        importing row column.
ENDCLASS.                    "lcl_handle_events DEFINITION
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_before_command FOR EVENT before_user_command OF cl_gui_alv_grid.
ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_before_command.
    FIELD-SYMBOLS: <layout> TYPE kkblo_layout.
    FIELD-SYMBOLS: <grids>  TYPE STANDARD TABLE.

    ASSIGN ('(SAPLSLVC_FULLSCREEN)GT_GRID[]') TO <grids>.
    IF sy-subrc = 0.
      ASSIGN <grids>[ 1 ] TO FIELD-SYMBOL(<grid>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'S_LAYOUT' OF STRUCTURE <grid> TO <layout>.
        IF sy-subrc = 0.
          <layout>-confirmation_prompt = gv_upd.
          ASSIGN ('(SAPLSLVC_FULLSCREEN)GT_GRID-S_LAYOUT') TO <layout>.
          IF sy-subrc = 0.
            <layout>-confirmation_prompt = gv_upd.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*                   SELECTION-SCREEN                                   *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-004.

SELECT-OPTIONS:
        s_farm FOR zabs_emp_hcm_upd-fazenda_farm,
        s_arbpl        FOR zabs_emp_hcm_upd-arbpl,
        s_kostl        FOR zabs_emp_hcm_upd-kostl,
        s_orgeh        FOR zabs_emp_hcm_upd-orgeh,
        s_stell        FOR zabs_emp_hcm_upd-stell.


SELECTION-SCREEN END OF BLOCK b1.
