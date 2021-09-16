*&---------------------------------------------------------------------*
*&  Include           /AGRI/GLPG_UPLOAD_PROCESS_TOP
*&---------------------------------------------------------------------*
CONSTANTS: c_program  LIKE sy-repid VALUE '/AGRI/GLPG_UPLOAD_PROCESS'.

CONSTANTS: BEGIN OF c_object,
             log TYPE balobj-object VALUE '/AGRI/GLFL',
           END OF c_object.

CONSTANTS: BEGIN OF c_log_subobject,
              save   TYPE balsubobj VALUE 'SAVE',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_itypes,
              c    TYPE inttype    VALUE 'C',
              quan TYPE datatype_d VALUE 'QUAN',
              unit TYPE datatype_d VALUE 'UNIT',
              dec  TYPE datatype_d VALUE 'DEC',
           END OF  c_itypes.

DATA: BEGIN OF gs_variables,
        overview_mode,
        refresh_monitor_grid,
        errors,
        manual_changes,
        initiator TYPE /agri/gdescr,
      END OF gs_variables.

CLASS lcl_log_handler  DEFINITION DEFERRED.

FIELD-SYMBOLS: <gt_dyntable> TYPE STANDARD TABLE.

DATA: gt_message       TYPE /agri/t_gprolog,
      ref_log_handler  TYPE REF TO lcl_log_handler.
