*&---------------------------------------------------------------------*
*& Include ZABS_INC_EXCEL_UPLOAD_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: abap.

TABLES: /agri/fmprhdr,
        likp.

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
INCLUDE zabs_inc_checkin_fruta_sel.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0m.
