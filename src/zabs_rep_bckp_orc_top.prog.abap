*&---------------------------------------------------------------------*
*& Include          ZABS_REP_STORDENS_TOP
*&---------------------------------------------------------------------*

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0a.
INCLUDE /agri/glpg_upload_process_f0c.
INCLUDE /agri/glpg_upload_process_f0f.
INCLUDE /agri/glpg_upload_process_f0i.
INCLUDE /agri/glpg_upload_process_f0m.
INCLUDE /agri/glpg_upload_process_f0t.

TYPE-POOLS: abap.

TABLES: zfmachdr,
        zfmrchdr,
        zabs_orcamento,
        zabs_yorcamento,
        zabs_str_orcamento_fcat.

TYPES: tty_orcamento TYPE STANDARD TABLE OF zabs_str_orcamento_fcat INITIAL SIZE 0.

DATA: gt_orcamento_db    TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
      gt_orcamento       TYPE STANDARD TABLE OF zabs_str_orcamento_fcat INITIAL SIZE 0,
      gt_outtab          TYPE tty_orcamento,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      ok_code            TYPE sy-ucomm.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_acnum FOR zfmachdr-acnum.
SELECTION-SCREEN END OF BLOCK b1.
