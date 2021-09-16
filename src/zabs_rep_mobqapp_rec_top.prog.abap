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

TABLES: zabs_mobqapp_rec.

TYPES: tty_mobqapp_rec TYPE STANDARD TABLE OF zabs_mobqapp_rec INITIAL SIZE 0.

DATA: gt_mobqapp_rec     TYPE STANDARD TABLE OF zabs_mobqapp_rec INITIAL SIZE 0,
      gt_outtab          TYPE tty_mobqapp_rec,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      ok_code            TYPE sy-ucomm.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_ernam TYPE zabs_mobqapp_rec-ernam,
            p_imei  TYPE zabs_mobqapp_rec-zzimei1,
            p_badge TYPE zabs_mobqapp_rec-zbadge.

SELECT-OPTIONS: s_erdat FOR zabs_mobqapp_rec-erdat,
                s_erzet FOR zabs_mobqapp_rec-erzet.
SELECTION-SCREEN END OF BLOCK b1.
