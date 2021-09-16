*&---------------------------------------------------------------------*
*& Include ZABS_INC_FMACM_UPDATE_TOP
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

TABLES: zfmachdr.

TYPE-POOLS: abap.

DATA: gt_achdr_old     TYPE STANDARD TABLE OF zfmachdr INITIAL SIZE 0,
      gt_acitm_old     TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
*-- BOC T_T.KONNO 04.07.21
      gt_glflcma       TYPE STANDARD TABLE OF /agri/glflcma INITIAL SIZE 0,
*-- EOC T_T.KONNO 04.07.21
      gt_acvlcl_old    TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
      gs_acdoc_infocus TYPE zsc_fmac_doc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: so_acnum FOR zfmachdr-acnum.
PARAMETERS: p_ajahr TYPE zfmachdr-ajahr.
*-- BOC T_T.KONNO 04.07.21
*-- Indicador /AGRI/GLMDM
PARAMETERS: p_glmdm TYPE abap_bool NO-DISPLAY.
*-- EOC T_T.KONNO 04.07.21
SELECTION-SCREEN END OF BLOCK b1.
