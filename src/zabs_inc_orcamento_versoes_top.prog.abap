*&---------------------------------------------------------------------*
*& Include          ZABS_REP_INSPREC_UPDATE_TOP
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

TABLES: zabs_orcamento, zabs_yorcamento, zfmachdr, sscrfields.

TYPE-POOLS: abap, icon.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_acnum TYPE zfmachdr-acnum   MODIF ID t2,
            p_sver  TYPE zabs_del_ver_orc MODIF ID t2,
            p_tver  TYPE zabs_del_ver_orc MODIF ID t3.
SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN END OF BLOCK b1.
