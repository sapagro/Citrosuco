*&---------------------------------------------------------------------*
*& Include          ZABS_REP_MEASUREM_DOC_CREATTOP
*&---------------------------------------------------------------------*

*--Table declarations
TABLES: /agri/fmfphdr, werks, /agri/glflot, /agri/glflatv.

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

TYPES: BEGIN OF ty_afko,
         aufnr TYPE aufnr,
         gamng TYPE gamng,
         igmng TYPE co_igmng,
       END OF ty_afko.

DATA: gt_fmfphdr TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gt_afko    TYPE STANDARD TABLE OF ty_afko INITIAL SIZE 0.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
*-- Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_aufnr FOR /agri/fmfphdr-aufnr,
                s_auart FOR /agri/fmfphdr-auart,
                s_iwerk FOR /agri/fmfphdr-iwerk,
                s_erdat FOR /agri/fmfphdr-erdat.
SELECTION-SCREEN END OF BLOCK b1.
