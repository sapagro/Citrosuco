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

TABLES: /agri/fmfphdr.

TYPES: BEGIN OF ty_fmfphdr,
         aufnr    TYPE /agri/fmfpnum,
         auart    TYPE aufart,
         autyp    TYPE /agri/gl_autyp,
         tplnr_fl TYPE /agri/gltplnr_fl,
         tplma    TYPE /agri/gltplma,
         class    TYPE /agri/glab_class,
         matnr    TYPE /agri/glmatnr,
         cstat    TYPE /agri/fmcstat,
         iwerk    TYPE iwerk,
         tecom    TYPE /agri/fmtecom,
         ernam    TYPE /agri/gernam,
         erdat    TYPE erdat,
       END OF ty_fmfphdr,

       tty_fmfphdr TYPE STANDARD TABLE OF ty_fmfphdr INITIAL SIZE 0.

DATA: gt_fmfphdr         TYPE tty_fmfphdr,
      gt_fmfpitm         TYPE STANDARD TABLE OF /agri/fmfpitm INITIAL SIZE 0,
      gt_outtab          TYPE tty_fmfphdr,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      ok_code            TYPE sy-ucomm.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_iwerk FOR /agri/fmfphdr-iwerk,
                s_auart FOR /agri/fmfphdr-auart,
                s_aufnr FOR /agri/fmfphdr-aufnr,
                s_matnr FOR /agri/fmfphdr-matnr,
                s_erdat FOR /agri/fmfphdr-erdat,
                s_ernam FOR /agri/fmfphdr-ernam.
SELECTION-SCREEN END OF BLOCK b1.

*--Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_tecom RADIOBUTTON GROUP grp MODIF ID typ DEFAULT 'X'
                    USER-COMMAND grp,
            p_canc  RADIOBUTTON GROUP grp MODIF ID typ,
            p_alv   RADIOBUTTON GROUP grp MODIF ID typ.
SELECTION-SCREEN END OF BLOCK b2.
