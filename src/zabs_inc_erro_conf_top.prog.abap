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

TABLES: /agri/fmachdr,
        /agri/fmacitm.

TYPES: BEGIN OF ty_aufk,
         aufnr  TYPE aufk-aufnr,
         auart  TYPE aufk-auart,
         autyp  TYPE aufk-autyp,
         bukrs  TYPE aufk-bukrs,
         werks  TYPE aufk-werks,
         kokrs  TYPE aufk-kokrs,
         waers  TYPE aufk-waers,
         loekz  TYPE aufk-loekz,
         gasmg  TYPE afko-gasmg,
         gamng  TYPE afko-gamng,
         gmein  TYPE afko-gmein,
         plnbez TYPE afko-plnbez,
         plnty  TYPE afko-plnty,
         plnnr  TYPE afko-plnnr,
         plnal  TYPE afko-plnal,
         stlty  TYPE afko-stlty,
         stlbez TYPE afko-stlbez,
         stlst  TYPE afko-stlst,
         stlnr  TYPE afko-stlnr,
       END OF ty_aufk,

       BEGIN OF ty_fmacitm,
         accom      TYPE /agri/fmaccom,
         actyp      TYPE /agri/fmactyp,
         gjahr      TYPE gjahr,
         bukrs      TYPE bukrs,
         werks      TYPE werks_d,
         status     TYPE /agri/fmac_status,
         posnr      TYPE /agri/glposnr,
         tplnr      TYPE /agri/gltplnr_fl,
         tmatnr     TYPE /agri/gltmatnr,
         aufnr      TYPE aufnr,
         idresource TYPE /agri/fmidrsc,
         arbpl      TYPE arbpl,
         equnr      TYPE /agri/fmaequnr,
         strtdat    TYPE /agri/fmastrtdat,
         strttim    TYPE /agri/fmastrttim,
         findat     TYPE /agri/fmafindat,
         fintim     TYPE /agri/fmafintim,
         duran      TYPE /agri/glduran,
         idactvl    TYPE /agri/fmidactl,
         idactve    TYPE /agri/fmidacte,
         menge      TYPE menge_d,
       END OF ty_fmacitm.

TYPES: tty_erro_conf TYPE STANDARD TABLE OF zabs_str_erro_conf INITIAL SIZE 0.

DATA: gt_outtab          TYPE tty_erro_conf,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      ok_code            TYPE sy-ucomm.

DATA: gt_fmacitm       TYPE STANDARD TABLE OF ty_fmacitm INITIAL SIZE 0,
      gt_activity      TYPE STANDARD TABLE OF /agri/fmacact INITIAL SIZE 0,
      gt_aufk          TYPE STANDARD TABLE OF ty_aufk INITIAL SIZE 0,
      gt_fmac_messages TYPE /agri/t_gprolog,
      gt_acdoc         TYPE /agri/t_fmacs_doc,
      gt_accom         TYPE /agri/t_fmacom,
      gt_messages      TYPE /agri/t_gprolog,
      gt_acitm_new     TYPE /agri/t_fmfmacitm,
      gs_acdoc_new     TYPE /agri/s_fmacs_doc,
      gs_accom         TYPE /agri/s_fmacom.

CONSTANTS: BEGIN OF c_rstype,
             labor TYPE /agri/fmrstype VALUE 'A',
             equnr TYPE /agri/fmrstype VALUE 'B',
           END OF c_rstype.

CONSTANTS: BEGIN OF c_conftype,
             partial    TYPE zabs_del_conftyp VALUE 'P',
             complement TYPE zabs_del_conftyp VALUE 'C',
             total      TYPE zabs_del_conftyp VALUE 'T',
           END OF c_conftype.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_actyp  FOR /agri/fmachdr-actyp,
                s_werks  FOR /agri/fmachdr-werks,
                s_tplnr  FOR /agri/fmacitm-tplnr,
                s_tmatnr FOR /agri/fmacitm-tmatnr,
                s_aufnr  FOR /agri/fmacitm-aufnr,
                s_arbpl  FOR /agri/fmacitm-arbpl,
                s_begda  FOR /agri/fmacitm-strtdat,
                s_endda  FOR /agri/fmachdr-findat.
SELECTION-SCREEN END OF BLOCK b2.
