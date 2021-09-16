*&---------------------------------------------------------------------*
*& Include ZABS_INC_ATUALIZA_SAFRA_TOP
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

*&---------------------------------------------------------------------*
*&    TABLES
*&---------------------------------------------------------------------*
TABLES: /agri/glflot,
        /agri/glflcma.

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_glflot,
         tplnr_fl TYPE /agri/gltplnr_fl,
       END OF ty_glflot,

       BEGIN OF ty_cpros,
         cmnum TYPE /agri/glcmnum,
         varia TYPE /agri/glvaria,
         cpros TYPE /agri/glcpros,
         matnr TYPE matnr,
         midur TYPE /agri/glmidur,
         miunt TYPE /agri/gltunit,
         descr TYPE /agri/gdescr_40,
       END OF ty_cpros,

       BEGIN OF ty_fmfphdr,
         aufnr    TYPE /agri/fmfpnum,
         auart    TYPE aufart,
         autyp    TYPE /agri/gl_autyp,
         tplnr_fl TYPE /agri/gltplnr_fl,
         contr    TYPE /agri/gcontr,
         tplma    TYPE /agri/gltplma,
         cmnum    TYPE /agri/glcmnum,
         varia    TYPE /agri/glvaria,
         cpros    TYPE /agri/glcpros,
         iwerk    TYPE iwerk,
         tecom    TYPE /agri/fmtecom,
       END OF ty_fmfphdr,

       BEGIN OF ty_glmdhdr,
         mdocm    TYPE /agri/glmdocm,
         mdtyp    TYPE /agri/glmdtyp,
         aslvl    TYPE /agri/glaslvl,
         tplnr_fl TYPE /agri/gltplnr_fl,
         contr    TYPE /agri/gcontr,
         cmnum    TYPE /agri/glcmnum,
         equnr    TYPE /agri/glequnr,
         mpgrp    TYPE /agri/glmpgrp,
         mdate    TYPE /agri/glmdate,
         mtime    TYPE imrc_itime,
         atzhl    TYPE atzhl,
         atwrt    TYPE atwrt,
         atflv    TYPE atflv,
         atinn    TYPE atinn,
         atnam    TYPE atnam,
       END OF ty_glmdhdr.

*&---------------------------------------------------------------------*
*&    DATA
*&---------------------------------------------------------------------*
DATA: gt_glflcma   TYPE STANDARD TABLE OF /agri/glflcma INITIAL SIZE 0,
      gt_fmfphdr   TYPE STANDARD TABLE OF ty_fmfphdr INITIAL SIZE 0,
      gt_cpros     TYPE STANDARD TABLE OF ty_cpros INITIAL SIZE 0,
      gt_glmdhdr   TYPE STANDARD TABLE OF ty_glmdhdr INITIAL SIZE 0,
      gt_fcat      TYPE lvc_t_fcat,
      gv_factid    TYPE tkevs-fcalid VALUE 'BR',
      gv_third_day TYPE sydatum,
      gv_monday    TYPE sydatum,
      gv_sunday    TYPE sydatum,
*--Object declaration
      gobj_alv     TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont    TYPE REF TO cl_gui_custom_container,
*--Screen attribute declarations
      fcode        TYPE sy-ucomm,
      ok_code      TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
****Ownership
CONSTANTS : BEGIN OF c_ownership,
              own         TYPE /agri/glownshp VALUE 'OW',
              third_party TYPE /agri/glownshp VALUE 'TP',
            END OF c_ownership,

            BEGIN OF c_crop_season,
              formacao   TYPE /agri/glvaria VALUE 'FORMAÇÃO',
              manutencao TYPE /agri/glvaria VALUE 'MANUT&COLHEITA',
            END OF c_crop_season,

            BEGIN OF c_crop_process,
              formacao    TYPE /agri/glcpros VALUE 'FORMAÇÃO',
              manutencao  TYPE /agri/glcpros VALUE 'MANUTEÇÃO',
              colheita    TYPE /agri/glcpros VALUE 'COLHEITA',
              implantacao TYPE /agri/glcpros VALUE 'IMPLANT',
              close_all   TYPE /agri/glcpros VALUE abap_true,
            END OF c_crop_process.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
*-- Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_tplnr  FOR /agri/glflcma-tplnr_fl,
                s_iwerk  FOR /agri/glflcma-iwerk.
PARAMETERS: p_cmnum TYPE /agri/glflcma-cmnum,
            p_varia TYPE /agri/glflcma-varia,
            p_date  TYPE /agri/glflcma-datab.
SELECTION-SCREEN END OF BLOCK b1.

*-- Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_upd RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND uc MODIF ID id1,
            p_enh RADIOBUTTON GROUP rb1 MODIF ID id1.
SELECTION-SCREEN END OF BLOCK b2.

*-- Mudança de Safra
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_enh1 RADIOBUTTON GROUP rb2 DEFAULT 'X' MODIF ID id2,
            p_enh2 RADIOBUTTON GROUP rb2 MODIF ID id2.
SELECTION-SCREEN END OF BLOCK b3.
