*&---------------------------------------------------------------------*
*& Include          ZABS_INC_ATUALIZA_CLIMA_TOP
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
         pltxt    TYPE /agri/glpltxt,
         tplkz    TYPE /agri/gltplkz,
         fltyp    TYPE /agri/glfltyp,
         tplvl    TYPE /agri/gltplvl,
         tplma    TYPE /agri/gltplma,
         loevm    TYPE loevm,
       END OF ty_glflot,

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
         tplma    TYPE /agri/gltplma,
       END OF ty_glmdhdr.

*&---------------------------------------------------------------------*
*&    DATA
*&---------------------------------------------------------------------*
DATA: gt_glflot  TYPE STANDARD TABLE OF ty_glflot INITIAL SIZE 0,
      gt_glmdhdr TYPE STANDARD TABLE OF ty_glmdhdr INITIAL SIZE 0,
      gt_fcat    TYPE lvc_t_fcat,
      gv_factid  TYPE tkevs-fcalid VALUE 'BR',
*--Object declaration
      gobj_alv   TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont  TYPE REF TO cl_gui_custom_container,
*--Screen attribute declarations
      fcode      TYPE sy-ucomm,
      ok_code    TYPE sy-ucomm.

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
SELECT-OPTIONS: s_tplma FOR /agri/glflot-tplma.
PARAMETERS: p_tipo TYPE zabst_clima_hist-tipo,
            p_data TYPE /agri/glflcma-datab_ref.
SELECTION-SCREEN END OF BLOCK b1.

**-- Opções de Processamento
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*PARAMETERS: p_upd RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND uc MODIF ID id1,
*            p_enh RADIOBUTTON GROUP rb1 MODIF ID id1.
*SELECTION-SCREEN END OF BLOCK b2.
*
**-- Mudança de Safra
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
*PARAMETERS: p_enh1 RADIOBUTTON GROUP rb2 DEFAULT 'X' MODIF ID id2,
*            p_enh2 RADIOBUTTON GROUP rb2 MODIF ID id2.
*SELECTION-SCREEN END OF BLOCK b3.
