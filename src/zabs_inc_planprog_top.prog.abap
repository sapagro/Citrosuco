*&---------------------------------------------------------------------*
*& Include          ZABS_INC_PLANPROG_TOP
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
TABLES: /agri/fmirhdr,
        zabst_irrmon.

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_shift,
         vornr TYPE vornr,
         ltxa1 TYPE ltxa1,
       END OF ty_shift,

       ty_shift_tab TYPE STANDARD TABLE OF ty_shift INITIAL SIZE 0,

       BEGIN OF ty_glflot,
         tplnr_fl TYPE /agri/gltplnr_fl,
         pltxt    TYPE /agri/glpltxt,
         tplkz    TYPE /agri/gltplkz,
         fltyp    TYPE /agri/glfltyp,
         tplvl    TYPE /agri/gltplvl,
         tplma    TYPE /agri/gltplma,
         iwerk    TYPE iwerk,
         swerk    TYPE swerk,
         kokrs    TYPE kokrs,
         anlnr    TYPE anln1,
         kostl    TYPE kostl,
         garea    TYPE /agri/glgarea,
         kfrst    TYPE /agri/glkfrst,
         loevm    TYPE loevm,
       END OF ty_glflot,

       BEGIN OF ty_glflatv,
         tplnr_fl TYPE /agri/gltplnr_fl,
         atwrt    TYPE atwrt,
         atflv    TYPE atflv,
         atinn    TYPE atinn,
       END OF ty_glflatv,

       BEGIN OF ty_glmdhdr_sum,
         irrdate    TYPE sydatum,
         qtd_doctos TYPE i,
         tplma_in   TYPE /agri/gltplma,
         tplma_out  TYPE /agri/gltplma,
         tplnr_fl   TYPE /agri/gltplnr_fl,
         mpgrp      TYPE /agri/glmpgrp,
         atflv      TYPE atflv,
         atinn      TYPE atinn,
         atnam      TYPE atnam,
       END OF ty_glmdhdr_sum,

       BEGIN OF ty_fazendas,
         equnr     TYPE /agri/glequnr,
         tplma_in  TYPE /agri/gltplma,
         tplma_out TYPE /agri/gltplma,
       END OF ty_fazendas,

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
*--Global data declarations
DATA: gt_fmirhdr     TYPE STANDARD TABLE OF /agri/fmirhdr INITIAL SIZE 0,
      gt_fazendas    TYPE STANDARD TABLE OF ty_fazendas INITIAL SIZE 0,
      gt_fmirhdrt    TYPE STANDARD TABLE OF /agri/fmirhdrt INITIAL SIZE 0,
      gt_fmirflo     TYPE STANDARD TABLE OF /agri/fmirflo INITIAL SIZE 0,
      gt_glflot      TYPE STANDARD TABLE OF ty_glflot INITIAL SIZE 0,
      gt_glflcma     TYPE STANDARD TABLE OF /agri/glflcma INITIAL SIZE 0,
      gt_glflatv     TYPE STANDARD TABLE OF ty_glflatv INITIAL SIZE 0,
      gt_glmdhdr     TYPE STANDARD TABLE OF ty_glmdhdr INITIAL SIZE 0,
      gt_glmdhdr_sum TYPE STANDARD TABLE OF ty_glmdhdr_sum INITIAL SIZE 0,
      gt_ordhdr      TYPE STANDARD TABLE OF zabst_ordhdr INITIAL SIZE 0,
      gt_ordcnf      TYPE STANDARD TABLE OF zabst_ordcnf INITIAL SIZE 0,
      gt_clima_hist  TYPE STANDARD TABLE OF zabst_clima_hist INITIAL SIZE 0,
      gt_shift       TYPE ty_shift_tab,
      gt_fcat        TYPE lvc_t_fcat,
      gt_output      TYPE STANDARD TABLE OF zabst_planej INITIAL SIZE 0,
      gt_output_old  TYPE STANDARD TABLE OF zabst_planej INITIAL SIZE 0,
      gt_output_new  TYPE STANDARD TABLE OF zabst_planej INITIAL SIZE 0,
      gt_index_rows  TYPE TABLE OF lvc_s_row,
*--Object declaration
      gobj_alv       TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont      TYPE REF TO cl_gui_custom_container,
*--Screen attribute declarations
      gv_irrtyp      TYPE zabs_del_tipo_irr,
      gv_dias        TYPE zabs_del_qtd_dias,
      gv_tx_repo     TYPE zabs_del_tx_repo,
      fcode          TYPE sy-ucomm,
      ok_code        TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
*--Constant declarations
CONSTANTS: BEGIN OF c_shift,
             1 TYPE zabs_del_shift VALUE '0010',
             2 TYPE zabs_del_shift VALUE '0020',
             3 TYPE zabs_del_shift VALUE '0030',
             4 TYPE zabs_del_shift VALUE '0040',
             5 TYPE zabs_del_shift VALUE '0050',
             6 TYPE zabs_del_shift VALUE '0060',
           END OF c_shift.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
*-- Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE t001w-werks.
SELECT-OPTIONS: so_proj  FOR /agri/fmirhdr-equnr,
                so_shift FOR zabst_irrmon-shift NO-EXTENSION,
                so_date  FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

*-- Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_plan RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND uc MODIF ID id1,
            p_prog RADIOBUTTON GROUP rb1 MODIF ID id1.
SELECTION-SCREEN END OF BLOCK b2.
