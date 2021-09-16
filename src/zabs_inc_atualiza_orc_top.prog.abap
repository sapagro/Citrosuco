*&---------------------------------------------------------------------*
*& Include          ZABS_INC_ATUALIZA_ORC_TOP
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

TYPES: BEGIN OF ty_terrains,
         tplnr_in  TYPE /agri/gltplnr_fl,
         tplnr_out TYPE /agri/gltplnr_fl,
       END OF ty_terrains.

TYPES: tty_terrains TYPE TABLE OF ty_terrains.

DATA: gt_achdr     TYPE STANDARD TABLE OF zfmachdr INITIAL SIZE 0,
      gt_orcamento TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
      gt_update    TYPE STANDARD TABLE OF zabs_str_orc_fcat INITIAL SIZE 0,
      gt_terrains  TYPE tty_terrains,
      gt_processos TYPE zct_grp_mara,
      gs_achdr     TYPE zfmachdr,
      gt_fcat      TYPE lvc_t_fcat,
*--Screen attribute declarations
      fcode        TYPE sy-ucomm,
      ok_code      TYPE sy-ucomm,
*--Object declaration
      gobj_alv     TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont    TYPE REF TO cl_gui_custom_container.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_acnum TYPE zfmachdr-acnum,
            p_vers  TYPE zabs_del_ver_orc MODIF ID sel,
            p_save  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
