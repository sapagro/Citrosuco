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

TABLES: zfmachdr,
        zfmrchdr,
        zabs_orcamento,
        zabs_yorcamento,
        zabs_str_yorcamento_fcat.

TYPES: tty_orcamento TYPE STANDARD TABLE OF zabs_str_yorcamento_fcat INITIAL SIZE 0.

DATA: gt_orcamento TYPE STANDARD TABLE OF zabs_str_yorcamento_fcat INITIAL SIZE 0,
      gt_outtab    TYPE tty_orcamento,
      gt_fieldcat  TYPE lvc_t_fcat,
*--Screen attribute declarations
      fcode        TYPE sy-ucomm,
      ok_code      TYPE sy-ucomm,
*--Object declaration
      gobj_alv     TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont    TYPE REF TO cl_gui_custom_container.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_acnum TYPE zfmachdr-acnum.
SELECT-OPTIONS: s_rcnum  FOR zfmrchdr-rcnum,
                s_period FOR zabs_yorcamento-period MODIF ID per.
PARAMETERS: p_date TYPE zabs_yorcamento-data_log,
            p_user TYPE zabs_yorcamento-usuario DEFAULT sy-uname.
SELECT-OPTIONS: s_time   FOR zabs_yorcamento-hora.
PARAMETERS: p_sver TYPE zabs_del_ver_orc MODIF ID t2,
            p_tver TYPE zabs_del_ver_orc MODIF ID t3.
SELECTION-SCREEN END OF BLOCK b1.
*--Parâmetros Adicionais
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
PARAMETERS: p_var TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.
