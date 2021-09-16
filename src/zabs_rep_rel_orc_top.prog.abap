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
        twew,
        zabs_orcamento,
        zabs_str_orcamento_fcat.

TYPES: tty_orcamento TYPE STANDARD TABLE OF zabs_str_orcamento_fcat INITIAL SIZE 0.

DATA: gt_orcamento       TYPE STANDARD TABLE OF zabs_str_orcamento_fcat INITIAL SIZE 0,
      gt_outtab          TYPE tty_orcamento,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      ok_code            TYPE sy-ucomm.

*--Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_acnum  FOR zfmachdr-acnum,
                s_extwg  FOR twew-extwg,
                s_matkl  FOR zabs_orcamento-matkl,
                s_rcnum  FOR zfmrchdr-rcnum,
                s_matnr  FOR zabs_orcamento-matnr,
                s_period FOR zabs_orcamento-period,
                s_faz    FOR zabs_orcamento-fazenda.
PARAMETERS: p_versao TYPE zabs_str_orcamento_fcat-versao,
            p_vcusto TYPE zabs_str_orcamento_fcat-versao_custo.
SELECTION-SCREEN END OF BLOCK b1.

*-- Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_det RADIOBUTTON GROUP grp MODIF ID typ DEFAULT 'X'
                    USER-COMMAND grp,
            p_agr RADIOBUTTON GROUP grp MODIF ID typ.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
PARAMETERS: p_var TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.
