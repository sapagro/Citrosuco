*&---------------------------------------------------------------------*
*& Include          ZABS_REP_MEASUREM_DOC_CREATTOP
*&---------------------------------------------------------------------*

*--Table declarations
TABLES: werks, /agri/glflot, /agri/glflatv.

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

TYPES: BEGIN OF ty_notes_buffer,
         objtyp    LIKE tojtb-name,
         objkey    LIKE /agri/gnotes-object,
         subobject LIKE /agri/gnotes-subobject,
         posnr     TYPE numc06,
         notes     TYPE /agri/t_gnote,
         subscreen LIKE sy-dynnr,
         changed,
       END OF ty_notes_buffer.

*--Table type declarations
TYPES: tty_terrain_dtls TYPE TABLE OF zabs_str_terrain_dtls,
       tty_notes_buffer TYPE TABLE OF ty_notes_buffer,
       ty_atwrt         TYPE RANGE OF /agri/glflatv-atwrt.

DATA: gt_terrain_dtls TYPE tty_terrain_dtls,
      gt_notes_buffer TYPE tty_notes_buffer,
      gt_farm_subtot  TYPE zabs_tty_farm_subtot,
      gt_sheet        TYPE /agri/t_excel_sheet,
      gv_ano_safra    TYPE zfmanosafra,
      gv_periodo      TYPE char7,
      gt_notes_buffer_db LIKE TABLE OF zabs_notes_buff WITH HEADER LINE,
      gv_tipo_laudo   TYPE char3,
      gv_print        TYPE abap_bool,
      gv_plantas      TYPE i,
      gv_centro       TYPE i,
      gv_norte        TYPE i,
      gv_sul          TYPE i,
      gv_exsul        TYPE i.
