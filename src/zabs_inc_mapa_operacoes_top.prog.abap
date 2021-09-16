*&---------------------------------------------------------------------*
*& Include ZABS_INC_MAPA_CARENCIA_TOP
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
TABLES: /agri/glflot, mara.

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
*DATA: status TYPE char3,
*      color  TYPE char3.

TYPES: BEGIN OF gy_output,
         tplnr_fl    TYPE /agri/gltplnr_fl,
         pltxt       TYPE /agri/glpltxt,
         tplkz       TYPE /agri/gltplkz,
         tplvl       TYPE /agri/gltplvl,
         tplma       TYPE /agri/gltplma,
         bukrs       TYPE bukrs,
         iwerk       TYPE iwerk,
         swerk       TYPE swerk,
         kokrs       TYPE kokrs,
         anlnr       TYPE anln1,
         kostl       TYPE kostl,
         ownshp      TYPE /agri/glownshp,
         status      TYPE zabs_del_stat,
         color       TYPE zabs_del_cor,
         tarefa      TYPE /agri/glmatnr,
         tarefa_txt  TYPE maktx,
         data_inicio TYPE co_gstrp,
         data_fim    TYPE co_gltrp,
         aufnr_to    TYPE /agri/fmaufnr_to,
       END OF gy_output,

       BEGIN OF gy_fmocopr,
         ocnum TYPE /agri/fmocnum,
         rueck TYPE co_rueck,
         rmzhl TYPE co_rmzhl,
         aufnr TYPE aufnr,
         posnr TYPE co_posnr,
         vornr TYPE vornr,
         ltxa1 TYPE ltxa1,
         lmnga TYPE /agri/fmlmnga,
         meinh TYPE vorme,
         budat TYPE budat,
       END OF gy_fmocopr,

       BEGIN OF gy_matdoc,
         budat TYPE budat,
         cpudt TYPE cpudt,
         cputm TYPE cputm,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         zeile TYPE mblpo,
         aufnr TYPE aufnr,
         anln1 TYPE anln1,
         anln2 TYPE anln2,
         bwart TYPE bwart,
       END OF gy_matdoc,

       BEGIN OF gy_mara,
         matnr TYPE matnr,
         mtart TYPE mtart,
         matkl TYPE matkl,
         maktx TYPE maktx,
       END OF gy_mara,

       gy_budat_range TYPE RANGE OF budat,

       gy_matnr_range TYPE RANGE OF matnr.

*&---------------------------------------------------------------------*
*&    DATA
*&---------------------------------------------------------------------*
*--Global data declarations
DATA: gt_farm     TYPE STANDARD TABLE OF gy_output INITIAL SIZE 0,
      gt_glflot   TYPE STANDARD TABLE OF gy_output INITIAL SIZE 0,
      gt_mara     TYPE STANDARD TABLE OF gy_mara INITIAL SIZE 0,
      gt_fmfphdr  TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gt_fmfpbch  TYPE STANDARD TABLE OF /agri/fmfpbch INITIAL SIZE 0,
      gt_fmfpitm  TYPE STANDARD TABLE OF /agri/fmfpitm INITIAL SIZE 0,
      gt_fmfpcom  TYPE STANDARD TABLE OF /agri/fmfpcom INITIAL SIZE 0,
      gt_fmocindx TYPE STANDARD TABLE OF /agri/fmocindx INITIAL SIZE 0,
      gt_fmocopr  TYPE STANDARD TABLE OF gy_fmocopr INITIAL SIZE 0,
      gt_fmoccom  TYPE STANDARD TABLE OF /agri/fmoccom INITIAL SIZE 0,
      gt_afwi     TYPE STANDARD TABLE OF afwi INITIAL SIZE 0,
      gt_matdoc   TYPE STANDARD TABLE OF gy_matdoc INITIAL SIZE 0,
      gt_output   TYPE STANDARD TABLE OF gy_output INITIAL SIZE 0,
      gt_fcat     TYPE lvc_t_fcat,
*--Object declaration
      gobj_alv    TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_cont   TYPE REF TO cl_gui_custom_container,
*--Screen attribute declarations
      fcode       TYPE sy-ucomm,
      ok_code     TYPE sy-ucomm.

DATA: html_control   TYPE REF TO cl_gui_html_viewer,
      html_container TYPE REF TO cl_gui_custom_container,
      html_event_tab TYPE cntl_simple_events,
      html_event     TYPE cntl_simple_event,
      edurl(2048),
      cb_no3d        TYPE c,
      cb_noiectx     TYPE c,
      ui_flag        TYPE i,
      alignment      TYPE i.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS: BEGIN OF c_color,
             planned     TYPE zabs_del_cor VALUE 'red',
             not_planned TYPE zabs_del_cor VALUE 'green',
           END OF c_color.

CONSTANTS: BEGIN OF c_status,
             planned     TYPE zabs_del_stat VALUE 'PLA',
             not_planned TYPE zabs_del_stat VALUE 'NPL',
           END OF c_status.

CONSTANTS: BEGIN OF c_screen,
             overview TYPE sy-dynnr VALUE '0100',
             map      TYPE sy-dynnr VALUE '0200',
           END OF c_screen.

CONSTANTS: BEGIN OF map_color,
             blue   TYPE zabs_del_cor VALUE 'blue',
             red    TYPE zabs_del_cor VALUE 'red',
             green  TYPE zabs_del_cor VALUE 'green',
             orange TYPE zabs_del_cor VALUE 'orange',
             black  TYPE zabs_del_cor VALUE 'black',
             purple TYPE zabs_del_cor VALUE 'purple',
           END OF map_color.

CONSTANTS: BEGIN OF c_move_type,
             goods_receipt TYPE bwart VALUE '101',
           END OF c_move_type.

*****************************************************
*              CLASS cl_myevent_handler             *
*****************************************************
CLASS cl_myevent_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_navigate_complete
                  FOR EVENT navigate_complete OF cl_gui_html_viewer
      IMPORTING url,

      on_ctxmenu_request
        FOR EVENT ctxmenu_request OF cl_gui_html_viewer,

      on_ctxmenu_selected
            FOR EVENT ctxmenu_selected OF cl_gui_html_viewer
        IMPORTING
            fcode.

ENDCLASS.

DATA: evt_receiver TYPE REF TO cl_myevent_handler.

****************************************************
*    cl_myevent_handler implementation             *
****************************************************
CLASS cl_myevent_handler IMPLEMENTATION.

  METHOD on_navigate_complete.
    edurl = url.

  ENDMETHOD.

  METHOD on_ctxmenu_request.
*    IF my_ctxmenu IS INITIAL.
*      CREATE OBJECT my_ctxmenu.
*      CALL METHOD my_ctxmenu->add_function
*        EXPORTING
*          fcode = 'sap'                                    "#EC NOTEXT
*          text  = 'To SAP home'.                            "#EC NOTEXT
*
*      CALL METHOD my_ctxmenu->add_function
*        EXPORTING
*          fcode = 'yahoo'                                  "#EC NOTEXT
*          text  = 'To Yahoo home'.                          "#EC NOTEXT
*
*      CALL METHOD my_ctxmenu->add_separator.
*
*      CALL METHOD my_ctxmenu->add_function
*        EXPORTING
*          fcode = 'print'                                  "#EC NOTEXT
*          text  = 'Print'.                                  "#EC NOTEXT
*
*    ENDIF.
*
*    CALL METHOD html_control->track_context_menu
*      EXPORTING
*        ctxmenu = my_ctxmenu.
  ENDMETHOD.

  METHOD on_ctxmenu_selected.
*    CASE fcode.
*      WHEN 'sap'.
*        edurl = 'http://www.sap.com'.                       "#EC NOTEXT
*        CALL METHOD html_control->show_url
*          EXPORTING
*            url = edurl.
*      WHEN 'yahoo'.
*        edurl = 'http://www.yahoo.com'.                     "#EC NOTEXT
*        CALL METHOD html_control->show_url
*          EXPORTING
*            url = edurl.
*      WHEN 'print'.
*        CALL METHOD html_control->execwb
*          EXPORTING
*            cmd_id = html_control->wb_cmdid_print.
*    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
*-- Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_iwerk TYPE /agri/glflot-iwerk.
SELECT-OPTIONS: s_tplnr FOR /agri/glflot-tplnr_fl MODIF ID gen,
                s_date  FOR syst-datum.
SELECTION-SCREEN END OF BLOCK b1.

*-- Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_alv RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND uc MODIF ID id1,
            p_map RADIOBUTTON GROUP rb1 MODIF ID id1.
SELECTION-SCREEN END OF BLOCK b2.
