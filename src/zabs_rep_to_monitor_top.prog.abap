************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TO_MONITOR                             *
* Tcode             :  ZABS_TRN_TOMON                                  *
* Include Name      :  ZABS_REP_TO_MONITOR_TOP                         *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.31.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Task Order Monitor global data declaration and  *
*                      Selection Screen                                *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Table declarations
TABLES: /agri/fmfphdr.

*--Constant declarations
CONSTANTS: c_to_monitor_0100_cc(30) TYPE c              VALUE 'TO_MONITOR_0100_CC',
           c_domname_doc_cat        TYPE dd07l-domname  VALUE '/AGRI/GL_AUTYP'.

*--Table type declarations
TYPES: tty_to_monitor TYPE TABLE OF zabs_str_to_monitor_fcat.

*--Global table declarations
DATA: gt_to_monitor TYPE tty_to_monitor.

*--Global reference declarations
DATA: ref_grid_task_ord      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_task_ord TYPE REF TO cl_gui_custom_container.

*--Screen attribute declarations
DATA: fcode   TYPE sy-ucomm,
      ok_code TYPE sy-ucomm.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_werks TYPE /agri/fmfphdr-iwerk OBLIGATORY.

SELECT-OPTIONS: so_matnr FOR /agri/fmfphdr-matnr OBLIGATORY,
                so_tplnr FOR /agri/fmfphdr-tplnr_fl,
                so_cmnum FOR /agri/fmfphdr-cmnum NO INTERVALS NO-EXTENSION,
                so_datum FOR sy-datum NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK b1.
