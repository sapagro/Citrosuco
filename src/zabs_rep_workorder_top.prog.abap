************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_WORKORDER_TOP                          *
* Tcode             :  ZABS_TRN_IORD_CRT                               *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  02.14.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Monitor Data Preperation and Display *
*                      Data.                                           *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

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

*&--------------------------------------------------------------------*
*&                             TYPES                                  *
*&--------------------------------------------------------------------*
TYPES : BEGIN OF ty_var,
          next_no TYPE /agri/fmfpnum,
          contr   TYPE nrnr,
        END OF ty_var.

*&---------------------------------------------------------------------*
*&    DATA
*&---------------------------------------------------------------------*
*--Global data declarations
DATA: gt_fmirhdr TYPE STANDARD TABLE OF /agri/fmirhdr INITIAL SIZE 0,
      gt_planej  TYPE STANDARD TABLE OF zabst_planej INITIAL SIZE 0,
      gt_ordhdr  TYPE STANDARD TABLE OF zabst_ordhdr INITIAL SIZE 0.

*&--------------------------------------------------------------------*
*&                        SELECTION-SCREEN                            *
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_werks TYPE werks_d.
SELECT-OPTIONS: so_equnr FOR /agri/fmirhdr-equnr.
PARAMETERS: p_date TYPE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.
