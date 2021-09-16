************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_INTEGRATION_TOP                        *
* Tcode          : ZABS_PEHCM                                          *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Rapheal                                             *
* Created on     : 10.30.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Update productions and events into HCM Info type    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*&                        TYPES
*&---------------------------------------------------------------------*
  TYPES: BEGIN OF ty_turmas,
           orgeh TYPE orgeh,
           turma TYPE yocod_turma,
         END OF ty_turmas.

  TYPES: BEGIN OF ty_resources,
           idresource TYPE /agri/fmidrsc,
           arbpl      TYPE arbpl,
           pernr      TYPE co_pernr,
         END OF ty_resources.

  TYPES: BEGIN OF ty_terrains,
           tplnr_fl TYPE /agri/gltplnr_fl,
           form     TYPE /agri/glstrno,
           terrain  TYPE /agri/glstrno,
         END OF ty_terrains.

*&---------------------------------------------------------------------*
*&                     GLOBAL DATA
*&---------------------------------------------------------------------*
  DATA: gt_productions TYPE STANDARD TABLE OF p9900,
        gt_events      TYPE STANDARD TABLE OF p2001,
        gt_dados       TYPE STANDARD TABLE OF zhrst_ocorr_aprov,
        gt_output      TYPE STANDARD TABLE OF zabs_s_hcm_output,
        gt_inactive    TYPE STANDARD TABLE OF zabs_s_hcm_output,
        gobj_cont      TYPE REF TO cl_gui_custom_container,
        gobj_grid      TYPE REF TO cl_gui_alv_grid.

*&---------------------------------------------------------------------*
*&                     TABLES
*&---------------------------------------------------------------------*
  TABLES: /agri/fmachdr, /agri/fmacitm.

*&---------------------------------------------------------------------*
*&                     CONSTANT
*&---------------------------------------------------------------------*
 CONSTANTS: c_cnf TYPE c LENGTH 3 VALUE 'CNF'.


*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_actyp TYPE /agri/fmachdr-actyp OBLIGATORY DEFAULT 'TAS2',
              p_date  TYPE /agri/fmacitm-zzcdate OBLIGATORY
                                                        DEFAULT sy-datum.
  SELECT-OPTIONS: so_accom FOR /agri/fmachdr-accom,
                  so_time  FOR /agri/fmacitm-zzctime.
  SELECTION-SCREEN END OF BLOCK b1.
