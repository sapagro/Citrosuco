************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PRODUCTIVITY_INDX_TOP                      *
* Tcode          : ZABS_RBREAK                                         *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 10.14.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : To specify the requirement for the creation of      *
*                  “Report for Comparison of Productivity Index vs     *
*                  Reasons for Loss/Stops (Breaks)”                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Tables
TABLES: /agri/glflcma,
        /agri/fmacitm.

*-- Types
TYPES: BEGIN OF ty_idactvl,
         idactvl TYPE /agri/fmidactl,
       END OF ty_idactvl,
       tty_idactvl TYPE STANDARD TABLE OF ty_idactvl.

TYPES: BEGIN OF ty_plant_wc,
         werks TYPE werks_d,
         arbpl TYPE arbpl,
       END OF ty_plant_wc.

TYPES: BEGIN OF ty_tplnr,
         tplnr TYPE /agri/gltplnr_fl,
         pltxt TYPE pltxt,
       END OF ty_tplnr.

TYPES: BEGIN OF ty_breaks,
         acppg  TYPE zfmacppg,
         idactv TYPE /agri/fmidact,
         period TYPE zfmacperiod,
         acdes  TYPE zfmacdsp,
         acprt  TYPE zfmacprt,
       END OF ty_breaks.

*-- Global data
DATA: gt_prod_indx TYPE STANDARD TABLE OF zabs_str_prod_indx,
*-- Global Objects
      gobj_cont    TYPE REF TO cl_gui_custom_container,
      gobj_grid    TYPE REF TO cl_gui_alv_grid.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE /agri/glflcma-iwerk.
SELECT-OPTIONS: so_date  FOR sy-datum NO-EXTENSION,
                so_accom FOR /agri/fmacitm-accom,
                so_tplnr FOR /agri/glflcma-tplnr_fl,
                so_matnr FOR /agri/fmacitm-tmatnr,
                so_aufnr FOR /agri/fmacitm-aufnr,
                so_arbpl FOR /agri/fmacitm-arbpl.
SELECTION-SCREEN END OF BLOCK b1.
