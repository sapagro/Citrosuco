************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_BGACCOM_TOP                            *
* Tcode             :  ZABS_TRN_BGACCOM                                *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  25.09.2020                                      *
* TR                :  C4DK925300                                      *
* Version           :  001                                             *
* Description       :  Accomplishment Confirmation Back Ground Program *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

TABLES : zabs_bgp_accom, zabs_ac_bggrp.

TYPES: BEGIN OF ty_mackey,
         zzmackey TYPE zabs_del_mackey,
       END OF ty_mackey,
       tt_mackey TYPE STANDARD TABLE OF ty_mackey.

*TYPES: BEGIN OF ty_duplicate_key,
*         zzmackey   TYPE zabs_del_mackey,
*         idresource TYPE /agri/fmidrsc,
*         zzhdate    TYPE zabs_del_hdate,
*         strttim    TYPE /agri/fmastrttim,
*       END OF ty_duplicate_key.

TYPES: BEGIN OF ty_joblog,
         jname  TYPE tbtcjob-jobname,       " Job Name
         jnumb  TYPE tbtcjob-jobcount,      " Job Number
         status TYPE tbtco-status,          " Job Status
         index  TYPE indx_srtfd,
       END OF ty_joblog,
       tt_joblog TYPE STANDARD TABLE OF ty_joblog.

**--Global table declarations
DATA :
*  gt_bgp_accelog  TYPE TABLE OF zabs_bgp_accelog,
*  gt_acbgrp_elog  TYPE TABLE OF zabs_acbgrp_elog,
  gt_str_accomlog    TYPE TABLE OF zabs_str_accommlog,
  gt_joblog          TYPE tt_joblog,
  gv_active_sessions TYPE i,
  gv_crt             TYPE char1,
  gv_cnf             TYPE char1.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_pro  RADIOBUTTON GROUP rg1 DEFAULT 'X' USER-COMMAND rad,
            p_rpro RADIOBUTTON GROUP rg1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 1.
PARAMETERS:p_acco  RADIOBUTTON GROUP rg2 MODIF ID k6 DEFAULT 'X' USER-COMMAND rad1.
SELECTION-SCREEN COMMENT 4(15) TEXT-009 FOR FIELD p_acco.
SELECTION-SCREEN POSITION 40.
PARAMETERS:p_cnfm RADIOBUTTON GROUP rg2 MODIF ID k7.
SELECTION-SCREEN COMMENT 44(15) TEXT-010 FOR FIELD p_cnfm.

SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: s_erdat  FOR zabs_bgp_accom-erdat DEFAULT sy-datum,
                s_mac    FOR zabs_bgp_accom-zzmackey MODIF ID k9,
*                s_pos    FOR zabs_bgp_accom-posnr MODIF ID k10,
                s_bggrp  FOR zabs_ac_bggrp-baggp MODIF ID k11.
PARAMETERS: p_par   TYPE c AS CHECKBOX USER-COMMAND xy,
            p_mac   TYPE zabs_tty_mackey NO-DISPLAY,
            p_acitm TYPE zabs_tty_bgp_accom NO-DISPLAY,
            p_accnf TYPE zabs_tty_ac_baggrp NO-DISPLAY,
            p_bgpro TYPE flag NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK bpp WITH FRAME TITLE TEXT-004.
PARAMETERS: p_psize  TYPE /irm/s_g_parallel_pro_ctrl-psize
                          DEFAULT '10' MODIF ID k1,
            p_maxsn  TYPE /irm/s_g_parallel_pro_ctrl-maxsn
                          DEFAULT '5' MODIF ID k2,
            p_jobpfx TYPE char11
                          DEFAULT 'ZABS_ACCOM' MODIF ID k3,
            p_rdate  TYPE sy-datum DEFAULT sy-datum MODIF ID k4,
            p_time   TYPE sy-uzeit DEFAULT sy-uzeit MODIF ID k5.
SELECTION-SCREEN END OF BLOCK bpp.
