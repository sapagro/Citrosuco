************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_ACCOM_BGPROG_TOP                       *
* Tcode             :  ZABS_TRN_ACCOM                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  03.09.2020                                      *
* TR                :  C4DK924087                                      *
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

**--Global table declarations
DATA :
*  gt_bgp_accelog  TYPE TABLE OF zabs_bgp_accelog,
*  gt_acbgrp_elog  TYPE TABLE OF zabs_acbgrp_elog,
  gt_str_accomlog TYPE TABLE OF zabs_str_accommlog.

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
SELECTION-SCREEN END OF BLOCK b1.
