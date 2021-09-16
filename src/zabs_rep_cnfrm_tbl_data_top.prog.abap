************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_CNFRM_TBL_DATA_TOP                     *
* Tcode             :  ZABS_TRN_BGDIS                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  24.09.2020                                      *
* TR                :  C4DK924087                                      *
* Version           :  001                                             *
* Description       :  Accomplishment Confirmation Data Display Report *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Data Declaration
DATA: gt_cnfrm TYPE STANDARD TABLE OF zabs_str_cnfrm,
      lv_mac TYPE zabs_del_mackey,
      lv_bgp TYPE zabs_del_baggp,
      lv_ldc TYPE zabs_del_ldcde,
      lv_ers TYPE zabs_del_hdate,
      lv_lod TYPE zabs_del_load,
      lv_wer TYPE werks_d,
      lv_tpl TYPE /agri/gltplnr_fl,
      lv_auf TYPE aufnr,
      lv_lid TYPE persno,
      lv_btc TYPE zabs_del_btcrt,
      lv_crb TYPE zabs_del_badge,
      lv_chb TYPE zabs_del_badge,
      lv_sta TYPE zabs_del_status,
      lv_erd TYPE erdat,
      lv_rdt TYPE zabs_del_rdate,
      lv_ern TYPE ernam.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:s_mac FOR lv_mac,
               s_bgp FOR lv_bgp,
               s_ldc FOR lv_ldc,
               s_ers FOR lv_ers,
               s_lod FOR lv_lod,
               s_wer FOR lv_wer,
               s_tpl FOR lv_tpl,
               s_auf FOR lv_auf,
               s_lid FOR lv_lid,
               s_btc FOR lv_btc,
               s_crb FOR lv_crb,
               s_chb FOR lv_chb,
               s_sta FOR lv_sta,
               s_erd FOR lv_erd,
               s_rdt FOR lv_rdt,
               s_ern FOR lv_ern.
SELECTION-SCREEN:END OF BLOCK b1.
