************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_ACCOM_TBL_DATA_TOP                     *
* Tcode             :  ZABS_TRN_ACDIS                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  24.09.2020                                      *
* TR                :  C4DK924087                                      *
* Version           :  001                                             *
* Description       :  Accomplishment Creation Data Display Report     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

TABLES: zabs_bgp_accom.

*--Data Declaration
DATA: gt_accom TYPE STANDARD TABLE OF zabs_bgp_accom,
      lv_mac   TYPE zabs_del_mackey,
      lv_pos   TYPE /agri/glposnr,
      lv_fpo   TYPE zabs_del_fposnr,
      lv_wer   TYPE werks_d,
      lv_tpl   TYPE /agri/gltplnr_fl,
      lv_zhd   TYPE zabs_del_hdate,
      lv_stt   TYPE /agri/fmastrttim,
      lv_zbg   TYPE zabs_del_baggp,
      lv_zac   TYPE zabs_del_actcg,
      lv_ida   TYPE /agri/fmidactl,
      lv_idr   TYPE /agri/fmidrsc,
      lv_mob   TYPE xubname,
      lv_bad   TYPE zabs_del_badge,
      lv_sta   TYPE zabs_del_status,
      lv_zcd   TYPE erdat.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_mac   FOR lv_mac,
*               s_pos   FOR lv_pos,
                s_fpo   FOR lv_fpo,
                s_wer   FOR lv_wer,
                s_tpl   FOR lv_tpl,
                s_zhd   FOR lv_zhd,
                s_stt   FOR lv_stt,
                s_zbg   FOR lv_zbg,
                s_zac   FOR lv_zac,
                s_ida   FOR lv_ida,
                s_idr   FOR lv_idr,
                s_mob   FOR lv_mob,
                s_bad   FOR lv_bad,
                s_sta   FOR lv_sta,
                s_zcd   FOR lv_zcd,
                s_erdat FOR zabs_bgp_accom-erdat,
                s_aedat FOR zabs_bgp_accom-aedat.
SELECTION-SCREEN:END OF BLOCK b1.
