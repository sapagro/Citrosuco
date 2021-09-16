************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_CNFRM_TBL_DATA_SUB                     *
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

*&---------------------------------------------------------------------*
*& Form FETCH_DATA
*&---------------------------------------------------------------------*
*& FETCH_DATA
*&---------------------------------------------------------------------*
FORM fetch_data.

  SELECT *
    FROM zabs_ac_bggrp
    INTO TABLE gt_cnfrm
   WHERE mackey      IN s_mac
     AND baggp       IN s_bgp
     AND ldcde       IN s_ldc
     AND loaded      IN s_lod
     AND ersda       IN s_ers
     AND aufnr       IN s_auf
     AND tplnr       IN s_tpl
     AND werks       IN s_wer
     AND lider_turma IN s_lid
     AND btcrt       IN s_btc
     AND crbadge     IN s_crb
     AND chbadge     IN s_chb
     AND status      IN s_sta
     AND rdate       IN s_rdt
     AND ernam       IN s_ern
     AND erdat       IN s_erd.
  IF sy-subrc NE 0.
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.

  TYPE-POOLS: slis.

  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        ls_layout TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZABS_STR_CNFRM'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT gt_cnfrm.
  DELETE ADJACENT DUPLICATES FROM gt_cnfrm COMPARING ALL FIELDS.

  IF lt_fcat IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = ls_layout
        it_fieldcat = lt_fcat
      TABLES
        t_outtab    = gt_cnfrm.
  ENDIF.

ENDFORM.
