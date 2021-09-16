************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TOCNF_BGPROG                           *
* Tcode             :  ZABS_TRN_TOMON                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  08.13.2020                                      *
* TR                :  ZABS_TRN_BGP_TOCNF                              *
* Version           :  001                                             *
* Description       :  Task Order Confirmation Back Ground Program     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_tocnf_bgprog.

*--Global data declarations
INCLUDE zabs_rep_tocnf_bgprog_top.

*--Processing data
INCLUDE zabs_rep_tocnf_bgprog_sub.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Task Order Confirm
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Task Order Confirm
  PERFORM Taskorder_conf.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display data
  IF gt_str_tocnfmlog IS NOT INITIAL.
*    AND p_par IS INITIAL.
    PERFORM display_data.
  ENDIF.
