************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_ACCOM_BGPROG                           *
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
REPORT zabs_rep_accom_bgprog.

*--Global data declarations
INCLUDE zabs_rep_accom_bgprog_top.

*--Processing data
INCLUDE zabs_rep_accom_bgprog_sub.

*&--------------------------------------------------------------------*
*&  AT SELECTION-SCREEN OUTPUT
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*--Screen validation
  PERFORM screen_modify.

*&--------------------------------------------------------------------*
*&  AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM screen_validations.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

*-- Processing
  IF p_pro IS NOT INITIAL.

    PERFORM process_accom_create_confirm.

*-- Reprocessing
  ELSEIF p_rpro IS NOT INITIAL
     AND p_acco IS NOT INITIAL.

    PERFORM reprocess_accom_create.

  ELSEIF p_rpro IS NOT INITIAL
     AND p_cnfm IS NOT INITIAL.

    PERFORM reprocess_accom_confirm.

  ENDIF.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*  WRITE TEXT-013.
*--Display data
  IF gt_str_accomlog IS NOT INITIAL.
    PERFORM display_data.
  ENDIF.
