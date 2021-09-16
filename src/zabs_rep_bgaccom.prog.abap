************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_BGACCOM                                *
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
REPORT zabs_rep_bgaccom.

*--Global data declarations
INCLUDE zabs_rep_bgaccom_top.

*--Processing data
INCLUDE zabs_rep_bgaccom_sub.

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
  IF p_pro IS NOT INITIAL
    AND p_par IS INITIAL.

    PERFORM process_accom_create_confirm.

*-- Reprocessing
  ELSEIF p_rpro IS NOT INITIAL
     AND p_acco IS NOT INITIAL
    AND p_par IS INITIAL.

    PERFORM reprocess_accom_create.

  ELSEIF p_rpro IS NOT INITIAL
     AND p_cnfm IS NOT INITIAL
    AND p_par IS INITIAL.

    PERFORM reprocess_accom_confirm.

*-- Parallel Processing
  ELSEIF p_par IS NOT INITIAL.
   PERFORM Backgrd_accom_create_confirm.

  ENDIF.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display data
  IF gt_str_accomlog IS NOT INITIAL.
    PERFORM display_data.
  ENDIF.
