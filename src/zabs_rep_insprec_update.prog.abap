*&---------------------------------------------------------------------*
*& Report ZABS_REP_INSPREC_UPDATE
*&---------------------------------------------------------------------*

REPORT zabs_rep_insprec_update.

*--Global data declarations
INCLUDE zabs_rep_insprec_update_top.

*--Processing data
INCLUDE zabs_rep_insprec_update_sub.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Inspection Recording Update
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Update Inspection Recording Update
  PERFORM insprec_update.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display data
  IF gt_str_qcharmlog IS NOT INITIAL.
*    AND p_par IS INITIAL.
    PERFORM display_data.
  ENDIF.
