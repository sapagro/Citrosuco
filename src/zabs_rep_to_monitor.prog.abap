************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TO_MONITOR                             *
* Tcode             :  ZABS_TRN_TOMON                                  *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.31.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Task Order Monitor                              *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_to_monitor.

*--Global data declarations
INCLUDE zabs_rep_to_monitor_top.

*--Processing data
INCLUDE zabs_rep_to_monitor_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Task Order Monitor Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Preparing Task Order Monitor Data
  PERFORM build_task_order_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Task Order Monitor Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Task Order Monitor Data
  PERFORM display_task_order_data.
