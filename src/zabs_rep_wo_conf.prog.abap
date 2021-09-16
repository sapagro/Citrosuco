************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_WO_CONF                                *
* Tcode             :  ZABS_TRN_IORD_CNF                               *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  02.25.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  WorkOrder                                       *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_wo_conf.

*--Global data declarations
INCLUDE zabs_rep_wo_conf_top.

*--Processing data
INCLUDE zabs_rep_wo_conf_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN                                             *
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN Validations                                       *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_shift.

*--Search Help for Shift
  PERFORM f4_for_shift.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN                                             *
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN Validations                                       *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_equnr.

*--Search Help for Equipment
  PERFORM f4_for_equnr.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.
*
*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing WorkOrder Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Refresh Global data
  PERFORM refresh_global_data.

*--Preparing WorkOrder Conf Data
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Work Order Confirmation Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Work Order Confirmation Data
  PERFORM display_data.
