************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRRORD_INFO                            *
* Tcode             :  ZABS_TRN_IRRord_info                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.04.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Order Information                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_irrord_info.

*--Global data declarations
INCLUDE zabs_rep_irrord_info_top.

*--Processing data
INCLUDE zabs_rep_irrord_info_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN                                             *
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN Validations                                       *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-low.
*--Search Help for Characteristics
  PERFORM f4_for_shift CHANGING so_shift-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-high.
*--Search Help for Characteristics
  PERFORM f4_for_shift CHANGING SO_shift-high.

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
*& Preparing Irrigation Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Refresh Global Data
  PERFORM refresh_global_data.

*--Preparing Irrigation Data
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Irrigation Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Irrigation Data
  PERFORM display_data.
