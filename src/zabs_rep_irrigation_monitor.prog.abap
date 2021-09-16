************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRRIGATION_MONITOR                     *
* Tcode             :  ZABS_TRN_IRRMON                                 *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  01.24.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Monitor                              *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_irrigation_monitor.

*--Global data declarations
INCLUDE zabs_rep_irr_monitor_top.

*--Processing data
INCLUDE zabs_rep_irr_monitor_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN                                             *
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN Validations                                       *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-low.
*--Search Help for Characteristics
  PERFORM f4_for_shift USING abap_true.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-high.
*--Search Help for Characteristics
  PERFORM f4_for_shift USING abap_false.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Irrigation Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Selection Screen Validations
  PERFORM selection_validations.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Preparing Irrigation Data
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Irrigation Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

  IF gt_final[] IS NOT INITIAL.
*--Display Irrigation Data
    PERFORM display_data.
  ELSE.
*--Não existem registros para os parâmetros informados!
    MESSAGE i114(zfmfp).
  ENDIF.
