************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_ACT_MAINTAIN                               *
* Tcode          : ZABS_FMACTIVITIES                                   *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.23.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Activities Maintain                                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_act_maintain.

*--Global data declarations
INCLUDE zabs_rep_act_maintain_top.

*--Processing data
INCLUDE zabs_rep_act_maintain_sub.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.
*
*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Activities Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

  IF r_aupl = abap_true.

*--Preparing Excel data
    PERFORM get_excel_data.

  ELSEIF r_amain = abap_true.

*--Build Activities Data
    PERFORM build_act_data.

  ENDIF.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Activities Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Activities Data
  PERFORM display_activity_data.
