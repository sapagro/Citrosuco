************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_RESOURCE_MAINTAIN                          *
* Tcode          : ZABS_FMRESOURCES                                    *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 09.18.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Resource Maintain                                   *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_resource_maintain.

*--Global data declarations
INCLUDE zabs_rep_resource_maintain_top.

*--Processing data
INCLUDE zabs_rep_resource_maintain_sub.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Maintain Resource Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

  IF r_rupl = abap_true.

*--Preparing Excel data
    PERFORM get_excel_data.

  ELSEIF r_rmain = abap_true.

*--Preparing Resource Data
    PERFORM get_resource_data.

  ENDIF.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Resource Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Resource Data
  PERFORM display_resource_data.
