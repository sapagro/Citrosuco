************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_ACT_MAINTAIN                               *
* Tcode          : ZABS_TRN_QUAL_CHAR                                  *
* Created By     :                                                     *
* Requested by   :                                                     *
* Created on     : 02.24.2020                                          *
* TR             : C4DK908476                                          *
* Version        : 001                                                 *
* Description    : Quality Characteristics Calculations                *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_qual_charcs.

*--Global data declarations
INCLUDE zabs_rep_qual_charcs_top.

*--Processing data
INCLUDE zabs_rep_qual_charcs_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Selection Screen Validations
  PERFORM selection_validations.

*---------------------------------------------------------------------*
*Initialization
*---------------------------------------------------------------------*
INITIALIZATION.
*--Perform For Initialization.
  PERFORM initialization.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Calculation Characteristics Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*--Preparing Calculation Characteristics Data.
  go_qual->get_qual_charcs_data( ).

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Calculation Characteristics Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*--Display Calculation Characteristics Data.
  go_qual->display_qual_charcs_data( ).
