************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PRODUCTIVITY_INDEX                         *
* Tcode          : ZABS_RBREAK                                         *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 10.14.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : to specify the requirement for the creation of      *
*                  “Report for Comparison of Productivity Index vs     *
*                  Reasons for Loss/Stops (Breaks)”                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_productivity_index.

*-- Global data declarations
INCLUDE zabs_rep_productivity_indx_top.

*-- Processing data
INCLUDE zabs_rep_productivity_indx_sub.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*-- Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing the productivity data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Preparing the productivity analysis
  PERFORM get_productivity_data.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display the productivity analysis Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*-- Display the productivity analysis Data
  PERFORM display_productivity_data.
