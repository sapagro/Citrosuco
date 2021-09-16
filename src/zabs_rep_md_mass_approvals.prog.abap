************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_MD_MASS_APPROVALS                          *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Rapheal                                             *
* Created on     : 11.06.2019                                          *
* TR             : C4DK903886                                          *
* Version        : 001                                                 *
* Description    : Provide the list of measurement documents for mass  *
*                  approvals based the user who executes this program  *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_md_mass_approvals.

*-- Global data declarations
INCLUDE zabs_inc_md_mass_approvals_top.

*-- Processing data
INCLUDE zabs_inc_md_mass_approvals_f01.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN                                             *
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN Validations                                       *
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_etapa.

*--Search Help for Step
  PERFORM f4_for_etapa.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN value requests
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_region.

*--Search Help for Period
  PERFORM f4_for_region.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Fetch the mesaurement data
  PERFORM get_measurement_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& DISPLAY_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM display_measurement_data.
