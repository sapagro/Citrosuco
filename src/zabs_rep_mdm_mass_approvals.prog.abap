************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_MD_MASS_APPROVALS                          *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Raphael                                             *
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
REPORT zabs_rep_mdm_mass_approvals.

*-- Global data declarations
INCLUDE zabs_inc_mdm_mass_approval_top.

*-- Processing data
INCLUDE zabs_inc_mdm_mass_approval_f01.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
*-- Default Screen Values
  PERFORM set_default_values.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*-- Selection Screen Validations
  PERFORM selection_validations.

AT SELECTION-SCREEN OUTPUT.
*-- Upate Screen
  PERFORM update_screen.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*-- Validate Parameters
  PERFORM validate_parameters.
*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Authority Check
  PERFORM authority_check.
*-- Fetch the mesaurement data
  PERFORM get_measurement_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& DISPLAY_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_alcada EQ abap_false.
    IF <gfs_dyn_tmdm>[] IS INITIAL.
*--Não existem dados para os parâmetros informados
      MESSAGE i032(zfmfp) WITH TEXT-061.
      LEAVE LIST-PROCESSING.
    ELSE.
*-- Display Measurement Documents Data
      PERFORM display_measurement_data.
    ENDIF.
  ELSEIF p_alcada EQ abap_true.
*-- Defines the Status Flow according
*-- to the variation interval
    PERFORM update_status_flow.
  ENDIF.
