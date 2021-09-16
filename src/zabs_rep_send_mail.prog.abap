************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_SEND_MAIL                                  *
* Tcode          : ZABS_EMAIL_ESTIMATIV                                *                                         *
* Created By     : Helio Kababe                                        *
* Requested by   : Daniele Janes                                       *
* Created on     : 16.02.2021                                          *
* TR             : C4DK931754                                          *
* Version        : 001                                                 *
* Description    : Provide the list of measurement documents for mass  *
*                  approvals based the user who executes this program  *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_send_mail.

*-- Global data declarations
INCLUDE zabs_inc_send_mail_top.

*-- Processing data
INCLUDE zabs_inc_send_mail_f01.

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
*-- Send e-mail to approvers
  PERFORM send_email.
*-- Display Measurement Documents Data
  PERFORM display_measurement_data.
