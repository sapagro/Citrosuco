************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_INTEGRATION                            *
* Tcode          : ZABS_COL_HCM                                        *
* Created By     : Helio Kababe                                        *
* Requested by   : Daniele Janes                                       *
* Created on     : 16.04.2021                                          *
* TR             :                                                     *
* Version        : 001                                                 *
* Description    : Update productions and events into HCM Info type    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_integra_hcm.

*-- Global data declarations
INCLUDE zabs_inc_integra_hcm_top.

*-- Processing data
INCLUDE zabs_inc_integra_hcm_sub.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM fill_default_values.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Check obligatory parameters
  PERFORM check_obligatory_parameters.

*-- Fetch productions and events from Accomplishments
  PERFORM fetch_prod_events_for_hcm.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*-- Display the output
  PERFORM display_output.
