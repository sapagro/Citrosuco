************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_INTEGRATION                            *
* Tcode          : ZABS_PEHCM                                          *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Raphael                                             *
* Created on     : 10.30.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Update productions and events into HCM Info type    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_hcm_integration.

*-- Global data declarations
INCLUDE zabs_rep_hcm_integration_top.

*-- Processing data
INCLUDE zabs_rep_hcm_integration_sub.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Fetch productions and events from Accomplishments
  PERFORM fetch_prod_events_for_hcm.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*-- Update the productions and events in HCM
  PERFORM updated_hcm_infotypes.

*-- Display the output
  PERFORM display_output.
