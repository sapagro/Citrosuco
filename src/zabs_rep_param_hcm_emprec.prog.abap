************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PARAM_HCM_EMPREC                           *
* Tcode          : ZABS_PARAM_HCMEMP                                   *
* Created By     : Adonis Rossato                                      *
* Requested by   : Mauricio J. Pereira                                 *
* Created on     : 12.07.2021                                          *
* Description    : Maitenance for ZABS_EMP_HCM_UPD parameter HCM table *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_param_hcm_emprec.

*-- Global data declarations
INCLUDE ZABS_REP_PARAM_HCM_EMPREC_TOP.
*INCLUDE zabs_rep_hcm_emprec_top.

*-- Processing data
INCLUDE ZABS_REP_PARAM_HCM_EMPREC_SUB.
*INCLUDE zabs_rep_hcm_emprec_sub.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Fetch Data
  PERFORM fetch_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*-- Display the output
  PERFORM display_output.
