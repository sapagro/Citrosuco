************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_EMPREC                                 *
* Tcode          : ZABS_TRN_HCMEMP                                     *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Daniele Janes                                       *
* Created on     : 30.03.2020                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Create job with daily execution to read employee    *
*                  records in HCM and update FARM employee records     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_hcm_emprec.

*-- Global data declarations
INCLUDE zabs_rep_hcm_emprec_top.

*-- Processing data
INCLUDE zabs_rep_hcm_emprec_sub.

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
