************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_APROVA_DOC_EMAIL                           *
* Tcode          : ZABS_APROVA_DOC                                     *
* Created By     : Adonis Rossato                                      *
* Requested by   : Daniele Janes                                       *
* Created on     : 02.04.2021                                          *
* TR             :                                        *
* Version        : 001                                                 *
* Description    : Provide the list of measurement documents and       *
*                  send an e-mail for massapprovals                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_aprova_doc_email.

*-- Global data declarations
INCLUDE zabs_inc_aprova_doc_email_top.
*INCLUDE zabs_inc_aprova_doc_med_top.

*-- Processing data
INCLUDE zabs_inc_aprova_doc_email_f01.
*INCLUDE zabs_inc_aprova_doc_med_f01.

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

  gv_send_email = abap_false.
*-- Fetch the mesaurement data
  PERFORM get_measurement_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& DISPLAY_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM send_email.
