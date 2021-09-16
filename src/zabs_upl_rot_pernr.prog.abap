************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_UPL_ROT_PERNR                                  *
* Tcode          : ZABS_TRN_ROTPERNR                                   *
* Created By     : John Anthony Obregón Lanas                          *
* Requested by   : Jonathan Rubiano                                    *
* Created on     : 05.26.2020                                          *
* TR             : C4DK914465                                          *
* Version        : 001                                                 *
* Description    : Routes and PErsonnel number association             *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_upl_rot_pernr.

*-- Macros for logs
INCLUDE zabs_gen_macros.
*--Global data declarations
INCLUDE zabs_upl_rot_pernr_top.
*INCLUDE zabs_rep_emp_role_top.

*--Processing data
INCLUDE zabs_upl_rot_pernr_sub.
*INCLUDE zabs_rep_emp_role_sub.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Uploading Employee Role Excel Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Preparing Excel data
  PERFORM get_excel_data.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Employee Role Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Employee Role Data
  PERFORM display_data.
