************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_EMP_ROLE                                   *
* Tcode          : ZABS_TRN_EMPROLE                                    *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 05.26.2020                                          *
* TR             : C4DK914465                                          *
* Version        : 001                                                 *
* Description    : Employees Role                                      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_emp_role.

*--Global data declarations
INCLUDE zabs_rep_emp_role_top.

*--Processing data
INCLUDE zabs_rep_emp_role_sub.

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
