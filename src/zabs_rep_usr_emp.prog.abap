* Report Name    : ZABS_REP_USR_EMP                                    *
* Tcode          : ZABS_TRN_USREMP                                     *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 05.19.2020                                          *
* TR             : C4DK911368                                          *
* Version        : 001                                                 *
* Description    : Employees User                                      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

REPORT zabs_rep_usr_emp.

*--Global data declarations
INCLUDE zabs_rep_usr_emp_top.

*--Processing data
INCLUDE zabs_rep_usr_emp_sub.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Uploading User Employee Excel Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Preparing Excel data
  PERFORM get_excel_data.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display User Employee Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display User Employee Data
  PERFORM display_data.
