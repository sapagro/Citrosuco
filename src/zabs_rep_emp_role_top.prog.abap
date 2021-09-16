************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_EMP_ROLE_TOP                               *
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

*--Types declaartion
TYPES tty_taba  TYPE TABLE OF dd07v.

*--Global data declaration
DATA: gt_upl_act TYPE STANDARD TABLE OF
                    zabs_str_emprole_xls.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_path  TYPE rlgrap-filename MODIF ID p.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*--File path
  PERFORM f4_filepath.
