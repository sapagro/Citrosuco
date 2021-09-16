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
DATA: gt_upl_hdr    TYPE TABLE OF zabs_str_rotpernr_xls,
      gt_upl_per    TYPE TABLE OF zabs_str_rot_xls,
      gc_messid     TYPE arbgb      VALUE 'ZABS_MSGCLS',
      gv_object     TYPE balobj_d   VALUE 'ZFMAC',
      gv_subobject  TYPE balsubobj  VALUE 'UPMASS',
      gv_log_handle TYPE balloghndl.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_path TYPE rlgrap-filename MODIF ID p,
             p_test TYPE flag DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*--File path
  PERFORM f4_filepath.
