************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_CLASSREC_TOP                           *
* Tcode          : ZABS_TRN_HCMCLASS                                   *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Daniele Janes                                       *
* Created on     : 30.03.2020                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Create job with daily execution to read class       *
*                  records in HCM and update FARM class records        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*----------------------------------------------------------------------*
*                   GLOBAL DATA                                        *
*----------------------------------------------------------------------*
DATA : gt_final  TYPE TABLE OF zabs_str_classrec,
       gobj_cont TYPE REF TO cl_gui_custom_container,
       gobj_grid TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
*                   SELECTION-SCREEN                                   *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS: p_date TYPE datum DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF block b1.
