*&---------------------------------------------------------------------*
*&  Include           ZABS_REP_TERRAIN_DETAILS_TOP
*&---------------------------------------------------------------------*
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TERRAIN_DETAILS_TOP                    *
* Tcode             :  ZABS_TRN_TERRAIN_DTL                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  11.06.2019                                      *
* TR                :  C4DK903782                                      *
* Version           :  002                                             *
* Description       :  Terrain Details global declaration and selection*
*                      screen                                          *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------*

*--Table declarations
TABLES: /agri/glflot, /agri/glflatv.

TYPES: BEGIN OF ty_farm_dtls,
         region     TYPE zabs_del_reg,
         farm       TYPE zabs_del_farm,
         pltxt      TYPE /agri/glpltxt,
         reason     TYPE zabs_del_reason,
         rsdesc     TYPE atwtb,
         qtd_plants TYPE zabs_del_plant,
       END OF ty_farm_dtls.

*--Table type declarations
TYPES: tty_terrain_dtls TYPE TABLE OF zabs_str_terrain_dtls.

*--Global table declarations
DATA: gt_terrain_dtls    TYPE tty_terrain_dtls,
      gt_farm_subtot     TYPE zabs_tty_farm_subtot,
      gt_att_content_hex TYPE solix_tab,
      gt_att_content     TYPE soli_tab.

*--Screen attribute declarations
DATA: gv_ok_code TYPE sy-ucomm,
      gv_langu   TYPE sy-langu,
      gv_datab   TYPE datab,
      gv_datbi   TYPE datbi,
      gv_rbtxt   TYPE /agri/gldescr,
*-- BOC-T_T.KONNO
      gv_subrc   TYPE sysubrc,
*-- EOC-T_T.KONNO
      gv_regtxt  TYPE atwtb.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_per TYPE char7 OBLIGATORY.
SELECT-OPTIONS: s_werks  FOR /agri/glflot-iwerk,
                s_region FOR /agri/glflatv-atwrt. " Wave 3 changes added
SELECT-OPTIONS: s_tplnr  FOR /agri/glflot-tplnr_fl.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_lep RADIOBUTTON GROUP rb1 USER-COMMAND xy DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(50) TEXT-014.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_lpp RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 3(50) TEXT-015.
SELECTION-SCREEN END OF LINE.
*-- BOC-T_T.KONNO
*PARAMETERS: p_path TYPE string OBLIGATORY.
PARAMETERS: p_path  TYPE string,
            p_print TYPE /agri/glflot-loevm NO-DISPLAY DEFAULT abap_true.
*-- EOC-T_T.KONNO
SELECTION-SCREEN END OF BLOCK b1.
