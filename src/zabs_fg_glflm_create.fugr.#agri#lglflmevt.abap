*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLMEVT
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF SCREEN 0099.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-045.
PARAMETERS: p_mpgrp TYPE /agri/glmdhdr-mpgrp.
SELECT-OPTIONS: so_mdate  FOR /agri/glmdhdr-mdate.
SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN: END OF SCREEN 0099.

AT SELECTION-SCREEN OUTPUT.

  SET TITLEBAR 'T204'.

AT SELECTION-SCREEN.
  IF p_mpgrp IS INITIAL.
    MESSAGE ID '/AGRI/GLMD' TYPE 'E' NUMBER '009'.
  ENDIF.
