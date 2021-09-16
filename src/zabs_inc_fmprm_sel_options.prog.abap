*&---------------------------------------------------------------------*
*& Include ZABS_INC_FMPRM_SEL_OPTIONS
*&---------------------------------------------------------------------*

DATA: lv_zplate TYPE /agri/fmprhdr-lic_plate.

SELECTION-SCREEN BEGIN OF BLOCK z1 WITH FRAME TITLE z1_text.
SELECT-OPTIONS: s_zplate FOR lv_zplate.
SELECTION-SCREEN END OF BLOCK z1.
