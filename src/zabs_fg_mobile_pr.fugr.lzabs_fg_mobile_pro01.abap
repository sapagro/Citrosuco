*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_MOBILE_PRO01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set.
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module FCODE_PROCESSING INPUT
*&---------------------------------------------------------------------*
* Fcode Processing
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.            "FCODE_PROCESSING INPUT

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

*--Local variable declarations
*  DATA: lv_tcode       TYPE sy-ucomm,
*        lv_routine(32) TYPE c VALUE 'FCODE_'.

*  lv_tcode = sy-ucomm.
*  CLEAR ok_code.
*  CONCATENATE lv_routine lv_tcode INTO lv_routine.
*  PERFORM (lv_routine) IN PROGRAM (sy-repid) IF FOUND.

  IF sy-ucomm EQ 'CANC'.
    zzqrcode = gv_qrcode.
  ENDIF.
  LEAVE TO SCREEN 0.

ENDFORM.

*FORM fcode_cont.
*  LEAVE TO SCREEN 0.
*ENDFORM.
*
*FORM fcode_canc.
*  LEAVE TO SCREEN 0.
*ENDFORM.
