*&---------------------------------------------------------------------*
*& Include          LZFG_ACMO01
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.                 " STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.                 " TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CONTROLS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.                 " CONTROLS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SUBSCREEN_AREA_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE subscreen_area_set OUTPUT.
  PERFORM subscreen_area_set.
ENDMODULE.                 " SUBSCREEN_AREA_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_modify OUTPUT.
  PERFORM screen_modify.
ENDMODULE.                 " SCREEN_MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_display OUTPUT.
  PERFORM header_data_display.
ENDMODULE.                 " HEADER_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DESCRIPTIONS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE descriptions_display OUTPUT.
  PERFORM descriptions_display USING space.
ENDMODULE.                 " DESCRIPTIONS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ITEMS_PREPARE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE items_prepare OUTPUT.
  PERFORM items_prepare.
ENDMODULE.                 " ITEMS_PREPARE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DETAILS_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE details_data_display OUTPUT.

  PERFORM details_data_display.
ENDMODULE.                 " DETAILS_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_UPDATE_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_update_1 OUTPUT.
  PERFORM header_data_update_1.
ENDMODULE.                 " HEADER_DATA_UPDATE_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ok_code_0010 INPUT.
  ok_code = sscrfields-ucomm.
ENDMODULE.                 " OK_CODE_0010  INPUT
