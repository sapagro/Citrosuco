*----------------------------------------------------------------------*
***INCLUDE LZABS_QACHAR_GRPI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MSTRCHAR_VAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mstrchar_val INPUT.

  CONSTANTS: lc_action TYPE fieldname VALUE 'ACTION'.

  DATA: lt_qachargrp TYPE TABLE OF zabs_qachar_grp WITH KEY mstr_char.

  FIELD-SYMBOLS : <fs_field> TYPE any.

  CHECK zabs_vqachar_grp-mstr_char IS NOT INITIAL.

  SELECT SINGLE kurztext
    FROM qpmt
    INTO @DATA(lv_kurztext)
    WHERE sprache = @sy-langu
      AND mkmnr   = @zabs_vqachar_grp-mstr_char. "<fs_field>.
  IF sy-subrc = 0.
    zabs_vqachar_grp-mstr_desc = lv_kurztext.
  ENDIF.
*  ENDIF.

ENDMODULE.
