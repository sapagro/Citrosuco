*----------------------------------------------------------------------*
***INCLUDE LZABS_QACHAR_GRPF01.
*----------------------------------------------------------------------*
FORM before_save.
  CONSTANTS: lc_action TYPE fieldname VALUE 'ACTION'.

  DATA: lt_qachargrp TYPE TABLE OF zabs_qachar_grp WITH KEY mstr_char.

  FIELD-SYMBOLS : <fs_field> TYPE any.

  SELECT SINGLE kurztext
    FROM qpmt
    INTO @DATA(lv_kurztext)
    WHERE sprache = @sy-langu
      AND mkmnr   = @zabs_vqachar_grp-mstr_char. "<fs_field>.
  IF sy-subrc = 0.
    zabs_vqachar_grp-mstr_desc = lv_kurztext.
  ENDIF.

ENDFORM.
