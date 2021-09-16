*----------------------------------------------------------------------*
***INCLUDE LZFMNTXF0C.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CAVAL_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_CAVAL
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM caval_check  USING    i_caval TYPE zfmprcavame
                  CHANGING lv_subrc TYPE int4.

FIELD-SYMBOLS: <target> TYPE ANY TABLE.
  SELECT *
  FROM /sapyl/yo_item
  INTO CORRESPONDING FIELDS OF TABLE <target>
                 BYPASSING BUFFER
                 WHERE ( ldap_code  EQ 'AVR' )
                 AND ( means_of_transp EQ  i_caval ).
   MOVE sy-subrc to lv_subrc.
ENDFORM.
