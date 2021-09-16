*----------------------------------------------------------------------*
***INCLUDE LZABS_USRPERNRO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module ELEMENTS_IN_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE elements_in_display OUTPUT.

*  CLEAR sy-msgli.

  DATA: lv_route TYPE /agri/gl_route.

  CONSTANTS lc_memid TYPE c LENGTH 7 VALUE 'ZZROTID'.

  IMPORT lv_dummy TO lv_route from MEMORY ID lc_memid.

  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM /agri/glrthdr
      INTO @DATA(ls_glrthdr).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name EQ 'ZABS_V_USRPERNR-ROUTE'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*  IF zabs_usrpernr-owrol EQ 'VN'.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'ZABS_USRPERNR-PERNR'.
*        screen-input = 1.
*        MODIFY SCREEN.
*      ENDIF.
*      IF screen-name EQ 'ZABS_USRPERNR-LIFNR'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSEIF zabs_usrpernr-owrol EQ 'EN'.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'ZABS_USRPERNR-LIFNR'.
*        screen-input = 1.
*        MODIFY SCREEN.
*      ENDIF.
*      IF screen-name EQ 'ZABS_USRPERNR-PERNR'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDMODULE.
