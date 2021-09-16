*&---------------------------------------------------------------------*
*&      Form  ICON_TEXT_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM icon_text_prepare  USING lv_icon_text TYPE val_text
                     CHANGING lv_actual_tab.
****Prepare tab button with icon, text and tooltip

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = lv_actual_tab
      text                  = lv_icon_text
      info                  = lv_icon_text
      add_stdinf            = 'X'
    IMPORTING
      result                = lv_actual_tab
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ICON_TEXT_PREPARE
