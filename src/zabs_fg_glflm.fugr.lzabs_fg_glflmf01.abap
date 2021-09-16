*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_GLFLMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_lock  USING lv_tplnr    TYPE /agri/gltplnr_fl
                                  lv_strno    TYPE /agri/glstrno
                                  lv_message_type
                         CHANGING lt_messages TYPE /agri/t_gprolog
                                  lv_subrc    TYPE sy-subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  IF lv_tplnr IS NOT INITIAL.
    CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFL'
      EXPORTING
        tplnr_fl       = lv_tplnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc IS NOT INITIAL.
      lv_subrc = sy-subrc.
      lv_msgv1 = sy-msgv1.
      MESSAGE ID '/AGRI/GLFL' TYPE lv_message_type
        NUMBER '011' WITH lv_tplnr lv_msgv1 INTO sy-msgli.
      PERFORM save_standard_message CHANGING lt_messages.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFLS'
    EXPORTING
      strno          = lv_strno
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    MESSAGE ID '/AGRI/GLFL' TYPE lv_message_type
      NUMBER '011' WITH lv_strno lv_msgv1 INTO sy-msgli.
    PERFORM save_standard_message CHANGING lt_messages.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*& Form SAVE_STANDARD_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_MESSAGES
*&---------------------------------------------------------------------*
FORM save_standard_message CHANGING lt_messages TYPE /agri/t_gprolog.

  INSERT INITIAL LINE INTO TABLE lt_messages
    ASSIGNING FIELD-SYMBOL(<lwa_message>).
  IF sy-subrc EQ 0.
    <lwa_message>-msgid = sy-msgid.
    <lwa_message>-msgno = sy-msgno.
    <lwa_message>-msgty = sy-msgty.
    <lwa_message>-msgv1 = sy-msgv1.
    <lwa_message>-msgv2 = sy-msgv2.
    <lwa_message>-msgv3 = sy-msgv3.
    <lwa_message>-msgv4 = sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  document_infocus_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock  USING lv_tplnr TYPE /agri/gltplnr_fl
                                    lv_strno TYPE /agri/glstrno.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLFL'
    EXPORTING
      tplnr_fl = lv_tplnr.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLFLS'
    EXPORTING
      strno = lv_strno.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
