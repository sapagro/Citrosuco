FUNCTION zabs_fm_order_mass_teco.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_AUFNR) TYPE  /AGRI/T_FMAUFNR
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

  DATA: lt_order      TYPE STANDARD TABLE OF bapi_order_key INITIAL SIZE 0,
        lt_messages   TYPE /agri/t_gprolog,
        lt_return_msg TYPE STANDARD TABLE OF bapi_order_return INITIAL SIZE 0,
        ls_return     TYPE bapiret2.

  LOOP AT it_aufnr INTO DATA(ls_aufnr).
    REFRESH: lt_order, lt_return_msg.
    CLEAR ls_return.
    INSERT INITIAL LINE INTO TABLE lt_order
      ASSIGNING FIELD-SYMBOL(<ls_order>).
    IF sy-subrc EQ 0.
      <ls_order>-order_number = ls_aufnr.
      CALL FUNCTION 'ZABS_PRODORD_COMPLETE_TECH'
        IMPORTING
          return        = ls_return
        TABLES
          orders        = lt_order
          detail_return = lt_return_msg.

      READ TABLE lt_return_msg TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

*-- Mensagem Encerramento Técnico Ordem &1
      IF lt_return_msg[] IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE lt_messages
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgty = 'I'.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '184'.
          <ls_message>-msgv1 = ls_aufnr.
        ENDIF.
      ENDIF.

      LOOP AT lt_return_msg INTO DATA(ls_return_msg).
        INSERT INITIAL LINE INTO TABLE lt_messages
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgty = ls_return_msg-type.
          <ls_message>-msgid = ls_return_msg-id.
          <ls_message>-msgno = ls_return_msg-number.
          <ls_message>-msgv1 = ls_return_msg-message_v1.
          <ls_message>-msgv2 = ls_return_msg-message_v2.
          <ls_message>-msgv3 = ls_return_msg-message_v3.
          <ls_message>-msgv4 = ls_return_msg-message_v4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  et_messages[] = lt_messages[].

ENDFUNCTION.
