*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_ATTR_GROUP_FPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MASS_TECO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FMFPHDR
*&      --> C_CROP_SEASON_FORMACAO
*&      --> C_CROP_PROCESS_CLOSE_ALL
*&      --> LS_GLFLCMA
*&---------------------------------------------------------------------*
FORM mass_teco USING lt_fmfphdr      TYPE gy_fmfphdr_tab
                     lv_crop_season  TYPE /agri/glvaria
                     lv_crop_process TYPE /agri/glcpros
                     ls_glflcma      TYPE /agri/glflcma
            CHANGING lt_msg_teco     TYPE /agri/t_gprolog.

  DATA: lt_aufnr TYPE /agri/t_fmaufnr.

  DATA(lt_fmfphdr_aux) = lt_fmfphdr[].
  DELETE lt_fmfphdr_aux WHERE varia NE lv_crop_season
                           OR tplnr_fl NE ls_glflcma-tplnr_fl.

*-- Verificação Adicional: todas as ordens devem ser encerradas
  IF lv_crop_process EQ c_crop_process-close_all.
    DELETE lt_fmfphdr_aux WHERE tplnr_fl NE ls_glflcma-tplnr_fl
                             OR contr    NE ls_glflcma-contr
                             OR cmnum    NE ls_glflcma-cmnum
                             OR varia    NE ls_glflcma-varia
                             OR iwerk    NE ls_glflcma-iwerk
                             OR tecom    EQ abap_true.
  ELSE.
    DELETE lt_fmfphdr_aux WHERE tplnr_fl NE ls_glflcma-tplnr_fl
                             OR contr    NE ls_glflcma-contr
                             OR cmnum    NE ls_glflcma-cmnum
                             OR varia    NE ls_glflcma-varia
                             OR cpros    NE lv_crop_process
                             OR iwerk    NE ls_glflcma-iwerk
                             OR tecom    EQ abap_true.
  ENDIF.

  IF lt_fmfphdr_aux[] IS NOT INITIAL.
    lt_aufnr = CORRESPONDING #( lt_fmfphdr_aux ).
*-- Order Mass Technical Complete
    CALL FUNCTION 'ZABS_FM_ORDER_MASS_TECO'
      EXPORTING
        it_aufnr    = lt_aufnr
      IMPORTING
        et_messages = lt_msg_teco.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form YARD_TECH_COMPLETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_GLFLCMA_MANU1_YAUFNR
*&---------------------------------------------------------------------*
FORM yard_tech_complete USING lv_yaufnr   TYPE /agri/glyaufnr
                     CHANGING lt_msg_teco TYPE /agri/t_gprolog.

  DATA: lt_orders TYPE TABLE OF bapi_order_key,
        lt_return TYPE cocf_t_bapi_return,
        ls_status TYPE bapiret2.

*-- Encerra Ordem Pátio
  IF lv_yaufnr IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lt_orders
      ASSIGNING FIELD-SYMBOL(<ls_order>).
    IF sy-subrc EQ 0.
      <ls_order>-order_number = lv_yaufnr.
      CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
        IMPORTING
          return        = ls_status
        TABLES
          orders        = lt_orders
          detail_return = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*-- Ordem Pátio &1.
        INSERT INITIAL LINE INTO TABLE lt_msg_teco
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '375'.
          <ls_message>-msgty = 'S'.
          <ls_message>-msgv1 = lv_yaufnr.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      LOOP AT lt_return INTO DATA(ls_return).
        INSERT INITIAL LINE INTO TABLE lt_msg_teco
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = ls_return-id.
          <ls_message>-msgno = ls_return-number.
          <ls_message>-msgty = ls_return-type.
          <ls_message>-msgv1 = ls_return-message_v1.
          <ls_message>-msgv2 = ls_return-message_v2.
          <ls_message>-msgv3 = ls_return-message_v3.
          <ls_message>-msgv4 = ls_return-message_v4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
