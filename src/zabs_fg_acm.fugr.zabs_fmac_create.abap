FUNCTION zabs_fmac_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_FMACDOC) TYPE  /AGRI/S_FMACS_DOC
*"     REFERENCE(IV_MCNF) TYPE  XFELD DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(ES_FMAC_DOC) TYPE  /AGRI/S_FMACS_DOC
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

*-- Local Declarations
  DATA: lv_stop_save,
        lt_acdoc     TYPE /agri/t_fmacs_doc,
        lt_messages  TYPE /agri/t_gprolog,
        ls_messages  TYPE /agri/s_gprolog,
        lv_subrc     TYPE sysubrc,
        ref_text     TYPE REF TO /agri/cl_gtext_process,
        ls_details   TYPE ty_details,
        lt_details   TYPE STANDARD TABLE OF ty_details,
        ls_item      TYPE /agri/s_fmacitm_layout,
        lt_acm_ord   TYPE STANDARD TABLE OF zabs_acm_ord.

*-- Field Symbols
  FIELD-SYMBOLS: <fs_fmac_doc>  TYPE /agri/s_fmacs_doc,
                 <fs_initiator> TYPE /agri/gdescr,
                 <fs_details>   TYPE STANDARD TABLE.

  PERFORM transaction_init IN PROGRAM saplzfg_acm IF FOUND USING 'V'.

  ASSIGN ('(SAPLZFG_ACM)GS_ACDOC_INFOCUS') TO <fs_fmac_doc>.
  IF sy-subrc EQ 0 AND <fs_fmac_doc> IS ASSIGNED.
    <fs_fmac_doc> = is_fmacdoc.
    ASSIGN ('(SAPLZFG_ACM)GS_VARIABLES-INITIATOR') TO <fs_initiator>.
    IF sy-subrc EQ 0.
*-- Acomplishment Create
      <fs_initiator> = 'SAVE'.
      PERFORM messages_initialize IN PROGRAM saplzfg_acm IF FOUND
                        USING <fs_initiator>
                              'SAVE'
                              <fs_fmac_doc>-x-achdr.

      ASSIGN ('(SAPLZFG_ACM)GT_DETAILS') TO <fs_details>.
      IF sy-subrc EQ 0 AND <fs_details> IS ASSIGNED.
        LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>)
                  WHERE updkz eq zcl_abs_abap_maintain=>c_updkz_insert.

          CLEAR ls_item.
          MOVE-CORRESPONDING <fs_acitm> TO ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'TPLNR'
                                     CHANGING ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'TMATNR'
                                     CHANGING ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'IDRESOURCE'
                                     CHANGING ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'EQUNR'
                                     CHANGING ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'STRTDAT'
                                     CHANGING ls_item.

          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'AUFNR'
                                     CHANGING ls_item.

          ls_item-idactvl = <fs_acitm>-idactvl.
          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'IDACTVL'
                                     CHANGING ls_item.

          ls_item-idactve = <fs_acitm>-idactve.
          PERFORM item_details_get IN PROGRAM saplzfg_acm IF FOUND
                                        USING 'IDACTVE'
                                     CHANGING ls_item.

          IF iv_mcnf IS INITIAL.
            ls_item-menge = <fs_acitm>-menge.
          ENDIF.

          MOVE-CORRESPONDING ls_item TO <fs_acitm>.
        ENDLOOP.

        IF iv_mcnf IS INITIAL.
          IF <fs_fmac_doc>-x-achdr-updkz EQ zcl_abs_abap_maintain=>c_insert.
            READ TABLE <fs_fmac_doc>-x-acitm ASSIGNING <fs_acitm> INDEX 1.
            IF sy-subrc EQ 0.
              CLEAR ls_details.
              ls_details-aufnr    = <fs_acitm>-aufnr.
              ls_details-matnr    = <fs_acitm>-tmatnr.
              ls_details-tplnr_fl = <fs_acitm>-tplnr.
              SELECT SINGLE gamng gmein
                INTO (ls_details-menge, ls_details-meins)
                FROM /agri/fmfphdr
               WHERE aufnr EQ <fs_acitm>-aufnr.
*          ls_details-meins    = <fs_acitm>-qmein.
*          ls_details-menge    = <fs_acitm>-menge.
              APPEND ls_details TO lt_details.
            ENDIF.
          ENDIF.
        ELSE.
          IF <fs_fmac_doc>-x-achdr-updkz EQ zcl_abs_abap_maintain=>c_insert.
            LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING <fs_acitm>
                         WHERE updkz EQ zcl_abs_abap_maintain=>c_insert.
              READ TABLE lt_details TRANSPORTING NO FIELDS
                    WITH KEY aufnr = <fs_acitm>-aufnr.
              IF sy-subrc NE 0.
                CLEAR ls_details.
                ls_details-aufnr    = <fs_acitm>-aufnr.
                ls_details-matnr    = <fs_acitm>-tmatnr.
                ls_details-tplnr_fl = <fs_acitm>-tplnr.
                SELECT SINGLE gamng gmein
                  INTO (ls_details-menge, ls_details-meins)
                  FROM /agri/fmfphdr
                 WHERE aufnr EQ <fs_acitm>-aufnr.
*          ls_details-meins    = <fs_acitm>-qmein.
*          ls_details-menge    = <fs_acitm>-menge.
                APPEND ls_details TO lt_details.
              ENDIF.
            ENDLOOP.
          ELSE.
            CALL FUNCTION 'ZABS_FM_ORD_GET'
              EXPORTING
                iv_accom  = <fs_fmac_doc>-x-achdr-accom
              TABLES
                t_acm_ord = lt_acm_ord.

            LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING <fs_acitm>
                         WHERE updkz EQ zcl_abs_abap_maintain=>c_insert.
              READ TABLE lt_acm_ord TRANSPORTING NO FIELDS
                    WITH KEY aufnr = <fs_acitm>-aufnr.
              IF sy-subrc NE 0.
                READ TABLE lt_details TRANSPORTING NO FIELDS
                      WITH KEY aufnr = <fs_acitm>-aufnr.
                IF sy-subrc NE 0.
                  CLEAR ls_details.
                  ls_details-aufnr    = <fs_acitm>-aufnr.
                  ls_details-matnr    = <fs_acitm>-tmatnr.
                  ls_details-tplnr_fl = <fs_acitm>-tplnr.
                  SELECT SINGLE gamng gmein
                    INTO (ls_details-menge, ls_details-meins)
                    FROM /agri/fmfphdr
                   WHERE aufnr EQ <fs_acitm>-aufnr.
*          ls_details-meins    = <fs_acitm>-qmein.
*          ls_details-menge    = <fs_acitm>-menge.
                  APPEND ls_details TO lt_details.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        <fs_details> = lt_details.
      ENDIF.

      REFRESH lt_messages.
      PERFORM messages_get IN PROGRAM saplzfg_acm IF FOUND
                           USING <fs_initiator>
                           CHANGING lt_messages.
      APPEND LINES OF lt_messages TO et_messages.

      READ TABLE lt_messages TRANSPORTING NO FIELDS
            WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error.
      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.

*--call badi Agreement check
      PERFORM badi_document_check IN PROGRAM saplzfg_acm IF FOUND
                                  USING <fs_fmac_doc>
                               CHANGING lv_stop_save.

      IF lv_stop_save EQ abap_true.
        REFRESH lt_messages.
        PERFORM messages_get IN PROGRAM saplzfg_acm IF FOUND
                             USING <fs_initiator>
                             CHANGING lt_messages.
        APPEND LINES OF lt_messages TO et_messages.
        EXIT.
      ENDIF.

      APPEND <fs_fmac_doc> TO lt_acdoc.

      CALL FUNCTION '/AGRI/FMAC_SAVE'
        EXPORTING
          iref_text   = ref_text
        CHANGING
          ct_acdoc    = lt_acdoc
          ct_messages = lt_messages
        EXCEPTIONS
          no_change   = 1
          OTHERS      = 2.

      lv_subrc  = sy-subrc.

      APPEND LINES OF lt_messages TO et_messages.

      READ TABLE lt_acdoc INTO <fs_fmac_doc> INDEX 1.
      IF <fs_fmac_doc> IS ASSIGNED.
        CLEAR <fs_fmac_doc>-updkz.
        CLEAR <fs_fmac_doc>-x-achdr-updkz.
        LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING <fs_acitm>.
          CLEAR <fs_acitm>-updkz.
        ENDLOOP.
        es_fmac_doc = <fs_fmac_doc>.
      ENDIF.

      IF lv_subrc NE 0.
        IF lv_subrc EQ 1.
          CLEAR ls_messages.
          ls_messages-msgid = '/AGRI/GLOBAL'.
          ls_messages-msgno = '322'.
          ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
          APPEND ls_messages TO et_messages.
        ELSE.
          CLEAR ls_messages.
          ls_messages-msgid = '/AGRI/FMAC'.
          ls_messages-msgno = '001'.
          ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
          ls_messages-msgv1 = <fs_fmac_doc>-accom.
          APPEND ls_messages TO et_messages.
        ENDIF.
        EXIT.
      ELSE.
        PERFORM accom_tskord_save IN PROGRAM saplzfg_acm IF FOUND.
        CLEAR ls_messages.
        ls_messages-msgid = '/AGRI/FMAC'.
        ls_messages-msgno = '007'.
        ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
        ls_messages-msgv1 = <fs_fmac_doc>-accom.
        APPEND ls_messages TO et_messages.

        REFRESH lt_messages.
        PERFORM messages_get IN PROGRAM saplzfg_acm IF FOUND
                     USING <fs_initiator>
                     CHANGING lt_messages.
        APPEND LINES OF lt_messages TO et_messages.
      ENDIF.

**-- Accomplishment Confirmations
*      <fs_initiator> = 'POST'.
*      PERFORM messages_initialize IN PROGRAM saplzfg_acm IF FOUND
*                        USING <fs_initiator>
*                              'SAVE'
*                              <fs_fmac_doc>-x-achdr.
*
*      PERFORM confirmation_data_check IN PROGRAM saplzfg_acm IF FOUND
*                                         CHANGING lv_subrc.
*
*      PERFORM confirmation_infocus_save IN PROGRAM saplzfg_acm IF FOUND
*                                           USING abap_true
*                                        CHANGING lv_subrc.
*
*      REFRESH lt_messages.
*      PERFORM messages_get IN PROGRAM saplzfg_acm IF FOUND
*                   USING <fs_initiator>
*                   CHANGING lt_messages.
*      APPEND LINES OF lt_messages TO et_messages.

    ENDIF.
  ENDIF.

ENDFUNCTION.
