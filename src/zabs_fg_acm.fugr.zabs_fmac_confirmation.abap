FUNCTION zabs_fmac_confirmation.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_BAGGP) TYPE  ZABS_DEL_BAGGP OPTIONAL
*"     REFERENCE(IS_FMACDOC) TYPE  /AGRI/S_FMACS_DOC
*"     REFERENCE(IV_MCNF) TYPE  XFELD DEFAULT ''
*"  EXPORTING
*"     REFERENCE(ES_FMAC_DOC) TYPE  /AGRI/S_FMACS_DOC
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

*-- Local Declarations
  DATA: lt_messages TYPE /agri/t_gprolog,
        ls_messages TYPE /agri/s_gprolog,
        lv_subrc    TYPE sysubrc.

*-- Field Symbols
  FIELD-SYMBOLS: <fs_fmac_doc>  TYPE /agri/s_fmacs_doc,
                 <fs_initiator> TYPE /agri/gdescr.

  ASSIGN ('(SAPLZFG_ACM)GS_VARIABLES-ERRORS') TO FIELD-SYMBOL(<fs_error>).
  IF sy-subrc EQ 0.
    CLEAR <fs_error>.
  ENDIF.

  PERFORM transaction_init IN PROGRAM saplzfg_acm IF FOUND USING 'V'.

*  PERFORM document_infocus_lock USING is_fmacdoc-x-achdr-accom
*                             CHANGING lv_subrc
*                                      ls_messages.
*  IF lv_subrc NE 0.
*    APPEND ls_messages TO et_messages.
*    EXIT.
*  ENDIF.

  ASSIGN ('(SAPLZFG_ACM)GS_ACDOC_INFOCUS') TO <fs_fmac_doc>.
  IF sy-subrc EQ 0 AND <fs_fmac_doc> IS ASSIGNED.
    <fs_fmac_doc> = is_fmacdoc.

    IF iv_mcnf IS INITIAL.
*-- Confirm the items only ZZCONFM is initial
      LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>)
                                        WHERE zzbaggp NE iv_baggp.
        <fs_acitm>-zzconfm = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT <fs_fmac_doc>-x-acitm ASSIGNING <fs_acitm>
                                        WHERE status EQ 'CNF'.
        <fs_acitm>-zzconfm = abap_true.
      ENDLOOP.
    ENDIF.

    ASSIGN ('(SAPLZFG_ACM)GS_VARIABLES-INITIATOR') TO <fs_initiator>.
    IF sy-subrc EQ 0.
*-- Accomplishment Confirmations
      <fs_initiator> = 'POST'.
      PERFORM messages_initialize IN PROGRAM saplzfg_acm IF FOUND
                        USING <fs_initiator>
                              'SAVE'
                              <fs_fmac_doc>-x-achdr.

      PERFORM confirmation_data_check IN PROGRAM saplzfg_acm IF FOUND
                                         CHANGING lv_subrc.

      PERFORM confirmation_infocus_save IN PROGRAM saplzfg_acm IF FOUND
                                           USING abap_true
                                        CHANGING lv_subrc.

      REFRESH lt_messages.
      PERFORM messages_get IN PROGRAM saplzfg_acm IF FOUND
                   USING <fs_initiator>
                   CHANGING lt_messages.
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.
  ENDIF.

ENDFUNCTION.
