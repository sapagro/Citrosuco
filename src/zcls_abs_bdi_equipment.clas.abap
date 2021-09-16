class ZCLS_ABS_BDI_EQUIPMENT definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_FMIR_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCLS_ABS_BDI_EQUIPMENT IMPLEMENTATION.


  method /AGRI/IF_EX_BADI_FMIR_ALL~AFTER_SAVE.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~AUTHORITY_CHECK.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~BEFORE_SAVE.
  endmethod.


  METHOD /agri/if_ex_badi_fmir_all~document_check.

    DATA : ls_msg    TYPE /irm/s_gprolog,
           ls_taba   TYPE dd07v,
           it_taba   TYPE STANDARD TABLE OF dd07v,
           it_tabb   TYPE STANDARD TABLE OF dd07v,
           lv_refcty TYPE domvalue_l.

    IF is_irhdr-irtyp = zcl_abs_abap_maintain=>c_irrtyp. "001
      IF is_irhdr-zzhydfac IS INITIAL.
        c_stop_save  = abap_true.
        ls_msg-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
        ls_msg-msgno = '124'.
        ls_msg-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
        APPEND ls_msg TO ct_messages.
      ENDIF.

      LOOP AT it_irflo INTO DATA(lwa_irflo) WHERE updkz IS NOT INITIAL.
        IF lwa_irflo-zztotal <> 100.
          c_stop_save  = abap_true.
          ls_msg-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
          ls_msg-msgno = '117'.
          ls_msg-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
          ls_msg-msgv1 = lwa_irflo-tplnr_fl.
          APPEND ls_msg TO ct_messages.
        ENDIF.
      ENDLOOP.

*      CALL FUNCTION 'DD_DOMA_GET'
*        EXPORTING
*          domain_name   = 'ZABS_DOM_REFCTY'
*          langu         = sy-langu
*          withtext      = abap_true
*        TABLES
*          dd07v_tab_a   = it_taba
*          dd07v_tab_n   = it_tabb
*        EXCEPTIONS
*          illegal_value = 1
*          op_failure    = 2
*          OTHERS        = 3.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      READ TABLE it_taba TRANSPORTING NO FIELDS
*      WITH KEY domvalue_l = lv_refcty.
*      IF sy-subrc <> 0.
*        c_stop_save  = abap_true.
*        ls_msg-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
*        ls_msg-msgno = '128'.
*        ls_msg-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*        APPEND ls_msg TO ct_messages.
*      ENDIF.

    ENDIF.

  ENDMETHOD.


  method /AGRI/IF_EX_BADI_FMIR_ALL~HEADER_DATA_FILL.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~HEADER_DEFAULTS_FILL.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~STATUS_FLOW_OUTCOME_SET.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~STATUS_FLOW_STEP_SET.
  endmethod.


  method /AGRI/IF_EX_BADI_FMIR_ALL~STATUS_PROFILE_TRIGGER_SET.
  endmethod.
ENDCLASS.
