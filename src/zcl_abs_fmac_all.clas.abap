class ZCL_ABS_FMAC_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_FMAC_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_FMAC_ALL IMPLEMENTATION.


  METHOD /agri/if_ex_badi_fmac_all~after_save.

****Message Types
    CONSTANTS : BEGIN OF c_msg_type,
                  info    LIKE sy-msgty VALUE 'I',
                  warning LIKE sy-msgty VALUE 'W',
                  error   LIKE sy-msgty VALUE 'E',
                  abend   LIKE sy-msgty VALUE 'A',
                  success LIKE sy-msgty VALUE 'S',
                  x       LIKE sy-msgty VALUE 'X',
                END   OF c_msg_type.

****Update Indicators
    CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
               c_updkz_update(1)  TYPE c VALUE 'U',
               c_updkz_delete(1)  TYPE c VALUE 'D',
               c_updkz_old(1)     TYPE c VALUE ' ',
               c_updkz_newrow     TYPE c VALUE 'N',
               c_updkz_propose(1) TYPE c VALUE 'P'.

    IF i_updkz EQ c_updkz_new
    AND sy-binpt EQ abap_true
    AND is_achdr-accom IS NOT INITIAL.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '007'
        WITH is_achdr-accom.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmac_all~authority_check.
  ENDMETHOD.


METHOD /agri/if_ex_badi_fmac_all~before_save.

****Update Indicators
  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  IF i_updkz EQ zcl_abs_abap_maintain=>c_insert.
    CALL FUNCTION 'ZABS_FM_ORD_SAVE' "IN UPDATE TASK
      EXPORTING
        iv_accom = cs_xachdr-accom.
  ENDIF.

*-- Populate the Activity category in header to identify whether this
*-- accomplishment is for productions or events
  IF cs_xachdr-zzactcg IS INITIAL.
    LOOP AT ct_xacitm INTO DATA(ls_acitm) WHERE idactvl IS NOT INITIAL.
      SELECT SINGLE zzactcg
        FROM /agri/fmacact
        INTO cs_xachdr-zzactcg
       WHERE idactv EQ ls_acitm-idactvl.
      EXIT.
    ENDLOOP.
  ENDIF.

  IF sy-tcode EQ 'ZABS_FMACM'.
    LOOP AT ct_xacitm ASSIGNING FIELD-SYMBOL(<ls_acitm>).
      IF <ls_acitm>-updkz EQ c_updkz_new.
        <ls_acitm>-zzcdate = sy-datum.
        <ls_acitm>-zzctime = sy-uzeit.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_fmac_all~document_check.

  DATA: ls_messages TYPE /agri/s_gprolog.

  LOOP AT it_xacitm INTO DATA(ls_acitm).
    CLEAR ls_messages.
    IF ls_acitm-tplnr IS INITIAL.
      ls_messages-msgid = 'ZABS_MSGCLS'.
      ls_messages-msgno = '079'.
      ls_messages-msgty = 'E'.
      APPEND ls_messages TO ct_messages.
      c_stop_save = abap_true.
    ELSEIF ls_acitm-aufnr IS INITIAL.
      ls_messages-msgid = 'ZABS_MSGCLS'.
      ls_messages-msgno = '080'.
      ls_messages-msgty = 'E'.
      APPEND ls_messages TO ct_messages.
      c_stop_save = abap_true.
    ENDIF.

*    IF ls_acitm-zzbill EQ 'NO' AND
*       ls_acitm-zzactrn IS INITIAL.
*      ls_messages-msgid = 'ZABS_MSGCLS'.
*      ls_messages-msgno = '083'.
*      ls_messages-msgty = 'E'.
*      APPEND ls_messages TO ct_messages.
*      c_stop_save = abap_true.
*    ENDIF.

  ENDLOOP.

ENDMETHOD.


  method /AGRI/IF_EX_BADI_FMAC_ALL~HEADER_DATA_FILL.
  endmethod.


  METHOD /agri/if_ex_badi_fmac_all~header_defaults_fill.
  ENDMETHOD.


  method /AGRI/IF_EX_BADI_FMAC_ALL~STATUS_FLOW_OUTCOME_SET.
  endmethod.


  METHOD /agri/if_ex_badi_fmac_all~status_flow_step_set.
  ENDMETHOD.


  method /AGRI/IF_EX_BADI_FMAC_ALL~STATUS_PROFILE_TRIGGER_SET.
  endmethod.
ENDCLASS.
