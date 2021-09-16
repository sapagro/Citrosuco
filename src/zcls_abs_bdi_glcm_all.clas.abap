class ZCLS_ABS_BDI_GLCM_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_GLCM_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCLS_ABS_BDI_GLCM_ALL IMPLEMENTATION.


METHOD /agri/if_ex_badi_glcm_all~after_save.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~authority_check.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~before_save.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~document_check.

  DATA : ls_msg    TYPE /irm/s_gprolog,
         lv_cnval1 TYPE zabs_del_cnval.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = 'NRCM'
      iv_k1val  = 'FLDTY'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'NURS'

  IF flt_val EQ lv_cnval1 AND
     cs_cmhdr-zzmpgrp IS INITIAL.
    c_stop_save  = abap_true.
    ls_msg-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
    ls_msg-msgno = '161'.
    ls_msg-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    APPEND ls_msg TO ct_messages.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~header_data_fill.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~header_defaults_fill.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~status_flow_outcome_set.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~status_flow_step_set.
ENDMETHOD.


METHOD /agri/if_ex_badi_glcm_all~status_profile_trigger_set.
ENDMETHOD.
ENDCLASS.
