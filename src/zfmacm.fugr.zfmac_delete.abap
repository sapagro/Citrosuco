FUNCTION zfmac_delete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CS_ACDOC) TYPE  ZSC_FMAC_DOC OPTIONAL
*"----------------------------------------------------------------------

  DATA : lwa_acitm TYPE zsc_fmacitm,
         lwa_acvlc TYPE zsc_fmacvlcl,
         lt_acdoc  TYPE zt_fmac_doc,
         lv_object TYPE tdobject,
         lv_objval TYPE tdobname.

  cs_acdoc-updkz = cs_acdoc-x-achdr-updkz = c_updkz_delete.
  cs_acdoc-acnum = cs_acdoc-x-achdr-acnum.
*-- BOC-T_T.KONNO
  cs_acdoc-y-achdr = cs_acdoc-x-achdr.
*-- BOC-T_T.KONNO

  IF cs_acdoc-x-acitm IS NOT INITIAL.
    lwa_acitm-updkz = c_updkz_delete.
    MODIFY cs_acdoc-x-acitm FROM lwa_acitm TRANSPORTING updkz WHERE acnum NE space.
    cs_acdoc-y = cs_acdoc-x.
  ENDIF.

*-- BOC-T_T.KONNO
  IF cs_acdoc-x-acvlc IS NOT INITIAL.
    lwa_acvlc-updkz = c_updkz_delete.
    MODIFY cs_acdoc-x-acvlc FROM lwa_acvlc TRANSPORTING updkz WHERE acnum NE space.
    cs_acdoc-y = cs_acdoc-x.
  ENDIF.
*-- EOC-T_T.KONNO

  IF ref_text IS NOT INITIAL.
    lv_object = c_object-text_object.
    lv_objval = cs_acdoc-acnum.
    CALL METHOD ref_text->text_delete
      EXPORTING
        i_object = lv_object
        i_objval = lv_objval.
  ENDIF.

*  APPEND cs_acdoc TO lt_acdoc.

  CALL FUNCTION 'ZFMAC_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text = ref_text
    CHANGING
      cs_acdoc  = cs_acdoc
*     CT_MESSAGES       =
    EXCEPTIONS
      no_change = 1
      OTHERS    = 2.

  IF sy-subrc EQ 0.
    MESSAGE ID 'ZFMAC' TYPE 'S' NUMBER '012'
            WITH cs_acdoc-acnum.
  ENDIF.

ENDFUNCTION.
