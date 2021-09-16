FUNCTION zfmrc_delete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CS_RCDOC) TYPE  ZSC_FMRC_DOC OPTIONAL
*"----------------------------------------------------------------------

  DATA :
    lwa_rchdr TYPE zsc_fmrchdr,
    lt_rcdoc  TYPE zt_fmrc_doc.
  DATA : lv_object TYPE tdobject,
         lt_rclst  TYPE zt_fmrclst,
         lv_objval TYPE tdobname.

FIELD-SYMBOLS: <lwa_rclst> TYPE zsc_fmrclst,
               <lwa_rcbom> TYPE zsc_fmrcbom,
               <lwa_rcvrs> TYPE zsc_fmrcvrs.

PERFORM all_alternatives_obtein CHANGING lt_rclst.

  cs_rcdoc-updkz = cs_rcdoc-x-rchdr-updkz = c_updkz_delete.
  cs_rcdoc-rcnum = cs_rcdoc-x-rchdr-rcnum.

  IF cs_rcdoc-x-rchdr IS NOT INITIAL.
    lwa_rchdr-updkz = c_updkz_delete.
  ENDIF.

REFRESH: cs_rcdoc-x-rclst.
cs_rcdoc-x-rclst[] = CORRESPONDING #( lt_rclst[] ).

LOOP AT cs_rcdoc-x-rclst ASSIGNING <lwa_rclst> WHERE rcnum is NOT INITIAL.
  MOVE c_updkz_delete to <lwa_rclst>-updkz.
ENDLOOP.

LOOP AT cs_rcdoc-x-rcbom ASSIGNING <lwa_rcbom> WHERE rcnum is NOT INITIAL..
  MOVE c_updkz_delete to <lwa_rcbom>-updkz.
ENDLOOP.

LOOP AT cs_rcdoc-x-rcvrs ASSIGNING <lwa_rcvrs> WHERE rcnum is NOT INITIAL..
  MOVE c_updkz_delete to <lwa_rcvrs>-updkz.
ENDLOOP.
cs_rcdoc-y = CORRESPONDING #( cs_rcdoc-x ).
  IF ref_text IS NOT INITIAL.
    lv_object = c_object-text_object.
    lv_objval = cs_rcdoc-rcnum.
    CALL METHOD ref_text->text_delete
      EXPORTING
        i_object = lv_object
        i_objval = lv_objval.
  ENDIF.


  CALL FUNCTION 'ZFMRC_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text = ref_text
    CHANGING
      cs_rcdoc  = cs_rcdoc
*     CT_MESSAGES       =
    EXCEPTIONS
      no_change = 1
      OTHERS    = 2.

  IF sy-subrc EQ 0.
    MESSAGE ID 'ZFMRC' TYPE 'S' NUMBER '012'
            WITH cs_rcdoc-rcnum.
  ENDIF.

ENDFUNCTION.
