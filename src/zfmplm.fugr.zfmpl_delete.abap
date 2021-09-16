FUNCTION ZFMPL_DELETE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CS_MDDOC) TYPE  /AGRI/S_GLMD_DOC OPTIONAL
*"--------------------------------------------------------------------

**  DATA : lwa_mdatv  TYPE /agri/s_FMACatv,
**         lwa_mditm  TYPE /agri/s_FMACitm,
**         lt_acdoc   TYPE /agri/t_FMAC_doc.
**  DATA : lv_object TYPE tdobject,
**         lv_objval TYPE tdobname.
**
**  CLEAR cs_acdoc-x.
**
**  cs_acdoc-y-achdr-updkz = c_updkz_delete.
**
**  IF cs_acdoc-y-mditm IS NOT INITIAL.
**    lwa_mditm-updkz = c_updkz_delete.
**    MODIFY cs_acdoc-y-mditm FROM lwa_mditm TRANSPORTING updkz WHERE acnum NE space.
**  ENDIF.
**
**  IF cs_acdoc-y-mdatv IS NOT INITIAL.
**    lwa_mdatv-updkz = c_updkz_delete.
**    MODIFY cs_acdoc-y-mdatv FROM lwa_mdatv TRANSPORTING updkz WHERE acnum NE space.
**  ENDIF.
**
**  IF ref_text IS NOT INITIAL.
**    lv_object = c_object-text_object.
**    lv_objval = cs_acdoc-acnum.
**    CALL METHOD ref_text->text_delete
**      EXPORTING
**        i_object = lv_object
**        i_objval = lv_objval.
**  ENDIF.
**
**  APPEND cs_acdoc TO lt_acdoc.
**
**  CALL FUNCTION '/AGRI/FMAC_SAVE'
**    EXPORTING
***     I_SET_UPDATE_TASK = 'X'
***     I_COMMIT_WORK     = 'X'
**      iref_text         = ref_text
**    CHANGING
**      ct_acdoc          = lt_acdoc
***     CT_MESSAGES       =
**    EXCEPTIONS
**      no_change         = 1
**      OTHERS            = 2.
**
**  IF sy-subrc EQ 0.
**    MESSAGE ID '/AGRI/FMAC' TYPE 'S' NUMBER '012'
**            WITH cs_acdoc-acnum.
**  ENDIF.

ENDFUNCTION.
