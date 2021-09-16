FUNCTION ZFMRC_CREATE_MASS.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MESSAGES_DISPLAY) TYPE  SY-DATAR DEFAULT SPACE
*"     VALUE(I_REFRESH_MESSAGES) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(I_SAVE_MESSAGES) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(IT_MDHDR) TYPE  /AGRI/T_GLMDHDR
*"     REFERENCE(IT_MDATV) TYPE  /AGRI/T_GLMDATV
*"  EXPORTING
*"     REFERENCE(ET_MDDOC) TYPE  /AGRI/T_GLMD_DOC
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"     REFERENCE(E_LOG_NUMBER) TYPE  BALOGNR
*"  EXCEPTIONS
*"      INCONSISTENT_DATA
*"--------------------------------------------------------------------

**  DATA: lv_subrc        TYPE sy-subrc,
**        lv_error,
**        lwa_dummy_header TYPE /agri/1899SC_FMRCHDR,
**        lwa_attr_values TYPE /agri/s_FMRCatv_fcat,
**        lwa_RCHDR       TYPE /agri/1899SC_FMRCHDR,
**        lwa_mdatv       TYPE /agri/s_FMRCatv,
**        lwa_context     TYPE /agri/s_FMRC_context,
**        lt_RCDOC        TYPE /agri/t_FMRC_doc,
**        ls_variant      TYPE disvariant.
**
**  DEFINE messages_all_display.
**    if i_messages_display eq c_true.
**      messages_display gs_variables-initiator c_true space
**                       space ls_variant.
**    else.
**      messages_get gs_variables-initiator et_messages[].
**    endif.
**    if i_save_messages eq c_true.
**      messages_save gs_variables-initiator c_true.
**    endif.
**    clear gs_variables-initiator.
**    e_log_number = gs_log_variables-log_number.
**    messages_init.
**  END-OF-DEFINITION.
**
**  PERFORM document_data_initialize USING i_refresh_messages.
**  gs_variables-document_mode = c_mode_create.
**
**  gs_variables-initiator = c_log_initiator-create.
**  PERFORM messages_initialize USING gs_variables-initiator
**                                    c_log_subobject-create
**                                    lwa_RCHDR.
**
**  IF it_RCHDR[] IS INITIAL OR
**     it_mdatv[] IS INITIAL.
**    RAISE inconsistent_data.
**  ENDIF.
**
**  LOOP AT it_RCHDR INTO lwa_RCHDR.
**
**    PERFORM document_data_initialize USING space.
**    CLEAR: lv_subrc.
**
**    gs_variables-document_mode     = c_mode_create.
**    gs_RCDOC_infocus-x-RCHDR-updkz = c_updkz_new.
**    gs_RCDOC_infocus-RCNUM         =
**    gs_RCDOC_infocus-x-RCHDR-RCNUM = text-046.
**
**    lwa_RCHDR-updkz = c_updkz_new.
**
**    MOVE-CORRESPONDING lwa_RCHDR TO lwa_context.
**    messages_context_data_set lwa_RCHDR-RCNUM space space
**                          '/AGRI/S_FMRC_CONTEXT' lwa_context.
**
**    IF lwa_RCHDR-aslvl EQ c_measurement_level-crop_seasons OR
**       lwa_RCHDR-aslvl EQ c_measurement_level-harvest.
**      MOVE-CORRESPONDING lwa_RCHDR TO /agri/s_glflcma.
**    ENDIF.
**
**    PERFORM md_header_data_check CHANGING lwa_RCHDR
**                                          lv_subrc.
**    IF lv_subrc IS NOT INITIAL.
**      CONTINUE.
**    ENDIF.
**    PERFORM md_header_update USING lwa_dummy_header
**                          CHANGING lwa_RCHDR
**                                   lv_subrc.
**    IF lv_subrc IS NOT INITIAL.
**      CONTINUE.
**    ENDIF.
**
**    MOVE-CORRESPONDING gs_RCDOC_infocus-x-RCHDR TO /agri/1899SC_FMRCHDR.
**    PERFORM check_attribute_group CHANGING lv_subrc.
**    IF lv_subrc IS NOT INITIAL.
**      CONTINUE.
**    ENDIF.
**
**    PERFORM attribute_values_fill.
**
**    PERFORM attr_value_check_class_call USING space
**                                     CHANGING gt_value_check_atinn
**                                              lv_subrc
**                                              lv_error.
**
**    LOOP AT it_mdatv INTO lwa_mdatv
**                    WHERE RCNUM EQ lwa_RCHDR-RCNUM.
**      MOVE-CORRESPONDING lwa_mdatv TO lwa_attr_values. "#EC CI_FLDEXT_OK
**      APPEND lwa_attr_values TO gt_attr_values.
**      CLEAR lwa_attr_values.
**    ENDLOOP.
**
**    PERFORM attributes_data_update
**                      CHANGING gt_attr_values[]
**                               lv_subrc
**                               lv_error.
**    IF lv_subrc IS NOT INITIAL
**    OR lv_error IS NOT INITIAL.
**      CLEAR: lv_subrc, lv_error.
**      REFRESH: gt_attr_values[], gt_value_check_atinn[].
**      CONTINUE.
**    ENDIF.
**
**    PERFORM RCDOC_infocus_save CHANGING lv_subrc
**                                        lt_RCDOC.
**    IF lv_subrc IS INITIAL.
**      APPEND LINES OF lt_RCDOC TO et_RCDOC.
**    ENDIF.
**    REFRESH: lt_RCDOC, gt_attr_values[], gt_value_check_atinn[].
**
**  ENDLOOP.
**
**  messages_all_display.

ENDFUNCTION.
