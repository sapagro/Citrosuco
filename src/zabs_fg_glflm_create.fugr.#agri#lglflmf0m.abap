*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0M .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  messages_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_display  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program-funloc.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR gs_variables-initiator.

ENDFORM.                    " MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  messages_initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_initialize USING lv_initiator TYPE /agri/gdescr
                               lv_subobject TYPE balsubobj
                               ls_glflhdr TYPE /agri/s_glflot.

*  DATA: ls_context TYPE /agri/s_glfl_context.

  messages_init.

  CHECK lv_initiator IS NOT INITIAL.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

*****Set the context for Process Log
*  IF NOT ls_glflhdr IS INITIAL.
*    MOVE-CORRESPONDING ls_glflhdr TO ls_context.
*    messages_context_data_set ls_glflhdr-tplnr_fl space space
*                              '/AGRI/S_GLCM_CONTEXT' ls_context.
*  ENDIF.

ENDFORM.                    " messages_initialize
*&---------------------------------------------------------------------*
*&      Form  MASS_PROCESSING_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mass_processing_initialize USING lv_mass_fcode.

  DATA: lv_answer.

  PERFORM changes_confirm CHANGING lv_answer.
  IF lv_answer EQ 'A' AND ok_code EQ c_fcode-save.
    CLEAR: lv_answer, gs_variables-errors.
    PERFORM document_infocus_save USING space.
  ENDIF.
  CHECK lv_answer NE 'A' AND gs_variables-errors EQ space.

  PERFORM document_infocus_unlock USING gs_fldoc_infocus-tplnr_fl
                                        gs_fldoc_infocus-x-flhdr-strno.
  PERFORM document_data_initialize USING c_true.

  gs_variables-document_mode = c_mode_change.
  gs_variables-overview_mode = c_mode_change.

  CASE lv_mass_fcode.
    WHEN c_fcode-mass_header_change.
      PERFORM header_mass_process TABLES gt_selected_terrains
                                  USING gs_variables-document_mode.
  ENDCASE.

ENDFORM.                    " MASS_PROCESSING_INITIALIZE
