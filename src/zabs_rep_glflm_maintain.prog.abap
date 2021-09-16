*&---------------------------------------------------------------------*
*& Report ZABS_REP_GLFLM_MAINTAIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_glflm_maintain.

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
*INCLUDE /agri/glpg_upload_process_sel.
INCLUDE zabs_inc_upload_process_sel.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0a.
INCLUDE /agri/glpg_upload_process_f0c.
*INCLUDE /agri/glpg_upload_process_f0d.
INCLUDE zabs_inc_upload_process_f0d.
INCLUDE /agri/glpg_upload_process_f0f.
INCLUDE /agri/glpg_upload_process_f0i.
INCLUDE /agri/glpg_upload_process_f0m.
INCLUDE /agri/glpg_upload_process_f0t.
INCLUDE zabs_rep_glflm_maintain_f01.

AT SELECTION-SCREEN OUTPUT.
  PERFORM change_screen_options.

INITIALIZATION.
  DATA: lv_subrc TYPE sy-subrc.
  config_tcode_authority_check '/AGRI/GLUPMD21' '01' lv_subrc.
  IF lv_subrc <> 0.
    MESSAGE ID '/AGRI/FMUPM' TYPE 'E' NUMBER '010' INTO sy-msgli.
    message_simple space.
  ENDIF.

START-OF-SELECTION.
  PERFORM data_get USING p_file.
  IF gs_variables-initiator IS INITIAL.
    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save.
    PERFORM message_add_table.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.
