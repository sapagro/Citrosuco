*&---------------------------------------------------------------------*
*& Report ZFMFP_TASK_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfp_scheduled_break_mass.

DATA: dummy.

INCLUDE zfmfp_inc_scheduled_break_mass.

*"Informações do Arquivo/File Info
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_fname TYPE char255 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk2.

INITIALIZATION.
  PERFORM screen_update.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_update.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM file_path_get CHANGING p_fname.

START-OF-SELECTION.
  PERFORM file_extension_get USING p_fname
                          CHANGING gv_file_extension
                                   gv_subrc.

  PERFORM excel_data_import USING p_fname
                                  gv_file_extension
                         CHANGING gt_excel
                                  gt_bdc_messages
                                  gv_subrc.

  IF gs_variables-initiator IS INITIAL.
    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save.
    PERFORM message_add_table.
*    PERFORM messages_display USING gs_variables-initiator.
    PERFORM messages_table_display USING gs_variables-initiator.
  ENDIF.
