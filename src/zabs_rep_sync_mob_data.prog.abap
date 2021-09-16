*&---------------------------------------------------------------------*
*& Report ZABS_REP_SYNC_MOB_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_sync_mob_data.

INCLUDE zabs_inc_sync_mob_data_top.
INCLUDE zabs_inc_sync_mob_data_f01.

START-OF-SELECTION.

  PERFORM check_parameters.
  PERFORM read_accomplishment_sheet.
  PERFORM update_accomplishment_sheet.

  IF gt_messages[] IS NOT INITIAL.
    gt_message[] = gt_messages[].
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
