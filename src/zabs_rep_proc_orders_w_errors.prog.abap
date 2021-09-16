*&---------------------------------------------------------------------*
*& Report ZABS_REP_MEASUREM_DOC_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_proc_orders_w_errors.

INCLUDE zabs_rep_proc_ord_w_errors_top.
INCLUDE zabs_rep_proc_ord_w_errors_f01.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Check Measurement Document's Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*-- Initializing Global Data
  PERFORM initialize_global_data.
*--Selection Screen Validations
  PERFORM selection_validations.
*-- Fetch Data From /AGRI/FMFPHDR
  PERFORM data_get.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Create Measurement Document
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*-- Check Process Order
  PERFORM check_process_order.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
