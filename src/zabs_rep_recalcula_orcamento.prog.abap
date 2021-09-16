*&---------------------------------------------------------------------*
*& Report ZABS_REP_ATUALIZA_ORCAMENTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_recalcula_orcamento.

INCLUDE zabs_inc_recalcula_orc_top IF FOUND.

INCLUDE zabs_inc_recalcula_orc_f01 IF FOUND.

*&--------------------------------------------------------------------*
*&    INITIALIZATION
*&--------------------------------------------------------------------*
*& Initialization
*&--------------------------------------------------------------------*
INITIALIZATION.
*-- Fill Parameter's Default Values
  PERFORM fill_default_values.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_acnum.
*-- Check Available Cultivation Area
  PERFORM check_available_cult_area.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vers.
*-- Check Available Versions
  PERFORM check_available_versions USING 'P_VERS'.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Fetch Budget's Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Selection Screen Validations
  PERFORM selection_validations.
*-- Fetch Agriplan's Processes
  PERFORM get_processes.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*-- Validate Selected Data
  PERFORM data_validation.

  IF p_save EQ abap_false.
*-- Display ALV
    PERFORM display_data.
  ELSEIF p_save EQ abap_true.
*-- Update ZABS_ORCAMENTO entries
    PERFORM save_wo_alv.
  ENDIF.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
