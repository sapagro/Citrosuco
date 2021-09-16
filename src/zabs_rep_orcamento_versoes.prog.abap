*&---------------------------------------------------------------------*
*& Report ZABS_REP_ORCAMENTO_VERSOES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_orcamento_versoes.

INCLUDE zabs_inc_orcamento_versoes_top IF FOUND.

INCLUDE zabs_rep_orcamento_versoes_f01 IF FOUND.

*&--------------------------------------------------------------------*
*&    INITIALIZATION
*&--------------------------------------------------------------------*
*& Initialization
*&--------------------------------------------------------------------*
INITIALIZATION.
*-- Fill Parameter's Defaul Values
  PERFORM fill_default_values.
*-- Create Button
  PERFORM button_create.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sver.
*-- Check Available Versions
  PERFORM check_available_versions USING 'P_SVER'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tver.
*-- Check Available Versions
  PERFORM check_available_versions USING 'P_TVER'.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sy-ucomm EQ 'FC01'.
    PERFORM initial_version_create.
  ENDIF.

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

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*-- Generate Budget's Version Data
  PERFORM budget_version_generate USING p_acnum
                                        p_sver
                                        p_tver.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
