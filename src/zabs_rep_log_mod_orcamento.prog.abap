*&---------------------------------------------------------------------*
*& Report ZABS_REP_STORDENS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_log_orcamento.

*--Global data declarations
INCLUDE zabs_rep_log_mod_top.

*--Processing data
INCLUDE zabs_rep_log_mod_f01.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Selection Screen Validations
  PERFORM selection_validations.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_period-low.
*--Search Help for Characteristics
  PERFORM f4_for_period USING abap_true.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_period-high.
*--Search Help for Characteristics
  PERFORM f4_for_period USING abap_false.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sver.
*-- Check Available Versions
  PERFORM f4_for_version USING 'P_SVER'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tver.
*-- Check Available Versions
  PERFORM f4_for_version USING 'P_TVER'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_user.
*-- Search Help for User
  PERFORM f4_for_user.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON VALUE-REQUEST FO
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
*--F4 Help for Varient
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
*--Display ALV Layout variant
  PERFORM f_display_variant.

*&--------------------------------------------------------------------*
*&    INITIALIZATION
*&--------------------------------------------------------------------*
*&
*&--------------------------------------------------------------------*
INITIALIZATION.
*--Initialize ALV Layout variant
  PERFORM f_initialize_variant.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Process Order Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*--Validate Parameters
  PERFORM validate_parameters.

*--Refresh Global Data
  PERFORM refresh_global_data.

*--Preparing Budget's Data
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Mobility's Inspection Lot Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*--Display Budget's Data
  PERFORM display_budget_data.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
