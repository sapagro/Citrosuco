*&---------------------------------------------------------------------*
*& Report ZABS_REP_STORDENS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_rel_orcamento.

*--Global data declarations
INCLUDE zabs_rep_rel_orc_top.

*--Processing data
INCLUDE zabs_rep_rel_orc_f01.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON VALUE-REQUEST FO
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
*--F4 Help for Varient
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
*--Display ALV Layout variant
  PERFORM f_display_variant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_period-low.
*--Search Help for Period
  PERFORM f4_for_period USING abap_true.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_period-high.
*--Search Help for Period
  PERFORM f4_for_period USING abap_false.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_faz-low.
*--Search Help for Terrain
  PERFORM f4_for_terrain USING abap_true.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_faz-high.
*--Search Help for Terrain
  PERFORM f4_for_terrain USING abap_false.

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
