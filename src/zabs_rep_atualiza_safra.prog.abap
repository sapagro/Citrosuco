*&---------------------------------------------------------------------*
*& Report ZABS_REP_ATUALIZA_SAFRA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_atualiza_safra.

*-- Global data declarations
INCLUDE zabs_inc_atualiza_safra_top.

*-- Processing data
INCLUDE zabs_inc_atualiza_safra_f01.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*-- Change Screen Elements
  PERFORM change_screen.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-- Change Screen Elements
  PERFORM change_screen.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Validate Parameters
  PERFORM validate_parameters.

*-- Fetch season's data
  IF p_upd EQ abap_true.
    PERFORM get_season_upd.
  ELSEIF p_enh EQ abap_true.
    PERFORM get_season_enh.
  ENDIF.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*& UPDATE_SEASON_DATA
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*-- Update Season Update
  IF p_upd EQ abap_true.
    PERFORM update_season.
  ELSEIF p_enh EQ abap_true.
    PERFORM enhance_season.
  ENDIF.

  IF gt_message[] IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM gt_message COMPARING ALL FIELDS.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM display_messages USING gs_variables-initiator.
    ENDIF.
  ENDIF.
