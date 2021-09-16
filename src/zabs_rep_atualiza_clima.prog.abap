*&---------------------------------------------------------------------*
*& Report ZABS_REP_ATUALIZA_CLIMA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_atualiza_clima.

*-- Global data declarations
INCLUDE zabs_inc_atualiza_clima_top.
*-- Processing data
INCLUDE zabs_inc_atualiza_clima_f01.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& Get weather data
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Validate Parameters
  PERFORM validate_parameters.
*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Read measurement document
  PERFORM fetch_measurement_document.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*& Updates weather data table ZABST_CLIMA_HIST
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Update ZABST_CLIMA_HIST
  PERFORM update_weather_data.

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
