*&---------------------------------------------------------------------*
*& Report ZABS_REP_PLANPROG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_planprog.

*-- Global data declarations
INCLUDE zabs_inc_planprog_top.

*-- Processing data
INCLUDE zabs_inc_planprog_f01.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*-- Change Screen Elements
  PERFORM change_screen.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-low.
*-- Search Help for Shift
  PERFORM f4_for_shift USING abap_true.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_shift-high.
*-- Search Help for Shift
  PERFORM f4_for_shift USING abap_false.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-- Change Screen Elements
  PERFORM change_screen.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& Get Irrigation Data
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Validate Parameters
  PERFORM validate_parameters.
*-- Initializing Global Data
  PERFORM initialize_global_data.

*-- Planejamento de Irrigação
  IF p_plan EQ abap_true.
*-- Read irrigation and equipments data
    PERFORM fetch_irrigation_data.
*-- Programação de Irrigação
  ELSEIF p_prog EQ abap_true.
*-- Read irrigation and equipments data
    PERFORM fetch_irrigation_data_for_prog.
  ENDIF.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*& Update Irrigation Data
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Planejamento de Irrigação
  IF p_plan EQ abap_true.
*-- Prepare output table
    PERFORM output_prepare.
*-- Programação de Irrigação
  ELSEIF p_prog EQ abap_true.
*-- Prepare output table
    PERFORM output_prepare_for_prog.
  ENDIF.

*-- Building dynamic field catalog
  PERFORM dynamic_fcat_prepare.
*-- Display irrigation data
  PERFORM display_irrigation_data.
