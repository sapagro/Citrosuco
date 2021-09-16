*&---------------------------------------------------------------------*
*& Report ZABS_REP_MAPA_OPERACOES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_mapa_operacoes.

*--Global data declarations
INCLUDE zabs_inc_mapa_operacoes_top.
*--Processing data
INCLUDE zabs_inc_mapa_operacoes_f01.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Change Screen Elements
  PERFORM change_screen.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*--Change Screen Elements
  PERFORM change_screen.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST FOR Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tplnr-low.
  PERFORM f_get_f4_on_terrain CHANGING s_tplnr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tplnr-high.
  PERFORM f_get_f4_on_terrain CHANGING s_tplnr-high.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*--Initializing Global Data
  PERFORM initialize_global_data.
*--Validate Parameters
  PERFORM validate_parameters.
*--Fetch terrain's data
  PERFORM get_terrains.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*--Display Data
  PERFORM display_data.
