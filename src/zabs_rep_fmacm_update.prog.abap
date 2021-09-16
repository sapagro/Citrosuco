*&---------------------------------------------------------------------*
*& Report ZABS_REP_FMACM_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_fmacm_update.

*-- Global data declarations
INCLUDE zabs_inc_fmacm_update_top.

*-- Processing data
INCLUDE zabs_inc_fmacm_update_f01.

*ZABS_REP_MDM_MASS_APPROVALS
*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
*-- Default Screen Values
  PERFORM set_default_values.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*-- Selection Screen Validations
  PERFORM selection_validations.

AT SELECTION-SCREEN OUTPUT.
*-- Upate Screen
  PERFORM update_screen.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*-- Get Cultivation Area Data
  PERFORM get_cultivation_area_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*-- Update Cultivation Area
  PERFORM update_cultivation_area.

*-- BOC T_T.KONNO 04.07.21
*  IF gs_variables-initiator IS INITIAL.
*    gs_variables-initiator = c_log_initiator-save.
*    PERFORM messages_initialize USING gs_variables-initiator
*                                      c_log_subobject-save.
*    PERFORM message_add_table.
*    PERFORM messages_display USING gs_variables-initiator.
*  ENDIF.
  IF p_glmdm IS INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ELSE.
    EXPORT gt_message TO MEMORY ID 'ZFMACM_UPDATE'.
  ENDIF.
*-- EOC T_T.KONNO 04.07.21
