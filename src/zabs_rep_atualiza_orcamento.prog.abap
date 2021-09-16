*&---------------------------------------------------------------------*
*& Report ZABS_REP_ATUALIZA_ORCAMENTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_atualiza_orcamento.

INCLUDE zabs_inc_atualiza_orc_top IF FOUND.

INCLUDE zabs_inc_atualiza_orc_f01 IF FOUND.

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

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  CALL SCREEN 100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_WO_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_wo_alv .

  DATA: lt_insert TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_delete TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0.

  LOOP AT gt_update INTO DATA(ls_update).
    IF ls_update-inserir EQ abap_true.
      INSERT INITIAL LINE INTO TABLE lt_insert
        ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
    ELSEIF ls_update-eliminar EQ abap_true.
      INSERT INITIAL LINE INTO TABLE lt_delete
        ASSIGNING <ls_orcamento>.
    ENDIF.

    IF <ls_orcamento> IS ASSIGNED.
      MOVE-CORRESPONDING ls_update TO <ls_orcamento>.
    ENDIF.
  ENDLOOP.

  IF lt_insert[] IS NOT INITIAL.
    INSERT zabs_orcamento FROM TABLE lt_insert.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_delete[] IS NOT INITIAL.
    DELETE zabs_orcamento FROM TABLE lt_delete.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*-- A tabela ZABS_ORCAMENTO foi atualizada com sucesso!
  MESSAGE i349(zfmfp).

ENDFORM.
