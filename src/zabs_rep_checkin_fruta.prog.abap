REPORT zabs_rep_checkin_fruta.

INCLUDE zabs_inc_checkin_fruta_top.

INCLUDE zabs_rep_checkin_fruta_selef01.

*&--------------------------------------------------------------------*
*&    INITIALIZATION
*&--------------------------------------------------------------------*
*& Initialization
*&--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_arrend_values.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN OUTPUT
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
*AT SELECTION-SCREEN.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN value requests
*&--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR...

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
*& Create Ticket
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*--Selection Screen Validations
  PERFORM selection_validations.

*--Create ticket
  PERFORM ticket_create.

  IF gs_variables-initiator IS INITIAL.
    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save.
    PERFORM message_add_table.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.
