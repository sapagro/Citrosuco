*&---------------------------------------------------------------------*
*& Report ZABS_REP_STORDENS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_erro_conf.

*--Global data declarations
INCLUDE zabs_inc_erro_conf_top.
*--Processing data
INCLUDE zabs_inc_erro_conf_f01.
INCLUDE zabs_inc_erro_conf_f02.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Accomplishment Sheet's Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*--Preparing Accomplishment Sheet's Data
  PERFORM check_parameters.
  PERFORM read_accomplishment_sheet.
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Mobility's Inspection Lot Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Errors
  PERFORM display_confirmation_errors.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
