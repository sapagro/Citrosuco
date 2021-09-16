*&---------------------------------------------------------------------*
*& Report ZABS_REP_STORDENS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_stordens.

*--Global data declarations
INCLUDE zabs_rep_stordens_top.

*--Processing data
INCLUDE zabs_rep_stordens_f01.

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
*& Preparing Process Order Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Validate Parameters
  PERFORM validate_parameters.

*--Refresh Global Data
  PERFORM refresh_global_data.

*--Preparing Process Order Data
  PERFORM build_data.

*&--------------------------------------------------------------------*
*&    END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Process Order Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_tecom EQ abap_true.
*--Technically Complete Order
    PERFORM technically_complete.
  ELSEIF p_canc EQ abap_true.
*--Revoke Technically Complete
    PERFORM zrevoke_technical_complete.
  ELSEIF p_alv EQ abap_true.
*--Display Order Data
    PERFORM display_order_data.
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
