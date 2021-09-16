FUNCTION y_ofarm_crop_season_op.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_INDATA) TYPE  ZVXS_CROP_INPUT
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  ZVXTT_MESSAGE_GPROLOG
*"     VALUE(ES_CROP_OUTPUT) TYPE  ZVXS_CROP_OUTPUT
*"     VALUE(ES_PO_OUTPUT) TYPE  ZVXTT_PRC_ORDER_OUT
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
*                    SOFTWARE FACTORY <Vistex>                         *
*----------------------------------------------------------------------*
* Requirement ID    : Gap ID 03 + GAP ID 04                            *
* Created By        : Adonis Rossato                                   *
* Creation Date     : 15.04.2021                                       *
* Descriptión       : Logical Crop Creation and Process orders         *
*----------------------------------------------------------------------*
*                    LOG OF MODIFICATIONS                              *
*----------------------------------------------------------------------*
* Changed by        :                                                  *
* Changed Date      :                                                  *
* Transport         :                                                  *
*----------------------------------------------------------------------*

*-- Export the Memory variable to BADI : WORKORDER_UPDATE
  EXPORT gv_fm_call FROM gv_fm_call TO MEMORY ID 'Z_PROG_CHECK'.

*-- Local variables
  DATA: ls_po_indata TYPE zvxs_prc_order,
        lt_messages  TYPE zvxtt_message_gprolog.

*-- Clear global data
  PERFORM global_data_initialize.

*-- Validations to input data
  PERFORM validate_input_data CHANGING is_indata
                                       et_messages[].

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

  CHECK et_messages[] IS INITIAL.

*-- Crop seasons creation/change/delete
  CALL FUNCTION 'ZVX_OFARM_LAV_LOGICA'
    EXPORTING
      is_indata      = is_indata
    IMPORTING
      et_messages    = et_messages
      es_crop_output = es_crop_output.

*-- If no errors, proceed with the Process order creation
  READ TABLE et_messages TRANSPORTING NO FIELDS
  WITH KEY   msgty = gc_constants-msgty_error.
  IF sy-subrc NE 0.

*-- Move the crop seasons data to process orders
    PERFORM move_indata USING is_indata
                     CHANGING ls_po_indata.

*-- Process order Creation/Change/Delete
    CALL FUNCTION 'ZVX_OFARM_LAV_LOG_OP'
      EXPORTING
        is_indata    = ls_po_indata
      IMPORTING
        et_messages  = lt_messages
        es_po_output = es_po_output.

*-- Change the crop season to update the PO creation complete field
*  ZZPOFLAG
    IF NOT gv_poflag IS INITIAL.
      PERFORM upd_poflag_in_season USING is_indata
                                CHANGING et_messages[].
    ENDIF.

    IF gv_po_error EQ abap_true.
*-- Close the crop season based on RFC input data along with
*-- Yard orders
      PERFORM close_crop_season USING gt_cs_key
                                      is_indata
                             CHANGING et_messages[].
    ENDIF.

    APPEND LINES OF lt_messages TO et_messages.

  ENDIF.

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

ENDFUNCTION.
