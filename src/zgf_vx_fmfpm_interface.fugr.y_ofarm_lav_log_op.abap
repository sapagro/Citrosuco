FUNCTION y_ofarm_lav_log_op .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_INDATA) TYPE  ZVXS_PRC_ORDER
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  ZVXTT_MESSAGE_GPROLOG
*"     VALUE(ET_OUTDATA) TYPE  ZVXTT_PRC_ORDER_OUT
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                    SOFTWARE FACTORY <Vistex>                         *
*----------------------------------------------------------------------*
* Requirement ID    : Gap ID 04                                        *
* Created By        : Adonis Rossato                                   *
* Creation Date     : 15.04.2021                                       *
* Descriptión       : Logical Crop Creation                            *
*----------------------------------------------------------------------*
*                    LOG OF MODIFICATIONS                              *
*----------------------------------------------------------------------*
* Changed by        :                                                  *
* Changed Date      :                                                  *
* Transport         :                                                  *
*---------------------------------------------------------------------*

*-- Export the Memory variable to BADI : WORKORDER_UPDATE
*-- To check indicator in Enhancement by Partial Network in BADI
  EXPORT gv_fm_call TO MEMORY ID 'Z_PROG_CHECK'.

*-- Local internal tables
  DATA: lt_cskey TYPE /agri/t_glcs_key.

*-- Validations to input data
  PERFORM validate_input_data CHANGING is_indata
                                       et_messages[]
                                       lt_cskey[].

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

  CHECK et_messages[] IS INITIAL.

  CASE is_indata-zindfield.
    WHEN gc_constants-insert.

*-- Create Process Order
      PERFORM create_process_order USING is_indata
                                         lt_cskey
                                CHANGING et_messages[]
                                         et_outdata[].

    WHEN gc_constants-delete.

*-- Inactivate Process Order( Technically complete )
      PERFORM inactivate_process_order USING is_indata
                                    CHANGING et_messages[]
                                             et_outdata[].

  ENDCASE.

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

ENDFUNCTION.
