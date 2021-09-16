FUNCTION zabs_fm_mdm_message.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_MESSAGES) TYPE  /AGRI/T_GPROLOG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

  DATA: gt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        gt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        gt_message         TYPE /agri/t_gprolog.

  gt_stack = cl_abap_get_call_stack=>get_call_stack( ).
  gt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( gt_stack ).

  IF it_messages[] IS SUPPLIED.
    gt_message[] = it_messages[].
    EXPORT gt_message[] TO MEMORY ID 'ZVTX_MDM_MESSAGE'.
  ELSEIF et_messages IS REQUESTED.
    IMPORT gt_message[] FROM MEMORY ID 'ZVTX_MDM_MESSAGE'.
    et_messages[] = gt_message[].
    FREE MEMORY ID 'ZVTX_MDM_MESSAGE'.
  ENDIF.

ENDFUNCTION.
