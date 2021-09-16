FUNCTION zabs_fm_memory_msgs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_MESSAGES) TYPE  ZABS_TTY_SLS_MSGS OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  ZABS_TTY_SLS_MSGS
*"----------------------------------------------------------------------

  DATA: t_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        t_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        t_msgs            TYPE zabs_tty_sls_msgs.

  t_stack = cl_abap_get_call_stack=>get_call_stack( ).
  t_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( t_stack ).

  DATA(lv_monitor) = abap_false.
  READ TABLE t_formatted_stack TRANSPORTING NO FIELDS
    WITH KEY kind        = 'FUNCTION'
             progname    = 'SAPLZFMNTX_RECEIPT'
             includename = 'LZFMNTX_RECEIPTU01'
             event       = 'ZFMNTX_PRODUCE_RECEIPT_QTDE'.
  IF sy-subrc EQ 0.
    lv_monitor = abap_true.
  ENDIF.

  IF lv_monitor EQ abap_false.
    IF it_messages[] IS SUPPLIED.
      t_msgs[] = it_messages[].
      EXPORT t_msgs[] TO MEMORY ID 'ZVTX_ERROR'.
    ELSEIF et_messages IS REQUESTED.
      IMPORT t_msgs[] FROM MEMORY ID 'ZVTX_ERROR'.
      et_messages[] = t_msgs[].
      FREE MEMORY ID 'ZVTX_ERROR'.
    ENDIF.
  ELSE.
    IF it_messages[] IS SUPPLIED.
      IMPORT t_msgs[] FROM MEMORY ID 'ZVTX_ERROR'.
      APPEND LINES OF it_messages TO t_msgs.
      t_msgs[] = it_messages[].
      EXPORT t_msgs[] TO MEMORY ID 'ZVTX_ERROR'.
    ELSEIF et_messages IS REQUESTED.
      IMPORT t_msgs[] FROM MEMORY ID 'ZVTX_ERROR'.
      et_messages[] = t_msgs[].
      FREE MEMORY ID 'ZVTX_ERROR'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
