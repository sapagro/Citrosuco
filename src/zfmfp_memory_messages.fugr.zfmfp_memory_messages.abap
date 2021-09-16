FUNCTION zfmfp_memory_messages.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_MESSAGES) TYPE  /AGRI/T_GPROLOG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

  DATA: lt_memory_msg TYPE /agri/t_gprolog.

  IF it_messages[] IS SUPPLIED.
    lt_memory_msg[] = it_messages[].
    EXPORT lt_memory_msg[] TO MEMORY ID 'ZBS_TO_CREATE'.
  ELSEIF et_messages[] IS REQUESTED.
    IMPORT lt_memory_msg[] FROM MEMORY ID 'ZBS_TO_CREATE'.
    FREE MEMORY ID 'ZBS_TO_CREATE'.
    et_messages[] = lt_memory_msg[].
  ENDIF.

ENDFUNCTION.
