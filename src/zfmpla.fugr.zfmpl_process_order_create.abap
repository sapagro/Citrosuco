FUNCTION ZFMPL_PROCESS_ORDER_CREATE .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_PLITM) TYPE  ZT_FMPLITM
*"  CHANGING
*"     REFERENCE(CS_MESSAGE) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

  PERFORM process_order_create USING it_plitm
                               CHANGING cs_message.

ENDFUNCTION.
