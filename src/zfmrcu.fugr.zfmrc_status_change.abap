FUNCTION ZFMRC_STATUS_CHANGE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TPLNR) TYPE  TPLNR
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_COMMIT_WORK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_ACTIVITY) TYPE  IMAS_ACT_TYPE
*"     REFERENCE(IREF_TEXT) TYPE REF TO /AGRI/CL_GTEXT_PROCESS
*"         OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"      NO_CHANGE
*"      ERROR_WHILE_SAVING
*"--------------------------------------------------------------------



ENDFUNCTION.
