FUNCTION zabs_fm_func_location_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CS_GLFLOT) TYPE  /AGRI/S_GLFLOT OPTIONAL
*"----------------------------------------------------------------------

  CALL SCREEN 201 STARTING AT 5 5.

  MOVE-CORRESPONDING /agri/s_glflot TO cs_glflot.

ENDFUNCTION.
