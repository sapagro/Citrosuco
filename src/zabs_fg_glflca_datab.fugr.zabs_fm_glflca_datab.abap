FUNCTION zabs_fm_glflca_datab.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CV_DATAB) TYPE  /AGRI/GLDATAB
*"----------------------------------------------------------------------

  gv_datab_y = cv_datab.
  gv_datab_x = cv_datab.

*--Calling screen to change DATAB
  CALL SCREEN 0100 STARTING AT 25 5.

  cv_datab = gv_datab_x.

ENDFUNCTION.
