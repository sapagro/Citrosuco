FUNCTION zabs_fm_cs_popup_input.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_CSDOC) TYPE  /AGRI/S_GLCS_DOC
*"  EXPORTING
*"     REFERENCE(E_SEASON) TYPE  /AGRI/GL_SEASON
*"     REFERENCE(E_YEAR) TYPE  GJAHR
*"----------------------------------------------------------------------

  CLEAR /agri/s_glcsscrfields-gyear.
  gs_variables-date = is_csdoc-x-cshdr-datbi + 1.
  gs_variables-year = gs_variables-date(4).
  CALL SCREEN 0100 STARTING AT 5 5 ENDING AT 40 5.
  IF /agri/s_glflcma-season IS NOT INITIAL AND
     /agri/s_glcsscrfields-gyear IS NOT INITIAL.
    e_season = /agri/s_glflcma-season.
    e_year   = /agri/s_glcsscrfields-gyear.
  ENDIF.

ENDFUNCTION.
