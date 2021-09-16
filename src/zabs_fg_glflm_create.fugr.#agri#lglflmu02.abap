FUNCTION /agri/glfl_infocus_get.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_TPLNR) TYPE  /AGRI/GLTPLNR_FL
*"  EXPORTING
*"     REFERENCE(ES_GLFL_DOC) TYPE  /AGRI/S_GLFL_DOC
*"  EXCEPTIONS
*"      FL_NOT_IN_FOCUS
*"--------------------------------------------------------------------
  IF gs_fldoc_infocus-x-flhdr-tplnr_fl EQ i_tplnr.
    es_glfl_doc = gs_fldoc_infocus.
  ELSE.
    RAISE fl_not_in_focus.
  ENDIF.

ENDFUNCTION.
