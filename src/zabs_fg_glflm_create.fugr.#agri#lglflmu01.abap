FUNCTION /AGRI/GLFL_SEARCH_RESULTS_GET.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CT_FLOT) TYPE  /AGRI/T_GLFLOT
*"--------------------------------------------------------------------
DATA: lt_search_docs TYPE /agri/t_glflot.

  lt_search_docs = ct_flot.

  EXPORT /agri/glflot = lt_search_docs TO MEMORY ID 'GLFLOT_SRCH'.





ENDFUNCTION.
