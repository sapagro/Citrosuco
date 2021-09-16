FUNCTION ZFMAC_SEARCH_RESULTS_GET.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CT_ACHDR) TYPE  ZT_FMACHDR
*"     REFERENCE(CT_ACITM) TYPE  ZT_FMACITM
*"----------------------------------------------------------------------

  DATA : lt_search_docs TYPE zt_fmachdr.
  lt_search_docs = ct_achdr.

  EXPORT zfmachdr = lt_search_docs TO MEMORY ID 'FMAC_SRCH'.

ENDFUNCTION.
