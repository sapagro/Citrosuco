FUNCTION zfmrc_search_results_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CT_RCHDR) TYPE  ZT_FMRCHDR
*"     VALUE(CT_RCLST) TYPE  ZT_FMRCLST
*"     VALUE(CT_RCBOM) TYPE  ZT_FMRCBOM
*"----------------------------------------------------------------------

  DATA : lt_search_docs TYPE zt_fmrchdr.
  lt_search_docs = ct_rchdr.

  EXPORT zfmrchdr = lt_search_docs TO MEMORY ID 'FMRC_SRCH'.

ENDFUNCTION.
