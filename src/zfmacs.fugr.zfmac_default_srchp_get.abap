FUNCTION ZFMAC_DEFAULT_SRCHP_GET.
*"--------------------------------------------------------------------
*"*"Interface local:
*"--------------------------------------------------------------------
**DATA : ls_srchpdoc TYPE /agri/s_gsrchp_doc.
**
**  REFRESH: gt_srch_sel[].
**
**  PERFORM environment_initialize.
**  PERFORM search_selections_fill.
**
**  PERFORM default_search_profile_fill CHANGING ls_srchpdoc.
**
**  IF ls_srchpdoc IS INITIAL.
**    RAISE no_doc_found.
**  ELSE.
**    es_srchpdoc = ls_srchpdoc.
**  ENDIF.





ENDFUNCTION.
