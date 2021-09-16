FUNCTION ZFMAC_DB_SEARCH.
*"--------------------------------------------------------------------
*"*"Interface local:
*"--------------------------------------------------------------------
DATA: lt_selection TYPE /agri/t_bsel_values,
        lv_max_hits  TYPE /agri/gspmxhit.

**  PERFORM environment_initialize.
**  PERFORM search_selections_fill.
**
**  gs_variables-version = is_srch_prfl_envr-version.
**  gs_variables-repname = is_srch_prfl_envr-repname.
**
**  PERFORM version_selections_add TABLES lt_selection
**                                  USING gs_variables-version.
**
**  CALL FUNCTION '/AGRI/GSRCHP_SELECTIONS_GET'
**    EXPORTING
**      iref_upf_callback = iref_callback
**      is_environment    = is_srch_prfl_envr
**    IMPORTING
**      e_max_hits        = lv_max_hits
**    CHANGING
**      ct_selection      = ct_selection
**    EXCEPTIONS
**      cancelled_by_user = 1
**      data_not_found    = 2
**      OTHERS            = 3.
**
**  IF sy-subrc NE 0.
**    RAISE no_data_selected.
**  ENDIF.
**
**  lt_selection[] = ct_selection[].
**
***** Fill select Options
**  PERFORM search_options_fill TABLES lt_selection
**                               USING gs_variables-version.
**  PERFORM header_get TABLES lt_selection.
**
**  IF et_header IS REQUESTED.
**    et_header[] = gt_search_hdr[].
**  ENDIF.





ENDFUNCTION.
