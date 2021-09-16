*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0D.
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_SEARCH_PROFILE_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM default_search_profile_fill CHANGING ls_sp_infocus TYPE /agri/s_gsrchp_doc.

**  DATA : lt_spslf TYPE /agri/t_tgspsf,
**         lt_spgrp TYPE /agri/t_tgspag.
**
**  DATA : lwa_spslf    TYPE /agri/s_tgspsf,
**         lwa_srch_sel TYPE /agri/s_gsrchp_sel,
**         lwa_spgrp    TYPE /agri/s_tgspag,
**         lwa_sphdr    TYPE /agri/s_tgsp.
**
**  DATA:  lv_exclude TYPE xfeld.
**
**  FIELD-SYMBOLS : <lwa_srch_sel>  TYPE /agri/s_gsrchp_sel.
*****Doc header
**  ls_sp_infocus-srchp = 'DFLT'.
**
*****Header
**  ls_sp_infocus-x-sphdr-srchp = ls_sp_infocus-srchp.
**  ls_sp_infocus-x-sphdr-appln = '0'.
**  ls_sp_infocus-x-sphdr-spsts = abap_true.
**  ls_sp_infocus-x-sphdr-mxhit = 500.
**  ls_sp_infocus-x-sphdr-srhct = '1'.
**  ls_sp_infocus-x-sphdr-descr = TEXT-027.
**
**  LOOP AT gt_srch_sel ASSIGNING <lwa_srch_sel>.
**
**    PERFORM selection_exclude USING <lwa_srch_sel>
**                                    gs_variables-version
**                           CHANGING lv_exclude.
**
**    CHECK lv_exclude IS INITIAL.
**
**    READ TABLE lt_spslf
**    TRANSPORTING NO FIELDS
**    WITH KEY srchp = ls_sp_infocus-srchp
**             tname = <lwa_srch_sel>-tabname
**             fname = <lwa_srch_sel>-fieldname.
**    IF sy-subrc NE 0.
**      lwa_spslf-srchp = ls_sp_infocus-srchp.
**      lwa_spslf-tname = <lwa_srch_sel>-tabname.
**      lwa_spslf-fname = <lwa_srch_sel>-fieldname.
**      lwa_spslf-spgrp = <lwa_srch_sel>-spgrp.
**      lwa_spslf-descr = <lwa_srch_sel>-descr.
**      lwa_spslf-postn = <lwa_srch_sel>-postn.
**      lwa_spslf-kind  = <lwa_srch_sel>-param_typ.
**      lwa_spslf-fltyp = <lwa_srch_sel>-fltyp.
**      lwa_spslf-shlpn = <lwa_srch_sel>-shlpn.
**      lwa_spslf-rollname = <lwa_srch_sel>-rollname.
**      APPEND lwa_spslf TO lt_spslf.
**    ENDIF.
**  ENDLOOP.
**
**  READ TABLE lt_spslf
**  TRANSPORTING NO FIELDS
**  WITH KEY spgrp = '1'.
**  IF sy-subrc EQ 0.
**    lwa_spgrp-srchp = ls_sp_infocus-srchp.
**    lwa_spgrp-spgrp = '1'.
**    lwa_spgrp-postn = '1'.
**    lwa_spgrp-descr = TEXT-001.
**    APPEND lwa_spgrp TO lt_spgrp.
**  ENDIF.
**
**  READ TABLE lt_spslf
**  TRANSPORTING NO FIELDS
**  WITH KEY spgrp = '2'.
**  IF sy-subrc EQ 0.
**    lwa_spgrp-srchp = ls_sp_infocus-srchp.
**    lwa_spgrp-spgrp = '2'.
**    lwa_spgrp-postn = '2'.
**    lwa_spgrp-descr = TEXT-002.
**    APPEND lwa_spgrp TO lt_spgrp.
**  ENDIF.
**
**  READ TABLE lt_spslf
**  TRANSPORTING NO FIELDS
**  WITH KEY spgrp = '3'.
**  IF sy-subrc EQ 0.
**    lwa_spgrp-srchp = ls_sp_infocus-srchp.
**    lwa_spgrp-spgrp = '3'.
**    lwa_spgrp-postn = '3'.
**    lwa_spgrp-descr = TEXT-028.
**    APPEND lwa_spgrp TO lt_spgrp.
**  ENDIF.
**
**  APPEND LINES OF lt_spslf TO ls_sp_infocus-x-spslf.
**  APPEND LINES OF lt_spgrp TO ls_sp_infocus-x-spgrp.

ENDFORM.
*}   INSERT
