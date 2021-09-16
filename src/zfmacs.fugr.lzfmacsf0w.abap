*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0W.
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  WHERE_CLAUSE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM where_clause_prepare  TABLES lt_selection        TYPE /agri/t_bsel_values
                         CHANGING ls_flot_where_tab   TYPE rsds_where_tab
                                  ls_iflot_where_tab  TYPE rsds_where_tab
                                  ls_adrc_where_tab   TYPE rsds_where_tab
                                  ls_iloa_where_tab   TYPE rsds_where_tab
                                  ls_iflotx_where_tab TYPE rsds_where_tab
                                  ls_flatg_where_tab  TYPE rsds_where_tab
                                  ls_flatv_where_tab  TYPE rsds_where_tab
                                  ls_flown_where_tab  TYPE rsds_where_tab.

****get header where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-header
      i_prefix     = 'A'
*     i_table_alias = 'A'
    IMPORTING
*     et_where_pfx = ls_cmhdr_where_pfx
      et_where     = ls_flot_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get IFLOT where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-iflot
      i_prefix     = 'B'
*     i_table_alias = 'B'
    IMPORTING
*     et_where_pfx = ls_cmhdrt_where_pfx
      et_where     = ls_iflot_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get ADRC where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-adrc
      i_prefix     = 'C'
*     i_table_alias = 'B'
    IMPORTING
*     et_where_pfx = ls_cmwrk_where_pfx
      et_where     = ls_adrc_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get ILOA where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-iloa
      i_prefix     = 'D'
*     i_table_alias = 'D'
    IMPORTING
*     et_where_pfx = ls_cmqch_where_pfx
      et_where     = ls_iloa_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get IFLOTX where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-iflotx
      i_prefix     = 'E'
*     i_table_alias = 'C'
    IMPORTING
*     et_where_pfx = ls_cmvar_where_pfx
      et_where     = ls_iflotx_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get /AGRI/GLFLATG where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-flatg
      i_prefix     = 'F'
*     i_table_alias = 'F'
    IMPORTING
*     et_where_pfx = ls_cmprs_where_pfx
      et_where     = ls_flatg_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get /AGRI/GLFLATV where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-flatv
      i_prefix     = 'G'
*     i_table_alias = 'G'
    IMPORTING
*     et_where_pfx = ls_cmpvr_where_pfx
      et_where     = ls_flatv_where_tab
    CHANGING
      ct_selection = lt_selection[].

****get /AGRI/GLFLOWN where clause tab
  CALL METHOD /agri/cl_bsel_services=>selection_to_where_clause_prep
    EXPORTING
      i_table_name = c_tabname-flown
      i_prefix     = 'H'
*     i_table_alias = 'H'
    IMPORTING
*     et_where_pfx = ls_cmprso_where_pfx
      et_where     = ls_flown_where_tab
    CHANGING
      ct_selection = lt_selection[].

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  WHERE_TABLE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM where_table_prepare  TABLES   lt_header_sel_opt
                                   lt_iloa_sel_opt
                          USING    lv_hdr_sel_specified
                                   lv_iloa_sel_specified.

  DATA: lwa_select_option     TYPE char72.

  IF lv_hdr_sel_specified EQ c_true.
    lwa_select_option     = 'a~pltxt in so_pltxt'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~tplkz in so_tplkz'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~fltyp in so_fltyp'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~tplvl in so_tplvl'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~tplma in so_tplma'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~pspnr in so_pspnr'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~bukrs in so_bukrs'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~iwerk in so_iwerk'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~garea in so_garea'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~stort in so_stort'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~grup1 in so_grup1'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~grup2 in so_grup2'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~grup3 in so_grup3'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~owrol in so_owrol'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~owner in so_owner'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~ownshp in so_ownsh'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~stort in so_stort'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~eqfnr in so_eqfnr'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~ernam in so_ernam'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~erdat in so_erdat'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~aenam in so_aenam'.
    APPEND lwa_select_option TO lt_header_sel_opt[].
    lwa_select_option = 'and a~aedat in so_aedat'.
    APPEND lwa_select_option TO lt_header_sel_opt[].

  ENDIF.

  IF lv_iloa_sel_specified EQ c_true.
    lwa_select_option     = 'a~swerk in so_swerk'.
    APPEND lwa_select_option TO lt_iloa_sel_opt[].
  ENDIF.

ENDFORM.
