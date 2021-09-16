*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0V.
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  VERSION_SELECTIONS_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM version_selections_add TABLES lt_selection TYPE /agri/t_bsel_values
                             USING lv_version   TYPE rdir_type.

  DATA: lwa_selection TYPE /agri/s_bsel_values,
        ls_srch_sel   TYPE /agri/s_gsrchp_sel,
        lv_exclude    TYPE xfeld.

  ls_srch_sel-fieldname = lwa_selection-fieldname = c_selname-max_hits.

  PERFORM selection_exclude USING ls_srch_sel
                                  lv_version
                         CHANGING lv_exclude.

  IF lv_exclude IS INITIAL.
    APPEND lwa_selection TO lt_selection.
  ENDIF.

ENDFORM.
*}   INSERT
