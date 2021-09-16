*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0S.
*----------------------------------------------------------------------*

*{   INSERT         S4HK903321                                        1
*&---------------------------------------------------------------------*
*&      Form  SEARCH_SELECTIONS_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM search_selections_fill .

  DATA: ls_srch_sel TYPE /agri/s_gsrchp_sel.

  REFRESH: gt_srch_sel.

*** Header
  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_STRNO'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'STRNO'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '1'.
  ls_srch_sel-descr   = TEXT-003.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_PLTXT'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'PLTXT'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn  =  '2'.
  ls_srch_sel-descr   =  TEXT-004.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_FLTYP'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'FLTYP'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   =  '3'.
  ls_srch_sel-descr   =  TEXT-005.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_TPLVL'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'TPLVL'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '4'.
  ls_srch_sel-descr   = TEXT-006.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_PSPNR'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'PSPNR'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '5'.
  ls_srch_sel-descr   = TEXT-007.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_TPLKZ'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'TPLKZ'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '6'.
  ls_srch_sel-descr   = TEXT-008.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_TPLMA'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'TPLMA'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '7'.
  ls_srch_sel-descr   = TEXT-009.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_OWROL'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'OWROL'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '8'.
  ls_srch_sel-descr   = TEXT-010.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_OWNER'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'OWNER'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '9'.
  ls_srch_sel-descr   = TEXT-011.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_OWNSH'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'OWNSHP'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '10'.
  ls_srch_sel-descr   = TEXT-012.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_GAREA'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'GAREA'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '11'.
  ls_srch_sel-descr   = TEXT-013.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_EQFNR'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'EQFNR'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '12'.
  ls_srch_sel-descr   = TEXT-014.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_GRUP1'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'GRUP1'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '13'.
  ls_srch_sel-descr   = TEXT-015.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_GRUP2'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'GRUP2'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '14'.
  ls_srch_sel-descr   = TEXT-016.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_GRUP3'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'GRUP3'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '15'.
  ls_srch_sel-descr   = TEXT-017.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_STORT'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'STORT'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '16'.
  ls_srch_sel-descr   = TEXT-018.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_SWERK'.
  ls_srch_sel-tabname = c_tabname-iloa.
  ls_srch_sel-fieldname = 'SWERK'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '17'.
  ls_srch_sel-descr   = TEXT-019.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_BUKRS'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'BUKRS'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '18'.
  ls_srch_sel-descr   = TEXT-020.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_IWERK'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'IWERK'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '1'.
  ls_srch_sel-postn   = '19'.
  ls_srch_sel-descr   = TEXT-021.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_ERNAM'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'ERNAM'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '2'.
  ls_srch_sel-postn   = '1'.
  ls_srch_sel-descr   = TEXT-022.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_ERDAT'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'ERDAT'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '2'.
  ls_srch_sel-postn   = '2'.
  ls_srch_sel-descr   = TEXT-023.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_AENAM'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'AENAM'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '2'.
  ls_srch_sel-postn   = '3'.
  ls_srch_sel-descr   = TEXT-024.
  APPEND ls_srch_sel TO gt_srch_sel.

  CLEAR: ls_srch_sel.
  ls_srch_sel-name = 'SO_AEDAT'.
  ls_srch_sel-tabname = c_tabname-header.
  ls_srch_sel-fieldname = 'AEDAT'.
  ls_srch_sel-param_typ = 'S'.
  ls_srch_sel-spgrp   = '2'.
  ls_srch_sel-postn   = '4'.
  ls_srch_sel-descr   = TEXT-025.
  APPEND ls_srch_sel TO gt_srch_sel.

*** Parameters
  CLEAR: ls_srch_sel.
  ls_srch_sel-name = c_selname-max_hits.
  ls_srch_sel-tabname = '1'.
  ls_srch_sel-fieldname = 'MXHIT'.
  ls_srch_sel-param_typ = 'P'.
  ls_srch_sel-spgrp   = '3'.
  ls_srch_sel-postn   = '1'.
  ls_srch_sel-descr   = TEXT-026.
  ls_srch_sel-default_value = '500'.
  APPEND ls_srch_sel TO gt_srch_sel.

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  SELECTION_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection_exclude  USING ls_srch_sel LIKE ls_srchp_sel
                              lv_version  TYPE rdir_type
                     CHANGING lv_exclude  TYPE xfeld.

  CLEAR: lv_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_OPTIONS_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM search_options_fill  TABLES lt_selection TYPE /agri/t_bsel_values
                           USING lv_version   TYPE rdir_type.

  DATA: lv_exclude TYPE xfeld,
        lv_so_name TYPE string,
        lv_tabix   TYPE sy-tabix.

*  DATA: lref_so TYPE REF TO data,
*        lref_so_row TYPE REF TO data,
*        lref_srch_sel TYPE REF TO data.

  FIELD-SYMBOLS: <lt_rssel>      TYPE table,
                 <lwa_rssel>     TYPE any,
                 <lwa_srch_sel>  TYPE /agri/s_gsrchp_sel,
                 <lwa_selection> TYPE /agri/s_bsel_values,
                 <lwa_selopt>    TYPE /agri/s_bsel_selopt.

  LOOP AT gt_srch_sel ASSIGNING <lwa_srch_sel>.
    UNASSIGN: <lwa_selection>.
    CLEAR: lv_tabix,
           lv_so_name.

    PERFORM selection_exclude USING <lwa_srch_sel>
                                    lv_version
                           CHANGING lv_exclude.

    CHECK lv_exclude IS INITIAL.

    READ TABLE lt_selection ASSIGNING <lwa_selection>
    WITH KEY tabname = <lwa_srch_sel>-tabname
             fieldname = <lwa_srch_sel>-fieldname.

    IF <lwa_selection> IS NOT ASSIGNED.
      READ TABLE lt_selection ASSIGNING <lwa_selection>
      WITH KEY tabname = <lwa_srch_sel>-tabname
               fieldname = <lwa_srch_sel>-name.

    ENDIF.

    IF <lwa_selection> IS ASSIGNED.
      lv_tabix = sy-tabix.

      UNASSIGN: <lwa_rssel>,
                <lt_rssel>.

      IF <lwa_srch_sel>-param_typ EQ 'P'.
****Parameters
        ASSIGN (<lwa_srch_sel>-name) TO <lwa_rssel>.
        CHECK <lwa_rssel> IS ASSIGNED.

        READ TABLE <lwa_selection>-selopt ASSIGNING <lwa_selopt>
        INDEX 1.
        IF sy-subrc EQ 0
        AND <lwa_selopt> IS ASSIGNED.
          <lwa_rssel> = <lwa_selopt>-low.
        ENDIF.

      ELSE. " IF <lwa_srch_sel>-param_typ EQ 'S'.
****select options
        CONCATENATE <lwa_srch_sel>-name '[]' INTO lv_so_name.
        ASSIGN (lv_so_name) TO <lt_rssel>.
        CHECK <lt_rssel> IS ASSIGNED.

        LOOP AT <lwa_selection>-selopt ASSIGNING <lwa_selopt>.
          UNASSIGN: <lwa_rssel>.

          APPEND INITIAL LINE TO <lt_rssel> ASSIGNING <lwa_rssel>.
          CHECK <lwa_rssel> IS ASSIGNED.
          MOVE-CORRESPONDING <lwa_selopt> TO <lwa_rssel>.
          UNASSIGN: <lwa_rssel>.
        ENDLOOP.

      ENDIF.

      DELETE lt_selection INDEX lv_tabix.

    ENDIF.
  ENDLOOP.

ENDFORM.
