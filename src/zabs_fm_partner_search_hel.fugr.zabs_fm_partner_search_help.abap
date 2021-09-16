FUNCTION zabs_fm_partner_search_help.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: ls_temp LIKE record_tab,
        lt_temp LIKE record_tab OCCURS 0.

  TYPES: BEGIN OF ty_but,
           partner TYPE bu_partner,
         END OF ty_but.

  DATA: lt_but TYPE TABLE OF ty_but,
        ls_but TYPE ty_but.

  IF callcontrol-step = 'DISP'.

    LOOP AT record_tab INTO ls_temp WHERE string+17(4) = '2000'.
      APPEND ls_temp TO lt_temp.
      ls_but-partner = ls_temp-string+3(10).
      APPEND ls_but TO lt_but.
      CLEAR: ls_temp.
    ENDLOOP.

    SELECT partner, mc_name1, mc_name2
      FROM but000 INTO TABLE @DATA(lt_but000)
       FOR ALL ENTRIES IN @lt_but
     WHERE partner EQ @lt_but-partner.
    IF sy-subrc EQ 0.
      SORT lt_but000 BY  partner.
    ENDIF.

    CLEAR: record_tab[].
    record_tab[] = lt_temp[].

    LOOP AT record_tab INTO ls_temp.
      READ TABLE lt_but000 INTO DATA(ls_but000)
      WITH KEY partner = ls_temp-string+3(10) BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE ls_temp-string
                    ls_but000-mc_name1
*                    ls_but000-mc_name2
               INTO ls_temp-string
       SEPARATED BY '      '.

        CONCATENATE ls_temp-string
                    ls_but000-mc_name2
               INTO ls_temp-string
       SEPARATED BY space.

        MODIFY record_tab FROM ls_temp TRANSPORTING string.
      ENDIF.
      CLEAR ls_but000.
    ENDLOOP.

    CLEAR: lt_temp[].

  ENDIF.

ENDFUNCTION.
