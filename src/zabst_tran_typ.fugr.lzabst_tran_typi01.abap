*----------------------------------------------------------------------*
***INCLUDE LZABST_TRAN_TYPI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validation INPUT.

*Local Declarations
  TYPES: BEGIN OF ty_total.
      INCLUDE STRUCTURE zabsv_tran_typ.
  TYPES: action TYPE c,
         mark   TYPE c,
         END OF ty_total.

  DATA: lt_zabst_tran_typ TYPE STANDARD TABLE OF zabst_tran_typ,
        ls_zabst_tran_typ TYPE zabst_tran_typ,
        lt_total          TYPE STANDARD TABLE OF ty_total, " zabst_tran_typ,
        ls_total          TYPE ty_total, " zabst_tran_typ,
        msg1              TYPE string,
        lv_m1             TYPE c,
        lv_m2             TYPE c,
        lv_m3             TYPE c.

  FIELD-SYMBOLS: <fs_zabst_tran_typ> TYPE zabst_tran_typ,
                 <fs_total>          TYPE any,
                 <ft_total>          TYPE ANY TABLE.

  IF sy-ucomm NE 'NEWL'." OR sy-ucomm ne 'SAVE'. " 'SAVE'.
    IF sy-ucomm NE 'SAVE'. " 'SAVE'.

      REFRESH: lt_zabst_tran_typ.
      IF total[] IS NOT INITIAL.
        REFRESH: lt_total.
      ELSEIF total[] IS INITIAL.
        total[] = lt_total[].
      ENDIF.
*Storing newly created entries in LT_ZABST_TRAN_TYP
      LOOP AT total.
        IF <vim_total_struc> IS ASSIGNED AND <action> = 'N'. " ( <action> = 'N' OR <action> = 'U').
          MOVE-CORRESPONDING <vim_total_struc> TO ls_zabst_tran_typ.
          APPEND ls_zabst_tran_typ TO lt_zabst_tran_typ.
          CLEAR: ls_zabst_tran_typ.
*      ENDIF.
          MOVE-CORRESPONDING <vim_total_struc> TO ls_total. " <fs_total>.
          MOVE-CORRESPONDING <vim_total_struc> TO ls_total. " <fs_total>.
          ls_total-action = 'N'.
          APPEND ls_total TO lt_total.
          CLEAR ls_total.
        ELSE.
          MOVE-CORRESPONDING <vim_total_struc> TO ls_total. " <fs_total>.
          APPEND ls_total TO lt_total.
          CLEAR ls_total.
        ENDIF.
      ENDLOOP.

*Fetching Partner from table BUT0IS to validate newly added records.
      IF lt_zabst_tran_typ IS NOT INITIAL.
*      REFRESH total.
        SELECT partner, ind_sector
          FROM but0is
          INTO TABLE @DATA(lt_but0is)
          FOR ALL ENTRIES IN @lt_zabst_tran_typ
          WHERE partner EQ @lt_zabst_tran_typ-int_tr_prov
            AND ind_sector EQ '2000'.
        IF sy-subrc IS INITIAL.
          SORT lt_but0is BY partner.
        ENDIF.

        CLEAR:lv_m1, lv_m2, lv_m3.
        UNASSIGN <fs_zabst_tran_typ>.

        LOOP AT lt_zabst_tran_typ ASSIGNING <fs_zabst_tran_typ>.
          DATA(lv_tabix) = sy-tabix.
          READ TABLE lt_but0is TRANSPORTING NO FIELDS
          WITH KEY partner = <fs_zabst_tran_typ>-int_tr_prov BINARY SEARCH.

          IF sy-subrc IS NOT INITIAL.
            vim_abort_saving = abap_true.

            CONCATENATE TEXT-001 <fs_zabst_tran_typ>-int_tr_prov
                        TEXT-004 INTO msg1
              SEPARATED BY space.

            DATA(lt_dummy_tot) = lt_total.
            DELETE lt_dummy_tot WHERE action ='N'.
            REFRESH total[].
            total[] = lt_dummy_tot[].

            MESSAGE msg1 TYPE 'E'.
          ENDIF.

          IF  ( <fs_zabst_tran_typ>-loader EQ abap_true AND <fs_zabst_tran_typ>-in_licplate IS NOT INITIAL ).
            lv_m2 = abap_true.
          ELSEIF ( <fs_zabst_tran_typ>-loader EQ abap_false AND <fs_zabst_tran_typ>-in_licplate IS INITIAL ).

            DATA(lt_dummy_total) = lt_total.
            DELETE lt_dummy_total WHERE action ='N'.
            REFRESH total[].
            total[] = lt_dummy_total[].
*            DELETE total from ls_total. " results dump
            MESSAGE e368(00) WITH TEXT-003.
          ENDIF.

        ENDLOOP.

**    IF lv_m2 EQ abap_true.
**      MESSAGE i368(00) WITH TEXT-002.
**      CLEAR: lv_m2.
**    ENDIF.

        UNASSIGN <fs_zabst_tran_typ>.

      ENDIF.

****  ELSEIF sy-ucomm EQ 'SAVE'.
****    total[] = lt_total[].

    ENDIF.
  ENDIF.

ENDMODULE.
