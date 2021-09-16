*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_USR_CENTF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  f_event_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_event_01.

*-- TOTAL contains all data which are read, changed and created in TMG
  LOOP AT total.
    IF <action> = neuer_eintrag " New Entry.
    OR <action> = aendern "Changed entry
    OR <action> = original. "Same as DB
      ASSIGN COMPONENT 'BNAME' OF STRUCTURE <vim_total_struc>
        TO FIELD-SYMBOL(<lv_bname>).
      IF sy-subrc = 0.
        IF <lv_bname> IS NOT INITIAL.
          SELECT p~name_text
            UP TO 1 ROWS
            FROM adrp AS p
            INNER JOIN usr21 AS u
            ON  u~persnumber EQ p~persnumber
            INTO @DATA(lv_name_txt)
            WHERE u~bname EQ @<lv_bname>.
          ENDSELECT.

          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'NAME_TEXT' OF STRUCTURE <vim_total_struc>
              TO FIELD-SYMBOL(<lv_name_txt>).
            IF sy-subrc EQ 0.
              <lv_name_txt> = lv_name_txt.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <vim_total_struc>
        TO FIELD-SYMBOL(<lv_werks>).
      IF sy-subrc = 0.
        IF <lv_werks> IS NOT INITIAL.
          SELECT SINGLE name1
            INTO @DATA(lv_werks_txt)
            FROM t001w
           WHERE werks = @<lv_werks>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'NAME_WERKS' OF STRUCTURE <vim_total_struc>
              TO FIELD-SYMBOL(<lv_werks_txt>).
            IF sy-subrc EQ 0.
              <lv_werks_txt> = lv_werks_txt.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE extract WITH KEY <vim_xtotal_key>.
      IF sy-subrc = 0.
        extract = total.
        MODIFY extract INDEX sy-index.
      ENDIF.

      IF total IS NOT INITIAL.
        MODIFY total.
      ENDIF.
    ENDIF.
  ENDLOOP.

  sy-subrc = 0.

ENDFORM.                                                    "f_event_01
