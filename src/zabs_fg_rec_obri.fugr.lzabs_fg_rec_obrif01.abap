*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_REC_OBRIF01.
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
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <vim_total_struc>
        TO FIELD-SYMBOL(<lv_matnr>).
      IF sy-subrc = 0.
        IF <lv_matnr> IS NOT INITIAL.
          SELECT SINGLE maktx
            INTO @DATA(lv_maktx)
            FROM makt
           WHERE matnr = @<lv_matnr>
             AND spras = @sy-langu.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <vim_total_struc>
              TO FIELD-SYMBOL(<lv_makt>).
            IF sy-subrc EQ 0.
              <lv_makt> = lv_maktx.
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
