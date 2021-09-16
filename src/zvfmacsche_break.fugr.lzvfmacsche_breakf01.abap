*----------------------------------------------------------------------*
***INCLUDE LZVFMACSCHE_BREAKF01.
*----------------------------------------------------------------------*

FORM check_keyfields.

  DATA: lt_dfies TYPE TABLE OF dfies,
        lv_text  TYPE dfies-fieldtext.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZFMACSCHED_BREAK'
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS ##FM_SUBRC_OK
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  SORT lt_dfies BY keyflag DESCENDING.

  LOOP AT total.
    DATA(lv_tabix) = sy-tabix.
*...NEUER (value “N”) – new entry
*...AENDERN (value “U”) – updated entry
*...GELOESCHT (value “D”) – deleted entry
*...ORIGINAL (value “”) – unchanged entry
*...NEUER_GELOESCHT (value “X”) – new entry, deleted again
*...UPDATE_GELOESCHT (value “Y”) – updated entry, deleted again
    IF <action> EQ neuer " new entry
    OR <action> EQ aendern. " updated entry
      READ TABLE extract WITH KEY <vim_xtotal_key>.
      LOOP AT lt_dfies INTO DATA(lwa_dfies).
        IF lwa_dfies-keyflag NE abap_true.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lwa_dfies-fieldname OF STRUCTURE <vim_total_struc>
          TO FIELD-SYMBOL(<lv_keyfield>).
        IF sy-subrc EQ 0.
          IF <lv_keyfield> IS INITIAL.
* Feldbeschreibung lesen
            CALL FUNCTION 'TB_DATAELEMENT_GET_TEXTS'
              EXPORTING
                name        = lwa_dfies-rollname
              IMPORTING
                description = lv_text
              EXCEPTIONS
                not_found   = 1
                OTHERS      = 2.
            IF sy-subrc EQ 0.
              DELETE total INDEX lv_tabix.
* O campo &1 é de preenchimento obrigatório!
              MESSAGE i036(zfmfp) WITH lv_text DISPLAY LIKE 'I'.
              sy-subrc = 4.
              vim_abort_saving = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.
