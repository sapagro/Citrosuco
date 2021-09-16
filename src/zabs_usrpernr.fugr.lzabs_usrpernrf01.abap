*----------------------------------------------------------------------*
***INCLUDE LZABS_USRPERNRF01.
*----------------------------------------------------------------------*
FORM after_input.

  CONSTANTS lc_memid TYPE c LENGTH 7 VALUE 'ZZROTID'.

  DATA: lv_route TYPE /agri/gl_route.

  IF zabs_v_usrpernr-route IS INITIAL.
    IMPORT lv_dummy TO lv_route FROM MEMORY ID lc_memid.
    IF sy-subrc = 0.
      zabs_v_usrpernr-route = lv_route.
    ENDIF.
  ENDIF.

ENDFORM. "after_input
FORM before_save.

  CONSTANTS lc_memid TYPE c LENGTH 7 VALUE 'ZZROTID'.

  DATA: lv_route TYPE /agri/gl_route.

  FIELD-SYMBOLS <fs_field> TYPE any.

  IMPORT lv_dummy TO lv_route FROM MEMORY ID lc_memid.

  LOOP AT total.
    CASE <action>.
      WHEN neuer_eintrag.

        ASSIGN COMPONENT 'ROUTE'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          IF <fs_field> IS INITIAL
            AND lv_route IS NOT INITIAL.
            <fs_field> = lv_route.
          ELSEIF <fs_field> IS INITIAL.
            MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
            vim_abort_saving = abap_true.
            sy-subrc = 4.
            EXIT.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT 'ERNAM'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uname .
        ENDIF.

        ASSIGN COMPONENT 'ERDAT'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-datum .
        ENDIF.

        ASSIGN COMPONENT 'ERZET'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uzeit .
        ENDIF.

      WHEN aendern.

        ASSIGN COMPONENT 'AENAM'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uname .
        ENDIF.

        ASSIGN COMPONENT 'AEDAT'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-datum .
        ENDIF.

        ASSIGN COMPONENT 'AEZET'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uzeit .
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc = 0.
      extract = total .
      MODIFY extract INDEX sy-tabix.
    ENDIF.

    MODIFY total.
  ENDLOOP.

ENDFORM. "after_input
FORM at_exit.

  CONSTANTS lc_memid TYPE c LENGTH 7 VALUE 'ZZROTID'.

  FREE MEMORY ID  lc_memid.

ENDFORM.
