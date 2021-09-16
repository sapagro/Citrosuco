*----------------------------------------------------------------------*
***INCLUDE LZABSV_USRAPPI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module  UROLE_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& UROLE_VALIDATION
*----------------------------------------------------------------------*
MODULE urole_validation INPUT.

  IF zabsv_usrapp-urole EQ space.
    MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module  APP_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& APP_VALIDATION
*----------------------------------------------------------------------*
MODULE app_validation INPUT.

  IF zabsv_usrapp-app EQ space.
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
  ENDIF.

ENDMODULE.

FORM before_save.

  FIELD-SYMBOLS : <fs_field> TYPE any.

  LOOP AT total.
    CASE <action>.
      WHEN neuer_eintrag.

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

ENDFORM.
