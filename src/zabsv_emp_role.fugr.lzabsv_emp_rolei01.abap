*----------------------------------------------------------------------*
***INCLUDE LZABSV_EMP_ROLEI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module  PEVT_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& PEVT_VALIDATION
*----------------------------------------------------------------------*
MODULE pevt_validation INPUT.

  IF zabsv_emp_role-urole = zcl_abs_abap_maintain=>c_role_leader "'LE'
"Begin of José Sequeira - 06.04.2021 16:36:10
  OR zabsv_emp_role-urole = 'IA'"'IA'
"End of José Sequeira - 06.04.2021 16:36:10
  OR zabsv_emp_role-urole = zcl_abs_abap_maintain=>c_role_incharge ."'IN'
    IF zabsv_emp_role-pevt IS INITIAL.
      MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
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

*&---------------------------------------------------------------------*
*& Module  FPCNF_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& FPCNF_VALIDATION
*----------------------------------------------------------------------*
MODULE fpcnf_validation INPUT.

  IF NOT ( zabsv_emp_role-urole = zcl_abs_abap_maintain=>c_role_leader OR "'LE'
"Begin of José Sequeira - 06.04.2021 16:36:53
           zabsv_emp_role-urole = 'IA' OR
"End of José Sequeira - 06.04.2021 16:36:53
           zabsv_emp_role-urole = zcl_abs_abap_maintain=>c_role_incharge OR
           zabsv_emp_role-urole = space )."'IN'
    IF zabsv_emp_role-fpcnf IS INITIAL.
      MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

ENDMODULE.
