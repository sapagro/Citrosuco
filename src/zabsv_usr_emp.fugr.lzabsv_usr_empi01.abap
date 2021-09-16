*----------------------------------------------------------------------*
***INCLUDE LZABSV_USR_EMPI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module  PERNR_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& PERNR_VALIDATION
*----------------------------------------------------------------------*
MODULE pernr_validation INPUT.

  IF zabsv_usr_emp-pernr IS INITIAL AND
    ( zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_leader "'LE'
    OR zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_incharge  )."'IN'
    MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module  LIFNR_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& LIFNR_VALIDATION
*----------------------------------------------------------------------*
MODULE lifnr_validation INPUT.

  IF zabsv_usr_emp-lifnr IS INITIAL
    AND zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_loader. "'LD'
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module  UROLE_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& UROLE_VALIDATION
*----------------------------------------------------------------------*
MODULE urole_validation INPUT.

  IF zabsv_usr_emp-urole EQ space.
    MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
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
*& Module  PEVT_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& PEVT_VALIDATION
*----------------------------------------------------------------------*
MODULE pevt_validation INPUT.

  IF zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_leader "'LE'
      OR zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_incharge ."'IN'
    IF zabsv_usr_emp-pevt IS INITIAL.
      MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

  IF zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_loader "'LD'
   OR zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_bin. "'BN'
    IF zabsv_usr_emp-pevt IS NOT INITIAL.
      MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module  FLEET_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& FLEET_VALIDATION
*----------------------------------------------------------------------*
MODULE fleet_validation INPUT.

  IF zabsv_usr_emp-fleet IS INITIAL AND
     zabsv_usr_emp-urole = zcl_abs_abap_maintain=>c_role_equip. "'EQ'
    MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
  ENDIF.

ENDMODULE.
