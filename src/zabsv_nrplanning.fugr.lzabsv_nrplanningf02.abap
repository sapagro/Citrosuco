*----------------------------------------------------------------------*
***INCLUDE LZABSV_NRPLANNINGF02.
*----------------------------------------------------------------------*

FORM fill_hidden_fields.

  SELECT SINGLE name1
        FROM t001w
        INTO @DATA(lv_name2)
        WHERE werks EQ @zabsv_nrplanning-rwerks.
  IF sy-subrc = 0.
    zabsv_nrplanning-name2 = lv_name2.
  ELSE.
    CLEAR zabsv_nrplanning-name2.
  ENDIF.

  zabsv_nrplanning-ernam = sy-uname.
  zabsv_nrplanning-erdat = sy-datum.
  zabsv_nrplanning-erzet = sy-uzeit.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module GET_DESCRIPTIONS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE get_descriptions OUTPUT.

  PERFORM get_descriptions.

ENDMODULE.

FORM get_descriptions.

  LOOP AT total.

    ASSIGN: COMPONENT 'RWERKS' OF STRUCTURE total
            TO FIELD-SYMBOL(<lfs_werks>),
            COMPONENT 'NAME2' OF STRUCTURE total
            TO FIELD-SYMBOL(<lfs_name2>).

    IF <lfs_werks> IS ASSIGNED AND <lfs_name2> IS ASSIGNED.
      SELECT SINGLE name1
        FROM t001w
        INTO @DATA(lv_name2)
        WHERE werks EQ @<lfs_werks>.
      IF sy-subrc = 0.
        <lfs_name2> = lv_name2.
      ELSE.
        CLEAR <lfs_name2>.
      ENDIF.
    ENDIF.

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc EQ 0.
      extract = total.
      MODIFY extract INDEX sy-tabix.
    ENDIF.
    MODIFY total.

  ENDLOOP.

ENDFORM.

FORM admin_data_fill.

 FIELD-SYMBOLS : <fs_field> TYPE ANY .

  LOOP AT total .
    CHECK <action> EQ aendern.
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

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc = 0.
      extract = total .
      MODIFY extract INDEX sy-tabix.
    ENDIF.

    MODIFY total.
  ENDLOOP.

ENDFORM.
