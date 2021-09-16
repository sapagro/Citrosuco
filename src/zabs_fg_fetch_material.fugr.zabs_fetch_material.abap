FUNCTION zabs_fetch_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_TIPO_MATERIAL) TYPE  MTART DEFAULT 'ZPIC'
*"  EXPORTING
*"     VALUE(ET_MATERIAIS) TYPE  ZABS_TTY_MATERIAIS
*"----------------------------------------------------------------------

  DATA: lr_atnam TYPE RANGE OF atnam.

  CONSTANTS: lc_classe_material TYPE klassenart VALUE '001'.

  DO 2 TIMES.
    DATA(lv_index) = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_atnam
      ASSIGNING FIELD-SYMBOL(<lwa_atnam>).
    IF sy-subrc EQ 0.
      <lwa_atnam> = 'IEQ'.
      CASE lv_index.
        WHEN 1.
          <lwa_atnam>-low = 'FAZ_DIAS_REENTRADA'.
        WHEN 2.
          <lwa_atnam>-low = 'FAZ_DIAS_CARENCIA'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SELECT m~matnr, m~lvorm, m~mtart, t~spras, t~maktx,
         a~objek, a~atinn, a~atzhl, a~mafid, a~klart, a~adzhl, a~datub,
         a~dec_value_from, a~dec_value_to, c~atnam
    FROM mara AS m
    LEFT OUTER JOIN makt AS t
    ON m~matnr = t~matnr
    INNER JOIN ausp AS a
    ON m~matnr = a~objek
    INNER JOIN cabn AS c
    ON c~atinn = a~atinn AND
    c~adzhl = a~adzhl
    INTO TABLE @DATA(lt_mara)
   WHERE m~lvorm EQ @abap_false
     AND m~mtart EQ @iv_tipo_material
     AND t~spras EQ @sy-langu
     AND a~klart EQ @lc_classe_material
     AND a~datub GE @sy-datum
     AND c~atnam IN @lr_atnam[].

  LOOP AT lt_mara INTO DATA(lwa_mara).
    INSERT INITIAL LINE INTO TABLE et_materiais
      ASSIGNING FIELD-SYMBOL(<lwa_material>).
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_mara TO <lwa_material>.

      IF <lwa_material>-matnr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = <lwa_material>-matnr
          IMPORTING
            output = <lwa_material>-matnr.
      ENDIF.

      IF <lwa_material>-atinn IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
          EXPORTING
            input  = <lwa_material>-atinn
          IMPORTING
            output = <lwa_material>-atinn.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
