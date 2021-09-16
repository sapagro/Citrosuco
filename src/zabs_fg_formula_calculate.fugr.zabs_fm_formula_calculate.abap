FUNCTION zabs_fm_formula_calculate.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_FORMULA) TYPE  ZABS_DEL_FORMULA
*"     REFERENCE(IV_POINTPRES) TYPE  ZABS_DEL_GEN_VALUE OPTIONAL
*"     REFERENCE(IV_NUMPLANTS) TYPE  ZABS_DEL_GEN_VALUE OPTIONAL
*"     REFERENCE(IV_TOTALVALUE) TYPE  ZABS_DEL_GEN_VALUE OPTIONAL
*"  EXPORTING
*"     REFERENCE(IV_TOTAL_VALUE) TYPE  ZABS_DEL_TOTAL_VALUE
*"----------------------------------------------------------------------

  DATA: lt_variables    TYPE zabs_tty_forml_variables,
        lx_root         TYPE REF TO cx_root,
        lv_formula      TYPE zabs_del_formula,
        lv_forml_string TYPE char255,
        lv_forml_length TYPE i,
        lv_retcode      TYPE sy-subrc,
        lv_funcname(30) TYPE c,
        lv_message(70)  TYPE c,
        lv_pos          TYPE i,
        lv_float_value  TYPE f,
*        lv_value        TYPE p LENGTH 15 DECIMALS 2,
        lv_value        TYPE zabs_del_total_value,
        lv_dump(1).

*-- Assign the values for the formula
  INSERT INITIAL LINE INTO TABLE lt_variables
    ASSIGNING FIELD-SYMBOL(<ls_variable>).
  IF sy-subrc EQ 0.
    <ls_variable>-var   = 'P'. "POINTPRESENCE
    <ls_variable>-value = iv_pointpres.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lt_variables
    ASSIGNING <ls_variable>.
  IF sy-subrc EQ 0.
    <ls_variable>-var   = 'N'. "NUMPLANTS
    <ls_variable>-value = iv_numplants.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lt_variables
    ASSIGNING <ls_variable>.
  IF sy-subrc EQ 0.
    <ls_variable>-var   = 'T'. "TOTALVALUE
    <ls_variable>-value = iv_totalvalue.
  ENDIF.

  lv_formula = iv_formula.

  REPLACE ALL OCCURRENCES OF 'POINTPRESENCE' IN lv_formula WITH 'P'.
  REPLACE ALL OCCURRENCES OF 'NUMPLANTS' IN lv_formula WITH 'N'.
  REPLACE ALL OCCURRENCES OF 'TOTALVALUE' IN lv_formula WITH 'T'.
  REPLACE ALL OCCURRENCES OF '(' IN lv_formula WITH '%(%'.
  REPLACE ALL OCCURRENCES OF '{' IN lv_formula WITH '%(%'.
  REPLACE ALL OCCURRENCES OF '[' IN lv_formula WITH '%(%'.
  REPLACE ALL OCCURRENCES OF ')' IN lv_formula WITH '%)%'.
  REPLACE ALL OCCURRENCES OF '}' IN lv_formula WITH '%)%'.
  REPLACE ALL OCCURRENCES OF ']' IN lv_formula WITH '%)%'.
  REPLACE ALL OCCURRENCES OF '%' IN lv_formula WITH space.

  lv_forml_string = lv_formula.
  CONDENSE lv_forml_string NO-GAPS.
  lv_forml_length = strlen( lv_forml_string ).

  SORT lt_variables BY var.
  DO lv_forml_length TIMES.
    sy-index = sy-index - 1.
    READ TABLE lt_variables ASSIGNING <ls_variable>
      WITH KEY var = lv_forml_string+sy-index(1) BINARY SEARCH.
    IF sy-subrc = 0.
      REPLACE ALL OCCURRENCES OF lv_forml_string+sy-index(1)
        IN lv_formula WITH <ls_variable>-value.
    ENDIF.
  ENDDO.

  IF NOT lv_formula IS INITIAL.
    CALL FUNCTION 'CHECK_FORMULA'
      EXPORTING
        formula  = lv_formula
      IMPORTING
        subrc    = lv_retcode
        funcname = lv_funcname
        message  = lv_message
        pos      = lv_pos.

    IF lv_retcode IS INITIAL.
      TRY.
          CALL FUNCTION 'EVAL_FORMULA'
            EXPORTING
              formula                 = lv_formula
            IMPORTING
              value                   = lv_float_value
            EXCEPTIONS
              division_by_zero        = 1
              exp_error               = 2
              invalid_expression      = 3
              invalid_value           = 4
              log_error               = 5
              parameter_error         = 6
              sqrt_error              = 7
              units_not_valid         = 8
              formula_table_not_valid = 9.
*-- Error in Formula
        CATCH cx_root INTO lx_root.                      "#EC CATCH_ALL
          lv_dump  = abap_true.
      ENDTRY.
      IF sy-subrc EQ 0.
        MOVE lv_float_value TO lv_value.
        iv_total_value = lv_value.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
