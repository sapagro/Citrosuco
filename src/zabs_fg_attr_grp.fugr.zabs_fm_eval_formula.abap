FUNCTION zabs_fm_eval_formula.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_FORMULA) TYPE  ZABS_DEL_FORMULA
*"     REFERENCE(IT_ATTRIBUTES) TYPE  ZABS_TTY_ATTR_FORML
*"  EXPORTING
*"     REFERENCE(EV_SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(EV_MESSAGE) TYPE  CHAR80
*"     REFERENCE(EV_VALUE) TYPE  ATFLV
*"----------------------------------------------------------------------

  gt_attributes = it_attributes.
  SORT gt_attributes BY ident.

  CALL FUNCTION 'CHECK_FORMULA'
    EXPORTING
      formula           = iv_formula
      program           = sy-repid
      routine           = 'GET_CHECK_VALUES'
    EXCEPTIONS
      error_in_formula  = 1
      missing_parameter = 2
      OTHERS            = 3.

  ev_subrc = sy-subrc.

  CHECK ev_subrc EQ 0.

  CALL FUNCTION 'EVAL_FORMULA'
    EXPORTING
      formula                 = iv_formula
      program                 = sy-repid
      routine                 = 'GET_EVAL_VALUES'
    IMPORTING
      value                   = ev_value
    EXCEPTIONS
      division_by_zero        = 1
      exp_error               = 2
      formula_table_not_valid = 3
      invalid_expression      = 4
      invalid_value           = 5
      log_error               = 6
      parameter_error         = 7
      sqrt_error              = 8
      units_not_valid         = 9
      missing_parameter       = 10
      OTHERS                  = 11.

  ev_subrc = sy-subrc.

ENDFUNCTION.
