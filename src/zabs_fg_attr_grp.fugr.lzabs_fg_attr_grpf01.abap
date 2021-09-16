*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_ATTR_GRPF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_CHECK_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_check_values USING iv_name
                   CHANGING cv_subrc.

  cv_subrc = 4.

  READ TABLE gt_attributes INTO DATA(ls_attributes)
                           WITH KEY ident = iv_name
                           BINARY SEARCH.
  IF sy-subrc EQ 0.
    cv_subrc = 0.
  ELSE.
    gs_variables-subrc = 4.
    gs_variables-ident = iv_name.
  ENDIF.

ENDFORM.                    "GET_CHECK_VALUES

*&---------------------------------------------------------------------*
*&      Form  GET_EVAL_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_eval_values USING VALUE(iv_name)
                  CHANGING VALUE(cv_value)
                           VALUE(cv_subrc).

*-- Local Workareas
  DATA:
    ls_attributes TYPE zabs_str_attr_forml.

*-- Local Variables
  DATA:
    lv_value      TYPE p DECIMALS 3.

  cv_subrc = 0.
  cv_value = 0.

  CLEAR ls_attributes.
  READ TABLE gt_attributes INTO ls_attributes
      WITH KEY ident = iv_name
    BINARY SEARCH.
  IF sy-subrc EQ 0.
    lv_value = ls_attributes-atflv.
    cv_value = lv_value.
  ENDIF.

ENDFORM.                    "GET_EVAL_VALUES
