*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0A .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ATTR_VALUES_TABLES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM attr_values_tables_prepare
*                        USING lwa_flatg TYPE /agri/s_glflatg
*                              lt_flatv  TYPE /agri/t_glflatv
*                              lt_athdr  TYPE /agri/t_gathdr
*                     CHANGING lt_attr_values_num TYPE tt_bapi1003_alloc_values_num
*                              lt_attr_values_char TYPE tt_bapi1003_alloc_values_char
*                              lt_attr_values_curr TYPE tt_bapi1003_alloc_values_curr.
*
*  DATA: lwa_athdr TYPE /agri/s_gathdr,
*        lwa_flatv TYPE /agri/s_glflatv,
*        lwa_attr_values_num  TYPE bapi1003_alloc_values_num,
*        lwa_attr_values_char TYPE bapi1003_alloc_values_char,
*        lwa_attr_values_curr TYPE bapi1003_alloc_values_curr.

*  REFRESH: lt_attr_values_num,
*           lt_attr_values_char,
*           lt_attr_values_curr.
*
*  LOOP AT lt_flatv INTO lwa_flatv
*                  WHERE class = lwa_flatg-class.
*
*    CLEAR: lwa_attr_values_num,
*           lwa_attr_values_char,
*           lwa_attr_values_curr,
*           lwa_athdr.
*
*    READ TABLE lt_athdr INTO lwa_athdr
*                        WITH KEY atinn = lwa_flatv-atinn
*                      BINARY SEARCH.
*
*    CASE lwa_athdr-atfor.
*      WHEN c_atfor-numeric
*        OR c_atfor-date
*        OR c_atfor-time.
*
*        lwa_attr_values_num-charact = lwa_athdr-atnam.
*        lwa_attr_values_num-value_from = lwa_flatv-atflv.
*        lwa_attr_values_num-value_to = lwa_flatv-atflb.
*        lwa_attr_values_num-value_relation  = lwa_flatv-atcod.
*        lwa_attr_values_num-unit_from = lwa_flatv-atawe.
*        lwa_attr_values_num-unit_to = lwa_flatv-ataw1.
*        lwa_attr_values_num-instance = lwa_flatv-atzis.
*        lwa_attr_values_num-charact_descr = lwa_flatv-atbez.
*
*        APPEND lwa_attr_values_num TO lt_attr_values_num.
*
*      WHEN c_atfor-char.
*
*        IF lwa_athdr-atkle IS INITIAL.
*          SET LOCALE LANGUAGE sy-langu.
*          TRANSLATE lwa_flatv-atwrt TO UPPER CASE.
*        ENDIF.
*
*        lwa_attr_values_char-charact = lwa_athdr-atnam.
*        lwa_attr_values_char-value_char = lwa_flatv-atwrt.
*        lwa_attr_values_char-instance = lwa_flatv-atzis.
*        lwa_attr_values_char-value_neutral = lwa_flatv-atwrt.
*        lwa_attr_values_char-charact_descr = lwa_flatv-atbez.
*
**//  REPLACE        S4HK902881
**       lwa_attr_values_char-value_char = lwa_flatv-atwrt.
**       lwa_attr_values_char-instance = lwa_flatv-atzis.
**       lwa_attr_values_char-value_neutral = lwa_flatv-atwrt.
*     lwa_attr_values_char-value_char_long = lwa_flatv-atwrt.
*     lwa_attr_values_char-value_neutral_long = lwa_flatv-atwrt.
**  REPLACE
*
*        APPEND lwa_attr_values_char TO lt_attr_values_char.
*
*      WHEN c_atfor-currency.
*
*        lwa_attr_values_curr-charact = lwa_athdr-atnam.
*        lwa_attr_values_curr-value_from = lwa_flatv-atflv.
*        lwa_attr_values_curr-value_to = lwa_flatv-atflb.
*        lwa_attr_values_curr-value_relation = lwa_flatv-atcod.
*        lwa_attr_values_curr-currency_from = lwa_flatv-atawe.
*        lwa_attr_values_curr-currency_to = lwa_flatv-ataw1.
*        lwa_attr_values_curr-instance = lwa_flatv-atzis.
*        lwa_attr_values_curr-charact_descr  = lwa_flatv-atbez.
*
*        APPEND lwa_attr_values_curr TO lt_attr_values_curr.
*
*    ENDCASE.
*
*  ENDLOOP.
*
*ENDFORM.                    " ATTR_VALUES_TABLES_PREPARE
