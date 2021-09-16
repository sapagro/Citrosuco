class ZCL_VISTEX_AGRIPLAN definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_resumo,
        acnum  TYPE c LENGTH 20,
        ajahr  TYPE c LENGTH 4,
        extwg  TYPE c LENGTH 18,
        matkl  TYPE c LENGTH 9,
        matnr  TYPE c LENGTH 40,
        maktx  TYPE c LENGTH 40,
        resumo TYPE c LENGTH 40,
        m1     TYPE p LENGTH 8 DECIMALS 2,
        m2     TYPE p LENGTH 8 DECIMALS 2,
        m3     TYPE p LENGTH 8 DECIMALS 2,
        m4     TYPE p LENGTH 8 DECIMALS 2,
        m5     TYPE p LENGTH 8 DECIMALS 2,
        m6     TYPE p LENGTH 8 DECIMALS 2,
        m7     TYPE p LENGTH 8 DECIMALS 2,
        m8     TYPE p LENGTH 8 DECIMALS 2,
        m9     TYPE p LENGTH 8 DECIMALS 2,
        m10    TYPE p LENGTH 8 DECIMALS 2,
        m11    TYPE p LENGTH 8 DECIMALS 2,
        m12    TYPE p LENGTH 8 DECIMALS 2,
      END OF ts_resumo .

  class-methods ZM_CALC_LINE_RESUMO
    importing
      value(IM_INDEX) type INDEX optional
      value(IM_AJAHR) type AJAHR optional
      value(IM_ZFMAITM) type ZT_FMACITM optional
      value(IM_PERIOD) type ZABSTC_ORCA_PERIOD optional
      value(IM_ORCAMENTO) type ZABSTC_ORCAMENTO optional
    returning
      value(RL_ENTITYSET) type ZCL_ZFARM_AGRIPLAN_MPC=>Ts_GETRESUMOORC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_VISTEX_AGRIPLAN IMPLEMENTATION.


  METHOD zm_calc_line_resumo.

    TYPES: BEGIN OF ty_orcamento,
             period     TYPE spmon,
             aarea_form TYPE zabs_del_qty_20,
             aarea_manu TYPE zabs_del_qty_20,
             custo      TYPE zabs_del_qty_20,
           END OF ty_orcamento.

    DATA: lt_orcamento LIKE im_orcamento,
          lt_collect   TYPE STANDARD TABLE OF ty_orcamento INITIAL SIZE 0,
          ls_collect   LIKE LINE OF lt_collect,
          lv_tabixc    TYPE char2,
          lv_field     TYPE fieldname,
          lv_total     TYPE string.

    lt_orcamento[] = im_orcamento[].
    SORT lt_orcamento BY period.

    LOOP AT lt_orcamento INTO DATA(ls_orcamento).
      IF im_index EQ '1'.
        ls_collect-aarea_form = ls_orcamento-aarea_form.
      ELSEIF im_index EQ '2'.
        ls_collect-aarea_manu = ls_orcamento-aarea_manu.
      ELSEIF im_index EQ '3'.
        ls_collect-custo = ls_orcamento-custo.
      ENDIF.
      ls_collect-period = ls_orcamento-period.
      COLLECT ls_collect INTO lt_collect.
    ENDLOOP.

    SORT lt_collect BY period.

    LOOP AT im_period INTO DATA(ls_period).
      DATA(lv_tabix) = sy-tabix.
      lv_tabixc = lv_tabix.

      CLEAR lv_total.
      READ TABLE lt_collect INTO ls_collect
        WITH KEY period = ls_period BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF im_index EQ '1'.
          lv_total = ls_collect-aarea_form.
        ELSEIF im_index EQ '2'.
          lv_total = ls_collect-aarea_manu.
        ELSEIF im_index EQ '3'.
          lv_total = ls_collect-custo.
        ENDIF.
      ENDIF.

      CONCATENATE 'M' lv_tabixc INTO lv_field.
      ASSIGN COMPONENT lv_field OF STRUCTURE rl_entityset
        TO FIELD-SYMBOL(<lv_field>).
      IF sy-subrc EQ 0.
        <lv_field> = lv_total.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
