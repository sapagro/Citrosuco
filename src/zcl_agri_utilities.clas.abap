class ZCL_AGRI_UTILITIES definition
  public
  final
  create public .

public section.

  class-methods ZM_GET_TVARVC
    importing
      value(IV_NAME) type STRING
    returning
      value(RT_TABLE) type /IWBEP/T_COD_SELECT_OPTIONS .
  class-methods ZM_GET_PROCESSOS
    returning
      value(RT_GRP_MARA) type ZCT_GRP_MARA .
  class-methods ZM_GET_TOT_MATNR
    importing
      value(IT_ACITM) type ZT_FMACITM optional
      value(IV_BEGDA) type BEGDA optional
      value(IV_ENDDA) type ENDDA optional
      value(IV_MATKL) type MATKL optional
      value(IT_WEEK) type ZCFMPLPRSEMANAL optional
    returning
      value(RT_TOTAL) type ZFMPLAPR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AGRI_UTILITIES IMPLEMENTATION.


  METHOD zm_get_processos.

*-- BOC-T_T.KONNO
*    DATA: r_mtart TYPE /iwbep/t_cod_select_options.
    DATA: r_mtart  TYPE /iwbep/t_cod_select_options,
          wa_mtart LIKE LINE OF r_mtart.
*-- EOC-T_T.KONNO

    r_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).

*-- BOC-T_T.KONNO
    wa_mtart-sign = 'I'.
    wa_mtart-option = 'EQ'.
    MODIFY r_mtart FROM wa_mtart TRANSPORTING sign option WHERE low <> ''.
*-- EOC-T_T.KONNO

    SELECT b~extwg b~ewbez
      INTO CORRESPONDING FIELDS OF TABLE rt_grp_mara
      FROM zagri_grp_mara AS a
      INNER JOIN twewt AS b
      ON a~extwg EQ b~extwg
      WHERE mtart IN r_mtart.
    IF sy-subrc EQ 0.
      SORT rt_grp_mara BY extwg.
    ENDIF.

  ENDMETHOD.


  METHOD zm_get_tot_matnr.

    LOOP AT it_week INTO DATA(ls_week) WHERE matkl EQ iv_matkl AND
                                             plday BETWEEN iv_begda AND iv_endda.
      rt_total = rt_total + ls_week-plapr.
    ENDLOOP.

  ENDMETHOD.


  METHOD zm_get_tvarvc.

    SELECT sign opti low high
      INTO TABLE rt_table
      FROM tvarvc
      WHERE name EQ iv_name.

  ENDMETHOD.
ENDCLASS.
