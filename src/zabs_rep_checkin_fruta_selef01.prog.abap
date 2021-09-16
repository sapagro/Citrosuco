*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_CHECKIN_FRUTA_SELEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

  DATA: lt_domain_values TYPE TABLE OF dd07v,
        lt_lista         TYPE vrm_values,
        lv_dropdown_id   TYPE vrm_id,
        lv_domain        TYPE domname VALUE 'ZABS_DOM_TIPO_VIAGEM'.

  IF p_centro IS NOT INITIAL.
    SELECT SINGLE iwerk
      FROM t001w
      INTO @DATA(lv_centro)
     WHERE werks = @p_centro.
    IF sy-subrc <> 0.
*-- Centro &1 inválido!
      MESSAGE i220(zfmfp) WITH p_centro.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
*-- O Centro deve ser informado!
    MESSAGE i219(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_tipo IS INITIAL.
*-- O Tipo de Viagem deve ser informado!
    MESSAGE i227(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      READ TABLE lt_domain_values INTO DATA(ls_domain_value)
        WITH KEY domvalue_l = p_tipo.
      IF sy-subrc NE 0.
*-- Tipo de Viagem &1 inválido!
        MESSAGE i228(zfmfp) WITH p_tipo.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

*  IF p_dest IS NOT INITIAL.
*    SELECT SINGLE iwerk
*      FROM t001w
*      INTO @DATA(lv_dest)
*     WHERE werks = @p_dest.
*    IF sy-subrc <> 0.
**-- Fábrica de Destino &1 inválida!
*      MESSAGE i218(zfmfp) WITH p_dest.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ELSE.
**-- A Fábrica de Destino deve ser informada!
*    MESSAGE i217(zfmfp).
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  IF p_reb1 IS INITIAL.
*-- A placa do Semi Reboque 1 deve ser informada!
    MESSAGE i216(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_zrems IS NOT INITIAL.
**-- A Remessa deve ser informada!
*    MESSAGE i215(zfmfp).
*    LEAVE LIST-PROCESSING.
*  ELSE.
    SELECT SINGLE vbeln
      FROM likp
      INTO @DATA(lv_vbeln)
     WHERE vbeln EQ @p_zrems.
    IF sy-subrc <> 0.
*-- Remessa &1 inválida!
      MESSAGE i221(zfmfp) WITH p_centro.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF p_arrend IS NOT INITIAL
  AND p_arrend NE abap_true.
*-- Parâmetro Imóvel Arrendado inválido! Pressione F4 em casos de dúvidas!
    MESSAGE i222(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ARREND_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_arrend_values .

  DATA: lt_domain_values TYPE TABLE OF dd07v,
        lt_lista         TYPE vrm_values,
        lv_dropdown_id   TYPE vrm_id,
        lv_domain        TYPE domname VALUE 'ZABS_DOM_ARREND'.

  lv_domain = 'ZABS_DOM_ARREND'.
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = lv_domain
      text           = abap_true
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_domain_values
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  IF sy-subrc EQ 0.
    LOOP AT lt_domain_values INTO DATA(ls_domain_value).
      INSERT INITIAL LINE INTO TABLE lt_lista
        ASSIGNING FIELD-SYMBOL(<ls_lista>).
      IF sy-subrc EQ 0.
        <ls_lista>-key  = ls_domain_value-domvalue_l.
        <ls_lista>-text = ls_domain_value-ddtext.
      ENDIF.
    ENDLOOP.

    lv_dropdown_id = 'P_ARREND'.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = lv_dropdown_id
        values = lt_lista.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TICKET_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ticket_create .

  DATA: lt_messages         TYPE /agri/t_gprolog,
        lv_prnum            TYPE /agri/fmprnum,
        lv_centro           TYPE werks_d,
        lv_cavalo           TYPE zabs_del_placa,
        lv_remessa          TYPE zrems,
        lv_imovel_arrendado TYPE zfmprimovel,
        lv_fabrica_destino  TYPE werks_d,
        lv_semireboque1     TYPE zfmprsemireb1,
        lv_semireboque2     TYPE zfmprsemireb2,
        lv_tara             TYPE zfmprftw,
        lv_tipo_viagem      TYPE zabs_del_tipo_viagem,
        lv_viagem_original  TYPE zabs_del_viagem_orig.

  lv_centro           = p_centro.
  lv_cavalo           = p_caval.
  lv_remessa          = p_zrems.
  lv_imovel_arrendado = p_arrend.
*  lv_fabrica_destino  = p_dest.
  lv_semireboque1     = p_reb1.
  lv_semireboque2     = p_reb2.
  lv_tara             = p_tara.
  lv_tipo_viagem      = p_tipo.

  IF lv_tipo_viagem EQ '2'.
    lv_viagem_original  = p_nro.
  ENDIF.

  CALL FUNCTION 'ZABS_FM_CHECKIN_FRUTA'
    EXPORTING
      i_centro              = lv_centro
      i_cavalo              = lv_cavalo
      i_remessa             = lv_remessa
      i_imovel_arrendado    = lv_imovel_arrendado
      i_fabrica_destino     = lv_fabrica_destino
      i_semireboque1        = lv_semireboque1
      i_semireboque2        = lv_semireboque2
      i_peso_tara           = lv_tara
      i_tipo_viagem         = lv_tipo_viagem
      i_viagem_original     = lv_viagem_original
    CHANGING
      ct_messages           = lt_messages
    EXCEPTIONS
      error_plant           = 1
      ticket_existente      = 2
      transporte_nao_existe = 3
      OTHERS                = 4.

  gt_message[] = lt_messages[].

ENDFORM.
