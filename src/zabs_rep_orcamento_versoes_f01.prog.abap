*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_ORCAMENTO_VERSOES_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_message.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM selection_validations .

  DATA: lv_titlebar TYPE itex132,
        lv_question TYPE itex132,
        lv_answer.

  IF p_acnum IS INITIAL.
*-- A Área de Cultivo deve ser informada!
    MESSAGE i297(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT * UP TO 1 ROWS
      FROM zfmachdr
      INTO @DATA(ls_achdr)
     WHERE acnum EQ @p_acnum.
    ENDSELECT.

    IF sy-subrc NE 0.
*-- Área de Cultivo &1 inexistente!
      MESSAGE i303(zfmfp) WITH p_acnum.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF p_sver IS INITIAL.
*-- A Versão de Origem deve ser informada!
    MESSAGE i298(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_tver IS INITIAL.
*-- A Versão de Destino deve ser informada!
    MESSAGE i299(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT * UP TO 1 ROWS
    FROM zabs_orcamento
    INTO @DATA(ls_orcamento_source)
   WHERE acnum  EQ @p_acnum
     AND versao EQ @p_sver.
  ENDSELECT.

  IF sy-subrc NE 0.
*-- Não existe Versão &1 de Orçamento para Área de Cultivo &2!
    MESSAGE i300(zfmfp) WITH p_sver p_acnum.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT * UP TO 1 ROWS
      FROM zabs_orcamento
      INTO @DATA(ls_orcamento_target)
     WHERE acnum  EQ @p_acnum
       AND versao EQ @p_tver.
    ENDSELECT.

    IF sy-subrc EQ 0.
*-- Versão &1 de Orçamento já existe para Área de Cultivo &2!
*-- Substituir versão existente?
      MESSAGE i301(zfmfp) WITH p_tver p_acnum INTO lv_titlebar.
      PERFORM popup_to_confirm USING lv_titlebar
                                     TEXT-t02
                                     abap_true
                            CHANGING lv_answer.
      IF lv_answer NE '1'.
*-- Operação cancelada.
        MESSAGE s302(zfmfp).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILL_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fill_default_values .



ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AVAILABLE_VERSIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_available_versions USING lv_version TYPE fieldname .

  TYPES: BEGIN OF tty_versao,
           versao TYPE zabs_del_ver_orc,
         END OF tty_versao.

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        lt_versao TYPE TABLE OF tty_versao.

  lt_dynpfields-fieldname = 'P_ACNUM'.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc EQ 0.
    READ TABLE lt_dynpfields WITH KEY fieldname = 'P_ACNUM'.
    IF sy-subrc EQ 0.
      p_acnum = lt_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.

  IF p_acnum IS INITIAL.
*-- Informe a Área de Cultivo para consultar as versões existentes!
    MESSAGE i304(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
*-- Verifica versões orçamento
    SELECT DISTINCT versao
      FROM zabs_orcamento
      INTO TABLE @lt_versao
     WHERE acnum = @p_acnum.

    DATA(lv_null) = abap_false.
    IF sy-dbcnt EQ 1.
      READ TABLE lt_versao INTO DATA(ls_versao) INDEX 1.
      IF sy-subrc EQ 0.
        IF ls_versao-versao IS INITIAL.
          lv_null = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( lt_versao[] IS INITIAL OR
         lv_null EQ abap_true )
    AND lv_version EQ 'P_SVER'.
*-- Crie a versão clicando sobre o botão 'Criar Versão Inicial'.
      MESSAGE i306(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = lv_version
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          value_org       = 'S'
        TABLES
          value_tab       = lt_versao
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      IF sy-subrc = 0.
        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        IF sy-subrc = 0.
          IF lv_version EQ 'P_SVER'.
            p_sver = ls_return-fieldval.
          ELSEIF lv_version EQ 'P_TVER'.
            p_tver = ls_return-fieldval.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUDGET_VERSION_GENERATE
*&---------------------------------------------------------------------*
*& p_acnum p_sver p_tver
*&---------------------------------------------------------------------*
FORM budget_version_generate USING lv_acnum TYPE zfmacnum
                                   lv_sver  TYPE zabs_del_ver_orc
                                   lv_tver  TYPE zabs_del_ver_orc.

  DATA: lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        lt_log             TYPE STANDARD TABLE OF zabs_yorcamento,
        ls_message         TYPE /agri/s_gprolog.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Elimina último log de versão destino
  SELECT *
    FROM zabs_yorcamento
    INTO TABLE @DATA(lt_log_del)
   WHERE acnum    = @lv_acnum
     AND versao   = @lv_tver
     AND data_log = @sy-datum.

  IF sy-subrc EQ 0.
    DELETE zabs_yorcamento FROM TABLE lt_log_del[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*-- Verifica orçamento de versão origem
  SELECT *
    FROM zabs_orcamento
    INTO TABLE @DATA(lt_source)
   WHERE acnum  = @lv_acnum
     AND versao = @lv_sver.

  LOOP AT lt_source INTO DATA(ls_source).
    INSERT INITIAL LINE INTO TABLE lt_log
      ASSIGNING FIELD-SYMBOL(<ls_log>).
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_source TO <ls_log>.
      CLEAR <ls_log>-tcode.
      <ls_log>-acnum = ls_source-acnum.
      <ls_log>-rcnum = ls_source-rcnum.
      <ls_log>-matnr = ls_source-matnr.
      <ls_log>-period = ls_source-period.
      <ls_log>-fazenda = ls_source-fazenda.
      <ls_log>-versao = lv_tver.
      <ls_log>-extwg = ls_source-extwg.
      <ls_log>-matkl = ls_source-matkl.
      <ls_log>-kostl = ls_source-kostl.
      <ls_log>-aarea_form = ls_source-aarea_form.
      <ls_log>-aarea_manu = ls_source-aarea_manu.
      <ls_log>-passadas = ls_source-passadas.
      <ls_log>-rcdos = ls_source-rcdos.
      <ls_log>-custo = ls_source-custo.
      <ls_log>-produtos = ls_source-produtos.
      <ls_log>-versao_origem = lv_sver.
      <ls_log>-usuario = sy-uname.
    ENDIF.
  ENDLOOP.

  DATA(lv_datum) = sy-datum.
  DATA(lv_uzeit) = sy-uzeit.

*-- Atualiza Log [ZABS_ORCAMENTO -> ZABS_YORCAMENTO]
  IF lt_log[] IS NOT INITIAL.
    READ TABLE lt_log INTO DATA(ls_log) INDEX 1.
    IF sy-subrc EQ 0.
      ls_log-data_log = lv_datum.
      ls_log-tcode = 'ZABS_ORCAMENTO_VERS'.
      ls_log-hora = lv_uzeit.
      MODIFY lt_log FROM ls_log
        TRANSPORTING data_log tcode hora WHERE tcode IS INITIAL.
    ENDIF.
    MODIFY zabs_yorcamento FROM TABLE lt_log[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*-- Elimina último orçamento da versão destino
  SELECT *
    FROM zabs_orcamento
    INTO TABLE @DATA(lt_target_old)
   WHERE acnum  = @p_acnum
     AND versao = @lv_tver.

  IF sy-subrc EQ 0.
    DELETE zabs_orcamento FROM TABLE lt_target_old[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_source[] IS NOT INITIAL.
*-- Grava orçamento para a versão destino
    DATA(lt_target_new) = lt_source[].
    READ TABLE lt_target_new INTO DATA(ls_target_new) INDEX 1.
    IF sy-subrc EQ 0.
      ls_target_new-versao = lv_tver.
      ls_target_new-usuario = sy-uname.
      ls_target_new-data = lv_datum.
      ls_target_new-hora = lv_uzeit.
      ls_target_new-tcode = 'ZABS_ORCAMENTO_VERS'.
      MODIFY lt_target_new FROM ls_target_new
        TRANSPORTING versao usuario data hora tcode
               WHERE acnum  = lv_acnum
                 AND versao = lv_sver.
      MODIFY zabs_orcamento FROM TABLE lt_target_new[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

  lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
  lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Ao criar uma nova versão, apaga os registros da tabela ZABS_ORCAMENTO
*-- quando a versão de origem é vazio. [VERSAO = ''].
  DATA(lv_initial_version) = abap_false.
  READ TABLE lt_formatted_stack INTO DATA(ls_formatted_stack)
    WITH KEY event = 'INITIAL_VERSION_CREATE'.
  IF sy-subrc EQ 0.
    lv_initial_version = abap_true.
    IF lv_sver IS INITIAL
    AND lt_source[] IS NOT INITIAL.
      DELETE zabs_orcamento FROM TABLE lt_source[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_initial_version EQ abap_true.
*-- Versão Inicial de Orçamento criada com sucesso!
    MESSAGE i307(zfmfp).
  ELSE.
*-- As versões de Orçamento da Área de Cultivo &1 foram atualizadas!
    MESSAGE i305(zfmfp).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_006
*&      --> TEXT_007
*&      --> ABAP_TRUE
*&      <-- LV_ANSWER
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING lv_titlebar
                            lv_question
                            lv_display_cancel
                   CHANGING lv_answer.

  CLEAR lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = lv_titlebar
      text_question         = lv_question
      display_cancel_button = lv_display_cancel
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS ##fm_subrc_ok
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUTTON_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM button_create .

  DATA: ls_button TYPE smp_dyntxt.

*-- Criar Versão Inicial
  ls_button-text = TEXT-t03.
*  Ícone do Botão
  ls_button-icon_id   = icon_initial.
*-- Criar Versão Inicial
  ls_button-icon_text = 'Criar Versão Inicial'.
*-- Criar Versão Inicial de Orçamento
  ls_button-quickinfo = 'Criar Versão Inicial de Orçamento'.
*-- Associa as propriedades com a função 1
  sscrfields-functxt_01 = ls_button.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIAL_VERSION_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initial_version_create .

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lv_versao_null TYPE zabs_del_ver_orc,
        lv_new_version TYPE zabs_del_ver_orc,
        lv_returncode  TYPE c.

  lt_dynpfields-fieldname = 'P_ACNUM'.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc EQ 0.
    READ TABLE lt_dynpfields WITH KEY fieldname = 'P_ACNUM'.
    IF sy-subrc EQ 0.
      p_acnum = lt_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.

  IF p_acnum IS INITIAL.
*-- Informe a Área de Cultivo para criar a versão inicial de Orçamento!
    MESSAGE i309(zfmfp).
  ELSE.
    SELECT * UP TO 1 ROWS
      FROM zabs_orcamento
      INTO @DATA(ls_orcamento)
     WHERE acnum EQ @p_acnum.
    ENDSELECT.

    IF sy-subrc NE 0.
*-- Não existe Orçamento para a Área de Cultivo &1!
      MESSAGE i310(zfmfp) WITH p_acnum.
      LEAVE LIST-PROCESSING.
    ELSE.
      SELECT * UP TO 1 ROWS
        FROM zabs_orcamento
        INTO @ls_orcamento
       WHERE acnum  EQ @p_acnum
         AND versao NE @lv_versao_null.
      ENDSELECT.

      IF sy-subrc EQ 0.
*-- Já existe Versão Inicial &1 de Orçamento para Área de Cultivo &2!
        MESSAGE i308(zfmfp) WITH ls_orcamento-versao p_acnum.
      ELSE.
        PERFORM get_version CHANGING lv_new_version
                                     lv_returncode.

        IF lv_returncode NE 'A'
        AND lv_new_version IS NOT INITIAL.
          PERFORM budget_version_generate USING p_acnum
                                                lv_versao_null
                                                lv_new_version.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_VERSION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_version CHANGING lv_new_version TYPE zabs_del_ver_orc
                          lv_returncode  TYPE c.

  DATA: lt_fields TYPE TABLE OF sval,
        lv_title  TYPE char20,
        lv_char   TYPE char100.

  CLEAR lv_returncode.

  INSERT INITIAL LINE INTO TABLE lt_fields
    ASSIGNING FIELD-SYMBOL(<ls_field>).
  IF sy-subrc EQ 0.
    <ls_field>-tabname = 'ZABS_ORCAMENTO'.
    <ls_field>-fieldname = 'VERSAO'.
    <ls_field>-field_obl = abap_true.
  ENDIF.

*-- Informe a Versão Inicial
  lv_title = TEXT-t04.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = lv_title
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS ##FM_SUBRC_OK
      error_in_fields = 1
      OTHERS          = 2.

  IF lv_returncode EQ 'A'.
*-- Operação cancelada.
    MESSAGE i302(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    lv_char = sy-abcde && | 0123456789|.
    READ TABLE lt_fields INTO DATA(ls_field)
      WITH KEY tabname   = 'ZABS_ORCAMENTO'
               fieldname = 'VERSAO'.
    IF sy-subrc EQ 0.
      IF ls_field-value CN lv_char.
        lv_returncode = 'A'.
*-- Utilize somente letras e números!
        MESSAGE i311(zfmfp).
        LEAVE LIST-PROCESSING.
      ELSE.
        lv_new_version = ls_field-value.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
