*----------------------------------------------------------------------*
***INCLUDE ZABS_INC_ATUALIZA_ORC_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FILL_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_default_values .

*  DATA: lt_processos TYPE zct_grp_mara,
*        lt_values    TYPE vrm_values,
*        lv_vrm_id    TYPE vrm_id.
*
*  IF p_proc IS INITIAL.
*    REFRESH: lt_values.
*    lv_vrm_id = 'P_PROC'.
*
*    lt_processos = zcl_agri_utilities=>zm_get_processos( ).
*    DATA(lv_proc) = lines( lt_processos ).
*
*    DO lv_proc TIMES.
*      DATA(lv_index) = sy-index.
*      READ TABLE lt_processos INTO DATA(ls_processo) INDEX lv_index.
*      IF sy-subrc EQ 0.
*        INSERT INITIAL LINE INTO TABLE lt_values
*          ASSIGNING FIELD-SYMBOL(<lwa_value>).
*        IF sy-subrc EQ 0.
*          <lwa_value>-key = ls_processo-extwg.
*          <lwa_value>-text = ls_processo-ewbez.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = lv_vrm_id
*        values          = lt_values
*      EXCEPTIONS
*        id_illegal_name = 1
*        OTHERS          = 2.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_message, gt_achdr, gt_orcamento,
           gt_terrains, gt_processos, gt_update.

  CLEAR: gs_achdr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

  IF p_acnum IS INITIAL.
*-- A Área de Cultivo deve ser informada!
    MESSAGE i297(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT * UP TO 1 ROWS
      FROM zfmachdr
      INTO @gs_achdr
     WHERE acnum EQ @p_acnum.
    ENDSELECT.

    IF sy-subrc EQ 0.
      SELECT * UP TO 1 ROWS
        FROM zfmaitm
        INTO @DATA(ls_acitm)
       WHERE acnum = @gs_achdr-acnum.
      ENDSELECT.

      IF sy-subrc NE 0.
*-- ZFMACM: Área de Cultivo &1 não contém talhões na aba "Item".
        MESSAGE i350(zfmfp) WITH p_acnum.
        LEAVE LIST-PROCESSING.
      ENDIF.

      SELECT * UP TO 1 ROWS
        FROM zfmacvlcl
        INTO @DATA(ls_acvlcl)
       WHERE acnum = @gs_achdr-acnum.
      ENDSELECT.

      IF sy-subrc NE 0.
*-- ZFMACM: Área de Cultivo &1 não contém informações na aba "Vol.Calda".
        MESSAGE i351(zfmfp) WITH p_acnum.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
      IF sy-subrc NE 0.
*-- Área de Cultivo &1 inexistente!
        MESSAGE i303(zfmfp) WITH p_acnum.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF p_vers IS INITIAL.
*-- A Versão do Orçamento deve ser informada!
      MESSAGE i344(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.

    SELECT *
      INTO TABLE @gt_orcamento
      FROM zabs_orcamento
     WHERE acnum  EQ @p_acnum
       AND versao EQ @p_vers.

    IF sy-subrc NE 0.
*-- Área de Cultivo &1 Versão &2 inexistente!
      MESSAGE i345(zfmfp) WITH p_acnum p_vers.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AVAILABLE_VERSIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
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
    AND lv_version EQ 'P_VERS'.
*-- Nenhuma versão de orçamento encontrada para a Área de Cultivo &1!
      MESSAGE i343(zfmfp) WITH p_acnum.
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
          IF lv_version EQ 'P_VERS'.
            p_vers = ls_return-fieldval.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AVAILABLE_CULT_AREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_available_cult_area .

  TYPES: BEGIN OF tty_zfmachdr,
           acnum TYPE zfmacnum,
           ajahr TYPE ajahr,
           actyp TYPE zfmactyp,
           acdes TYPE zfmacdes,
         END OF tty_zfmachdr.

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lt_return   TYPE STANDARD TABLE OF ddshretval,
        lt_dfies    TYPE STANDARD TABLE OF dfies,
        lt_map      TYPE STANDARD TABLE OF dselc,
        lt_zfmachdr TYPE TABLE OF tty_zfmachdr.

  SELECT acnum, ajahr, actyp, acdes
    FROM zfmachdr
    INTO TABLE @lt_zfmachdr.

  IF sy-subrc EQ 0.
    SORT lt_zfmachdr BY acnum.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ACNUM'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = lt_zfmachdr
        field_tab       = lt_dfies
        return_tab      = lt_return
        dynpfld_mapping = lt_map
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
      IF sy-subrc = 0.
        p_acnum = ls_return-fieldval.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DATA_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM data_validation .

  TYPES: BEGIN OF ty_matkl,
           matnr   TYPE matnr,
           matkl   TYPE matkl,
           extwg   TYPE extwg,
           wgbez   TYPE wgbez,
           wgbez60 TYPE wgbez60,
         END OF   ty_matkl,

         BEGIN OF ty_total,
           fazenda    TYPE string,
           kostl_form TYPE kostl,
           kostl_manu TYPE kostl,
           period     TYPE spmon,
           aareaform  TYPE zfmacqtb,
           aareamanu  TYPE zfmacqtb,
           bombasform TYPE zfmacqtb,
           bombasmanu TYPE zfmacqtb,
           tarefa     TYPE matnr,
           matnr      TYPE matnr,
           iwerk      TYPE werks_d,
           valor      TYPE verpr,
         END OF ty_total,

         BEGIN OF ty_period,
           period TYPE period,
         END OF ty_period,

         BEGIN OF ty_orcamento,
           matkl      TYPE matkl,
           period     TYPE spmon,
           aarea_form TYPE zabs_del_qty_20,
           aarea_manu TYPE zabs_del_qty_20,
         END OF ty_orcamento,

         BEGIN OF ty_orc_tot,
           matkl     TYPE matkl,
           period    TYPE spmon,
           form_manu TYPE zabs_del_qty_20,
         END OF ty_orc_tot.

  DATA: lt_matkl_geral   TYPE TABLE OF ty_matkl,
        lt_consolidated  TYPE TABLE OF ty_total INITIAL SIZE 0,
        ls_consolidated  LIKE LINE OF lt_consolidated,
        ls_orcamento_new TYPE zabs_orcamento,
        lt_detailed      TYPE TABLE OF ty_total INITIAL SIZE 0,
        ls_detailed      LIKE LINE OF lt_detailed,
        lt_matkl         TYPE TABLE OF ty_matkl,
        lt_dates         TYPE /scwm/tt_lm_dates,
        lt_period        TYPE STANDARD TABLE OF ty_period INITIAL SIZE 0,
        ls_period        LIKE LINE OF lt_period,
        lt_orc_full      TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        ls_orc_full      LIKE LINE OF lt_orc_full,
        lrt_periodo      TYPE RANGE OF spmon,
        lrt_matnr        TYPE RANGE OF zfmaplmatnr,
        lrt_mtart        TYPE /iwbep/t_cod_select_options,
        lrt_rcnum        TYPE RANGE OF zfmrcnum,
        lrt_rcnum_w_orc  TYPE RANGE OF zfmrcnum,
        lrs_rcnum        LIKE LINE OF lrt_rcnum,
        lrs_rcnum_w_orc  LIKE LINE OF lrt_rcnum_w_orc,
        lrt_werks        TYPE RANGE OF werks_d,
        lrs_werks        LIKE LINE OF lrt_werks,
        lrs_mtart        LIKE LINE OF lrt_mtart,
        lrs_periodo      LIKE LINE OF lrt_periodo,
        lv_season        TYPE /agri/gl_season,
        lv_date_x        TYPE sydatum,
        lv_endda_month   TYPE endda,
        lv_matnr_x       TYPE zfmacmatnr,
        lv_total         TYPE zabs_del_qty_20,
        lv_tfor          TYPE matnr,
        lv_tman          TYPE matnr,
        lv_timp          TYPE matnr,
        lv_periodo       TYPE char6,
        lv_matkl         TYPE char9,
        lv_werks         TYPE werks_d,
        lv_auxiliar      TYPE f,
        lv_passadas      TYPE f,
        lv_produto       TYPE f,
        lrt_matkl_x      LIKE RANGE OF lv_matkl,
        lrs_matkl_x      LIKE LINE OF lrt_matkl_x.

  CONSTANTS: BEGIN OF c_crop_season_status,
               active   TYPE /agri/glastat VALUE 'A',
               inactive TYPE /agri/glastat VALUE 'I',
               closed   TYPE /agri/glastat VALUE 'C',
             END OF c_crop_season_status.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   TYPE char10 VALUE 'FORMAÇÃO',
               manutencao TYPE char10 VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  lrt_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).
  lrs_mtart-sign = 'I'.
  lrs_mtart-option = 'EQ'.
  MODIFY lrt_mtart FROM lrs_mtart TRANSPORTING sign option WHERE low <> ''.

  REFRESH lt_dates.
  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = gs_achdr-datab
      iv_endda = gs_achdr-datbi
    IMPORTING
      et_dates = lt_dates.

  lrs_periodo = 'IEQ'.
  LOOP AT lt_dates INTO DATA(ls_date).
    IF lv_periodo NE ls_date(6).
      lrs_periodo-low = ls_period-period = lv_periodo = ls_date(6).
      APPEND: ls_period   TO lt_period,
              lrs_periodo TO lrt_periodo.
    ENDIF.
  ENDLOOP.

  SELECT DISTINCT iwerk AS low
    FROM zfmaitm
    INTO CORRESPONDING FIELDS OF TABLE @lrt_werks
   WHERE acnum = @p_acnum.

  lrs_werks-sign = 'I'.
  lrs_werks-option = 'EQ'.
  MODIFY lrt_werks FROM lrs_werks TRANSPORTING sign option WHERE low <> ''.

*-- Validades da Área de Cultivo e Receitas não são modificadas
  SELECT r~rcnum, r~werks, r~matnr, r~datuv, r~rctex,
         r~rctyp, r~maktx, r~ausme, r~text1, r~datbi,
         m~matkl, m~extwg
    INTO TABLE @DATA(lt_receitas_geral)
    FROM zfmrchdr AS r
    INNER JOIN ztfmrctyp AS t
    ON r~rctyp EQ t~rctyp
    INNER JOIN mara AS m
    ON m~matnr EQ r~matnr
   WHERE r~werks     IN @lrt_werks[]
     AND r~datuv     GE @gs_achdr-datab
     AND r~datbi     LE @gs_achdr-datbi
     AND t~orcamento EQ @abap_true
    ORDER BY r~matnr, r~rcnum.

  IF lt_receitas_geral[] IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(lt_zfmrclst_geral)
      FROM zfmrclst
      FOR ALL ENTRIES IN @lt_receitas_geral
     WHERE rcnum EQ @lt_receitas_geral-rcnum
       AND werks EQ @lt_receitas_geral-werks
       AND matnr EQ @lt_receitas_geral-matnr.

    IF sy-subrc EQ 0.
*-- Preços Médios diferentes em Centros diferentes
      SELECT matnr, bwkey, bwtar, verpr
        FROM mbew
        INTO TABLE @DATA(lt_mbew_geral)
        FOR ALL ENTRIES IN @lt_zfmrclst_geral
       WHERE matnr EQ @lt_zfmrclst_geral-matnr_ins
         AND bwkey IN @lrt_werks[].

      SORT lt_mbew_geral BY matnr bwkey.
    ENDIF.
  ENDIF.

  DATA(lt_orcamento_geral) = gt_orcamento[].
  DELETE lt_orcamento_geral WHERE period NOT IN lrt_periodo[].

  REFRESH lt_matkl_geral.
  SELECT DISTINCT m~matnr, m~matkl, m~extwg
    INTO CORRESPONDING FIELDS OF TABLE @lt_matkl_geral
    FROM mara AS m
    INNER JOIN zfmrchdr AS r
    ON m~matnr EQ r~matnr
    INNER JOIN ztfmrctyp AS t
    ON r~rctyp EQ t~rctyp
    INNER JOIN zabs_orcamento AS o
    ON o~matkl EQ m~matkl
    AND o~extwg EQ m~extwg
   WHERE m~matnr     IN @lrt_matnr[]
     AND m~mtart     IN @lrt_mtart[]
     AND m~matkl     NE @space
     AND t~orcamento EQ @abap_true
     AND o~acnum     EQ @p_acnum
     AND o~versao    EQ @p_vers
    ORDER BY m~matkl.

*-- Main Process
  LOOP AT gt_processos INTO DATA(ls_processo).
    REFRESH lrt_matnr.
    INSERT INITIAL LINE INTO TABLE lrt_matnr
      ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'ICP'.
      CASE ls_processo-extwg.
        WHEN 'GIM'.
          <lrs_matnr>-low = 'TIMP*'.
        WHEN 'GTC'.
          <lrs_matnr>-low = 'TMAN*'.
          INSERT INITIAL LINE INTO TABLE lrt_matnr
            ASSIGNING <lrs_matnr>.
          IF sy-subrc EQ 0.
            <lrs_matnr> = 'ICP'.
            <lrs_matnr>-low = 'TFOR*'.
          ENDIF.
        WHEN 'GCO'.
          <lrs_matnr>-low = 'TCOL*'.
        WHEN 'GIR'.
          <lrs_matnr>-low = 'TMAN*'.
      ENDCASE.
    ENDIF.

    DATA(lt_receitas) = lt_receitas_geral[].
    DELETE lt_receitas WHERE matnr NOT IN lrt_matnr[].
    IF lt_receitas[] IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(lt_orcamento) = lt_orcamento_geral[].
    DELETE lt_orcamento WHERE extwg NE ls_processo-extwg.
    IF lt_orcamento[] IS INITIAL.
      CONTINUE.
    ENDIF.

    REFRESH: lrt_rcnum.
    lrt_rcnum = CORRESPONDING #( lt_receitas MAPPING low = rcnum ).
    lrs_rcnum-sign = 'I'.
    lrs_rcnum-option = 'EQ'.
    MODIFY lrt_rcnum FROM lrs_rcnum TRANSPORTING sign option WHERE low <> ''.
    SORT lrt_rcnum BY low.

    lt_matkl[] = lt_matkl_geral[].
    DELETE lt_matkl WHERE extwg NE ls_processo-extwg.

    DATA(lt_mara) = lt_matkl[].
    DATA(lt_matkl_distinct) = lt_matkl[].
    DELETE ADJACENT DUPLICATES FROM lt_matkl_distinct
      COMPARING matkl.

    SELECT s~tplnr_fl, s~contr, s~cmnum, s~varia,
           s~season, s~datab, s~datbi, s~astat,
           s~iwerk, s~anlnr, s~kostl, s~rtnid, s~loevm,
           s~datab_ref, s~zzfazplantio,
           s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
           v~acnum, v~vornr, v~matnr, v~acqtb, v~acarx
      INTO TABLE @DATA(lt_tplnr_geral)
      FROM zfmaitm AS i
      INNER JOIN zfmacvlcl AS v
      ON  i~acnum    EQ v~acnum
      AND i~tplnr_fl EQ v~tplnr_fl
      INNER JOIN /agri/glflcma AS s
      ON s~tplnr_fl EQ v~tplnr_fl
     WHERE i~acnum      EQ @p_acnum
       AND s~cmnum      EQ 'CITROS'
       AND s~astat      EQ @c_crop_season_status-active
       AND s~loevm      NE @abap_true
       AND s~zzprevisto EQ @abap_false.

    SELECT s~tplnr_fl, s~contr, s~cmnum, s~varia,
           s~season, s~datab, s~datbi, s~astat,
           s~iwerk, s~anlnr, s~kostl, s~rtnid, s~loevm,
           s~datab_ref, s~zzfazplantio,
           s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
           v~acnum, v~vornr, v~matnr, v~acqtb, v~acarx
      APPENDING TABLE @lt_tplnr_geral
      FROM zfmaitm AS i
      INNER JOIN zfmacvlcl AS v
      ON  i~acnum    EQ v~acnum
      AND i~tplnr_fl EQ v~tplnr_fl
      INNER JOIN /agri/glflcma AS s
      ON s~tplnr_fl EQ v~tplnr_fl
     WHERE i~acnum      EQ @p_acnum
       AND s~cmnum      EQ 'CITROS'
       AND s~loevm      NE @abap_true
       AND s~zzprevisto EQ @abap_true.

    SORT lt_tplnr_geral BY tplnr_fl contr cmnum varia season.
    DELETE ADJACENT DUPLICATES FROM lt_tplnr_geral COMPARING ALL FIELDS.

    LOOP AT lt_matkl_distinct INTO DATA(ls_matkl).
      lt_receitas[] = lt_receitas_geral[].
      DELETE lt_receitas WHERE matkl NE ls_matkl-matkl
                            OR extwg NE ls_matkl-extwg.

      IF lt_receitas[] IS NOT INITIAL.
        lt_orcamento[] = lt_orcamento_geral[].
        DELETE lt_orcamento WHERE matkl NE ls_matkl-matkl
                               OR extwg NE ls_matkl-extwg.

        SORT lt_orcamento BY rcnum period matnr.

        DELETE lt_orcamento WHERE fazenda  EQ space
                               OR produtos EQ space.

*-- BOC T_T.KONNO-04.09.21
*-- Performance Improvement
*-- Double Check (Prescription->Budget and Budget->Prescription)
        REFRESH: lrt_rcnum_w_orc.
        lrt_rcnum_w_orc = CORRESPONDING #( lt_orcamento MAPPING low = rcnum ).
        lrs_rcnum_w_orc-sign = 'I'.
        lrs_rcnum_w_orc-option = 'EQ'.
        MODIFY lrt_rcnum_w_orc FROM lrs_rcnum_w_orc
          TRANSPORTING sign option WHERE low <> ''.
        SORT lrt_rcnum_w_orc BY low.
        DELETE ADJACENT DUPLICATES FROM lrt_rcnum_w_orc COMPARING low.
        DELETE lt_receitas WHERE rcnum NOT IN lrt_rcnum_w_orc[].
*-- EOC T_T.KONNO-04.09.21

        IF lt_orcamento[] IS NOT INITIAL
        AND lt_receitas[] IS NOT INITIAL.
          SELECT *
            INTO TABLE @DATA(lt_zfmrclst)
            FROM zfmrclst
            FOR ALL ENTRIES IN @lt_receitas
           WHERE rcnum EQ @lt_receitas-rcnum
             AND werks EQ @lt_receitas-werks
             AND matnr EQ @lt_receitas-matnr.

          IF sy-subrc EQ 0.
            SORT lt_zfmrclst BY rcnum werks matnr.

            LOOP AT lt_receitas INTO DATA(ls_rchdr).
              lv_werks = ls_rchdr-werks.

              CLEAR lv_season.
              IF ls_rchdr-matnr(4) EQ 'TFOR'
              OR ls_rchdr-matnr(4) EQ 'TIMP'.
                lv_season = 'SAFRA PLAN'.
              ELSEIF ls_rchdr-matnr(4) EQ 'TMAN'
                  OR ls_rchdr-matnr(4) EQ 'TCOL'.
                lv_season = 'SAFRA PROD'.
              ENDIF.

              READ TABLE lt_zfmrclst TRANSPORTING NO FIELDS
                WITH KEY rcnum = ls_rchdr-rcnum
                         werks = ls_rchdr-werks
                         matnr = ls_rchdr-matnr BINARY SEARCH.

              IF sy-subrc EQ 0.
                LOOP AT lt_zfmrclst INTO DATA(ls_zfmrclst) FROM sy-tabix.
                  IF ls_zfmrclst-rcnum NE ls_rchdr-rcnum
                  OR ls_zfmrclst-werks NE ls_rchdr-werks
                  OR ls_zfmrclst-matnr NE ls_rchdr-matnr.
                    EXIT.
                  ENDIF.

                  LOOP AT lt_period INTO ls_period.
                    DATA(lv_tabix) = sy-tabix.
                    REFRESH: lt_consolidated, lt_detailed.

                    lv_date_x = ls_period(4) && ls_period+4(2) && '01'.

                    lt_orcamento[] = lt_orcamento_geral[].
                    DELETE lt_orcamento WHERE rcnum  NE ls_rchdr-rcnum
                                           OR period NE ls_period
                                           OR matnr  NE ls_zfmrclst-matnr_ins.

                    READ TABLE lt_orcamento INTO DATA(ls_orcamento_db)
                      WITH KEY rcnum  = ls_rchdr-rcnum
                               period = ls_period
                               matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                    IF sy-subrc NE 0.
                      CLEAR ls_orcamento_db.
*-- Receita não utilizada para gerar dados de orçamento
                      READ TABLE lt_orcamento_geral TRANSPORTING NO FIELDS
                        WITH KEY rcnum = ls_rchdr-rcnum.
                      IF sy-subrc NE 0.
                        EXIT.
                      ENDIF.
                    ENDIF.

*-- Otimização para ignorar receitas sem passadas na tabela de orçamento
                    DATA(lt_verifica_passadas) = lt_orcamento[].
                    DELETE lt_verifica_passadas WHERE passadas IS INITIAL.
                    IF lt_verifica_passadas[] IS INITIAL.
                      CONTINUE.
                    ENDIF.

                    DATA(lt_tplnr_fl) = lt_tplnr_geral[].
                    DELETE lt_tplnr_fl WHERE iwerk  NE ls_rchdr-werks
                                          OR matnr  NE ls_rchdr-matnr
                                          OR season NE lv_season.

                    DELETE lt_tplnr_fl WHERE datab  GT lv_date_x
                                          OR datbi  LT lv_date_x.

                    DELETE ADJACENT DUPLICATES FROM lt_tplnr_fl COMPARING ALL FIELDS.

                    IF ls_rchdr-matnr(4) EQ 'TFOR'
                    OR ls_rchdr-matnr(4) EQ 'TIMP'.
                      IF lv_date_x IS NOT INITIAL.
                        CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                          EXPORTING
                            i_day_in            = lv_date_x
                          IMPORTING
                            e_last_day_of_month = lv_endda_month.
                      ENDIF.

                      LOOP AT lt_tplnr_fl INTO DATA(ls_glflcma).
                        DATA(lv_glflcma_tabix) = sy-tabix.
                        IF ls_glflcma-zzprevisto EQ abap_true.
                          IF ls_glflcma-zzprev_plantio IS INITIAL.
                            DELETE lt_tplnr_fl INDEX lv_glflcma_tabix.
                          ELSE.
                            IF lv_endda_month IS NOT INITIAL.
                              IF lv_endda_month LE ls_glflcma-zzprev_plantio.
                                DELETE lt_tplnr_fl INDEX lv_glflcma_tabix.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDLOOP.
                    ENDIF.

                    IF lt_tplnr_fl[] IS NOT INITIAL.
                      DATA(lt_glflcma) = lt_tplnr_fl[].
                      SORT lt_glflcma BY matnr.
                      lv_tman = 'TMAN' && ls_rchdr-matnr+4(36).
                      lv_tfor = 'TFOR' && ls_rchdr-matnr+4(36).
                      lv_timp = 'TIMP' && ls_rchdr-matnr+4(36).

                      READ TABLE lt_glflcma INTO ls_glflcma
                        WITH KEY matnr = ls_rchdr-matnr BINARY SEARCH.
                      WHILE sy-subrc EQ 0.
                        DATA(lv_tabix_glflcma) = sy-tabix + 1.

                        CLEAR: ls_consolidated, ls_detailed.
                        IF lv_date_x GE ls_glflcma-datab
                        AND lv_date_x LE ls_glflcma-datbi.
                          IF ls_glflcma-matnr = lv_tman.
                            ls_consolidated-aareamanu  = ls_glflcma-acarx.
                            ls_consolidated-bombasmanu = ls_glflcma-acqtb.
                          ELSEIF ls_glflcma-matnr = lv_tfor
                              OR ls_glflcma-matnr = lv_timp.
                            ls_consolidated-aareaform  = ls_glflcma-acarx.
                            ls_consolidated-bombasform = ls_glflcma-acqtb.
                          ENDIF.
                        ENDIF.

                        ls_consolidated-period = lv_date_x(6).
                        ls_consolidated-tarefa = ls_rchdr-matnr.
                        COLLECT ls_consolidated INTO lt_consolidated.

                        ls_detailed = ls_consolidated.
                        ls_detailed-tarefa     = ls_rchdr-matnr.
                        ls_detailed-fazenda    = ls_glflcma-tplnr_fl.
                        ls_detailed-matnr      = ls_zfmrclst-matnr_ins.
                        ls_detailed-iwerk      = ls_zfmrclst-werks.
                        IF ls_glflcma-kostl IS NOT INITIAL.
                          ls_detailed-kostl_form = ls_glflcma-kostl.
                          ls_detailed-kostl_manu = ls_glflcma-kostl.
                        ELSEIF ls_glflcma-anlnr IS NOT INITIAL.
                          ls_detailed-kostl_form = ls_glflcma-anlnr.
                          ls_detailed-kostl_manu = ls_glflcma-anlnr.
                        ENDIF.
                        COLLECT ls_detailed INTO lt_detailed.

                        READ TABLE lt_glflcma INTO ls_glflcma
                          INDEX lv_tabix_glflcma COMPARING matnr.
                      ENDWHILE.

                      SORT: lt_consolidated BY period matnr,
                            lt_detailed BY period tarefa.

                      LOOP AT lt_detailed ASSIGNING FIELD-SYMBOL(<ls_detailed>).
                        READ TABLE lt_mbew_geral INTO DATA(ls_mbew)
                          WITH KEY matnr = ls_zfmrclst-matnr_ins
                                   bwkey = <ls_detailed>-iwerk BINARY SEARCH.
                        IF sy-subrc EQ 0.
                          <ls_detailed>-valor = ls_mbew-verpr.
                        ENDIF.
                      ENDLOOP.
*--------------------------------------------------------------------
*-- Recálculo de quantidade de produto/insumo
                      IF ls_rchdr-matnr(4) EQ 'TFOR'
                      OR ls_rchdr-matnr(4) EQ 'TIMP'.
                        READ TABLE lt_period TRANSPORTING NO FIELDS
                          WITH KEY period = ls_period.
                        IF sy-subrc EQ 0.
                          READ TABLE lt_detailed INTO ls_detailed
                            WITH KEY period = ls_period
                                     tarefa = ls_rchdr-matnr BINARY SEARCH.
                          WHILE sy-subrc EQ 0.
                            DATA(lv_tabix_x) = sy-tabix + 1.

                            READ TABLE lt_orcamento INTO ls_orcamento_db
                              WITH KEY acnum   = p_acnum
                                       extwg   = ls_matkl-extwg
                                       matkl   = ls_matkl-matkl
                                       rcnum   = ls_rchdr-rcnum
                                       matnr   = ls_zfmrclst-matnr_ins
                                       period  = ls_period
                                       fazenda = ls_detailed-fazenda
                                       versao  = p_vers.
                            IF sy-subrc EQ 0.
                              CLEAR ls_consolidated.
                              READ TABLE lt_consolidated INTO ls_consolidated
                                WITH KEY period = ls_period BINARY SEARCH.

                              ls_orcamento_new = ls_orcamento_db.

                              IF ls_zfmrclst-matnr(4) EQ 'TFOR'
                              OR ls_zfmrclst-matnr(4) EQ 'TIMP'.
                                ls_orcamento_new-aarea_form = ls_detailed-aareaform * ls_orcamento_new-passadas.
*-- Calcula Quantidade de Produto
                                IF ls_rchdr-ausme EQ 'VC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasform * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasform * ls_orcamento_new-rcdos.
                                ELSEIF ls_rchdr-ausme EQ 'HC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareaform * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareaform * ls_orcamento_new-rcdos.
                                ENDIF.
                              ELSEIF ls_zfmrclst-matnr(4) EQ 'TMAN'
                                  OR  ls_zfmrclst-matnr(4) EQ 'TCOL'.
                                ls_orcamento_new-aarea_manu = ls_detailed-aareamanu * ls_orcamento_new-passadas.
*-- Calcula Quantidade de Produto
                                IF ls_rchdr-ausme EQ 'VC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasmanu * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasmanu * ls_orcamento_new-rcdos.
                                ELSEIF ls_rchdr-ausme EQ 'HC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareamanu * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareamanu * ls_orcamento_new-rcdos.
                                ENDIF.
                              ENDIF.

                              IF ls_zfmrclst-rcinp EQ icon_wd_radio_button_empty.
                                CLEAR: ls_orcamento_new-aarea_manu, ls_orcamento_new-aarea_form.
                              ENDIF.

                              ls_orcamento_new-produtos = lv_produto.
                              ls_orcamento_new-custo = ls_orcamento_new-produtos * ls_detailed-valor.

                              IF ls_orcamento_new-produtos NE ls_orcamento_db-produtos
                              OR ls_orcamento_new-custo NE ls_orcamento_db-custo.
                                INSERT INITIAL LINE INTO TABLE gt_calculo
                                  ASSIGNING FIELD-SYMBOL(<ls_calculo>).
                                IF sy-subrc EQ 0.
                                  MOVE-CORRESPONDING ls_orcamento_new TO <ls_calculo>.
                                ENDIF.
                              ENDIF.
                            ENDIF.

                            READ TABLE lt_detailed INTO ls_detailed
                              INDEX lv_tabix_x COMPARING period tarefa.
                          ENDWHILE.
                        ENDIF.
*--------------------------------------------------------------------
*-- Recálculo de quantidade de produto/insumo
                      ELSEIF ls_rchdr-matnr(4) EQ 'TMAN'
                          OR ls_rchdr-matnr(4) EQ 'TCOL'.
                        READ TABLE lt_period TRANSPORTING NO FIELDS
                          WITH KEY period = ls_period.
                        IF sy-subrc EQ 0.
                          READ TABLE lt_detailed INTO ls_detailed
                            WITH KEY period = ls_period
                                     tarefa = ls_rchdr-matnr BINARY SEARCH.
                          WHILE sy-subrc EQ 0.
                            lv_tabix = sy-tabix + 1.

                            READ TABLE lt_orcamento INTO ls_orcamento_db
                              WITH KEY acnum   = p_acnum
                                       extwg   = ls_matkl-extwg
                                       matkl   = ls_matkl-matkl
                                       rcnum   = ls_rchdr-rcnum
                                       matnr   = ls_zfmrclst-matnr_ins
                                       period  = ls_period
                                       fazenda = ls_detailed-fazenda
                                       versao  = p_vers.
                            IF sy-subrc EQ 0.
                              CLEAR ls_consolidated.
                              READ TABLE lt_consolidated INTO ls_consolidated
                                WITH KEY period = ls_period BINARY SEARCH.

                              ls_orcamento_new = ls_orcamento_db.

                              CLEAR: lv_auxiliar, lv_passadas, lv_produto.
                              IF ls_zfmrclst-matnr(4) EQ 'TFOR'
                              OR ls_zfmrclst-matnr(4) EQ 'TIMP'.
                                ls_orcamento_new-aarea_form = ls_detailed-aareaform * ls_orcamento_new-passadas.
*-- Calcula Quantidade de Produto
                                IF ls_rchdr-ausme EQ 'VC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasform * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasform * ls_orcamento_new-rcdos.
                                ELSEIF ls_rchdr-ausme EQ 'HC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareaform * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareaform * ls_orcamento_new-rcdos.
                                ENDIF.
                              ELSEIF ls_zfmrclst-matnr(4) EQ 'TMAN'
                                  OR  ls_zfmrclst-matnr(4) EQ 'TCOL'.
                                ls_orcamento_new-aarea_manu = ls_detailed-aareamanu * ls_orcamento_new-passadas.
*-- Calcula Quantidade de Produto
                                IF ls_rchdr-ausme EQ 'VC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasmanu * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-bombasmanu * ls_orcamento_new-rcdos.
                                ELSEIF ls_rchdr-ausme EQ 'HC'.
*                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareamanu * ls_zfmrclst-rcdos.
                                  lv_produto = ls_orcamento_new-passadas * ls_detailed-aareamanu * ls_orcamento_new-rcdos.
                                ENDIF.
                              ENDIF.

                              IF ls_zfmrclst-rcinp EQ icon_wd_radio_button_empty.
                                CLEAR: ls_orcamento_new-aarea_manu, ls_orcamento_new-aarea_form.
                              ENDIF.

                              ls_orcamento_new-produtos = lv_produto.
                              ls_orcamento_new-custo = ls_orcamento_new-produtos * ls_detailed-valor.

                              IF ls_orcamento_new-produtos NE ls_orcamento_db-produtos
                              OR ls_orcamento_new-custo NE ls_orcamento_db-custo.
                                INSERT INITIAL LINE INTO TABLE gt_calculo ASSIGNING <ls_calculo>.
                                IF sy-subrc EQ 0.
                                  MOVE-CORRESPONDING ls_orcamento_new TO <ls_calculo>.
                                ENDIF.
                              ENDIF.
                            ENDIF.

                            READ TABLE lt_detailed INTO ls_detailed
                              INDEX lv_tabix COMPARING period tarefa.
                          ENDWHILE.
                        ENDIF.
*--------------------------------------------------------------------
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
      REFRESH: lt_receitas, lt_orcamento, lt_zfmrclst, lt_orc_full.
    ENDLOOP.
    REFRESH: lt_mara, lt_orcamento, lt_matkl, lt_receitas.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PROCESSES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_processes .

  gt_processos = zcl_agri_utilities=>zm_get_processos( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set .
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

*--Local table declaration
  DATA: lt_fcat    TYPE lvc_t_fcat,
*--Workarea declaration
        ls_variant TYPE disvariant,
        ls_stable  TYPE lvc_s_stbl,
        ls_layout  TYPE lvc_s_layo,
*--Variables
        lv_valid   TYPE char01,
        lv_input   TYPE i.

*--Set ALV attributes for layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
  ls_layout-smalltitle = abap_true.
  ls_variant-report = sy-repid.

*--Building field catalog
  PERFORM fcat_prepare.

  IF gobj_alv IS NOT BOUND.
*--Create Object for custom container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'C_MDMS_APR_0100_CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_alv
      EXPORTING
        i_parent = gobj_cont.

*--Displaying ALV Data
    CALL METHOD gobj_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_calculo
        it_fieldcatalog               = gt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE i035(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    gobj_alv->refresh_table_display( ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCAT_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fcat_prepare .

*--Field catalog prepare
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_ORC_FCAT' "'ZABS_ORCAMENTO'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      IF <fs_fcat>-fieldname = 'MANDT'
      OR <fs_fcat>-fieldname = 'USUARIO'
      OR <fs_fcat>-fieldname = 'DATA'
      OR <fs_fcat>-fieldname = 'HORA'
      OR <fs_fcat>-fieldname = 'TCODE'
      OR <fs_fcat>-fieldname = 'INSERIR'
      OR <fs_fcat>-fieldname = 'ELIMINAR'.
        <fs_fcat>-no_out = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR: ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Back Button
*&---------------------------------------------------------------------*
FORM fcode_back.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_REJECT
*&---------------------------------------------------------------------*
*& For Reject
*&---------------------------------------------------------------------*
FORM fcode_reject.

  DATA: lt_orcamento  TYPE STANDARD TABLE OF zabs_orcamento
                      INITIAL SIZE 0,
        lt_index_rows TYPE TABLE OF lvc_s_row,
        lv_titlebar   TYPE itex132,
        lv_question   TYPE itex132,
        lv_answer.

*-- Atualizar Tabela ZABS_ORCAMENTO?
*-- Eliminar linhas selecionadas da tabela ZABS_ORCAMENTO?
  MESSAGE i346(zfmfp) INTO lv_titlebar.
  PERFORM popup_to_confirm USING lv_titlebar
                                 TEXT-t02
                                 abap_true
                        CHANGING lv_answer.

  IF lv_answer NE '1'.
*-- Operação cancelada.
    MESSAGE s302(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    CALL METHOD gobj_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows
        et_row_no     = DATA(lt_row_no).

    IF lt_index_rows[] IS INITIAL.
*-- Selecione pelo menos uma linha!
      MESSAGE i347(zfmfp).
    ELSE.
      SORT lt_index_rows BY index DESCENDING.
      LOOP AT lt_index_rows INTO DATA(ls_rows).
        READ TABLE gt_update INTO DATA(ls_update) INDEX ls_rows-index.
        IF sy-subrc EQ 0.
          INSERT INITIAL LINE INTO TABLE lt_orcamento
            ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING ls_update TO <ls_orcamento>.
          ENDIF.
          DELETE gt_update INDEX ls_rows-index.
        ENDIF.
      ENDLOOP.

*      IF lt_orcamento[] IS NOT INITIAL.
*        DELETE zabs_orcamento FROM TABLE lt_orcamento.
*        IF sy-subrc EQ 0.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form FCODE_SAVE
*&---------------------------------------------------------------------*
*& For Save
*&---------------------------------------------------------------------*
FORM fcode_save.

  DATA: lt_update     TYPE STANDARD TABLE OF zabs_orcamento
                      INITIAL SIZE 0,
        lt_index_rows TYPE TABLE OF lvc_s_row,
        lv_titlebar   TYPE itex132,
        lv_question   TYPE itex132,
        lv_answer.

  CALL METHOD gobj_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = DATA(lt_row_no).

  IF lt_index_rows[] IS INITIAL.
*-- Selecione pelo menos uma linha!
    MESSAGE i347(zfmfp).
    RETURN.
  ENDIF.

*-- Gravar alterações na tabela ZABS_ORCAMENTO?
*-- Atenção: Deseja realmente efetivar as alterações na tabela ZABS_ORCAMENTO?
  MESSAGE i348(zfmfp) INTO lv_titlebar.
  PERFORM popup_to_confirm USING lv_titlebar
                                 TEXT-t03
                                 abap_true
                        CHANGING lv_answer.

  IF lv_answer NE '1'.
*-- Operação cancelada.
    MESSAGE s302(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_index_rows BY index DESCENDING.
    LOOP AT lt_index_rows INTO DATA(ls_rows).
      READ TABLE gt_calculo INTO DATA(ls_calculo) INDEX ls_rows-index.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE lt_update
          ASSIGNING FIELD-SYMBOL(<ls_orcamento>).

        IF <ls_orcamento> IS ASSIGNED.
          MOVE-CORRESPONDING ls_calculo TO <ls_orcamento>.
        ENDIF.

        DELETE gt_calculo INDEX ls_rows-index.
      ENDIF.
    ENDLOOP.

    IF lt_update[] IS NOT INITIAL.
      MODIFY zabs_orcamento FROM TABLE lt_update.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

*-- A tabela ZABS_ORCAMENTO foi atualizada com sucesso!
    MESSAGE i349(zfmfp).
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
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  CALL SCREEN 100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_WO_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_wo_alv .

  DATA: lt_update TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0.

  LOOP AT gt_calculo INTO DATA(ls_calculo).
    INSERT INITIAL LINE INTO TABLE lt_update
      ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
    IF <ls_orcamento> IS ASSIGNED.
      MOVE-CORRESPONDING ls_calculo TO <ls_orcamento>.
    ENDIF.
  ENDLOOP.

  IF lt_update[] IS NOT INITIAL.
    MODIFY zabs_orcamento FROM TABLE lt_update.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*-- A tabela ZABS_ORCAMENTO foi atualizada com sucesso!
  MESSAGE i349(zfmfp).

ENDFORM.
