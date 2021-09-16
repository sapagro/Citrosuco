*&---------------------------------------------------------------------*
*& Include ZABS_INC_ATUALIZA_SAFRA_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_glflcma, gt_fmfphdr, gt_cpros, gt_glmdhdr, gt_fcat.
  CLEAR: fcode, ok_code, gv_third_day, gv_monday, gv_sunday.
  FREE: gobj_alv, gobj_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_SEASON_UPD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_season_upd .

  DATA: lrt_atinn      TYPE RANGE OF atinn,
        lrt_atnam      TYPE RANGE OF atnam,
        lrs_atinn      LIKE LINE OF lrt_atinn,
        ls_duration    TYPE psen_duration,
        lv_begda_month TYPE begda,
        lv_endda_month TYPE endda.

  SELECT *
    FROM /agri/glflcma AS a
    INNER JOIN /agri/glflot AS t
    ON a~tplnr_fl EQ t~tplnr_fl
    INTO CORRESPONDING FIELDS OF TABLE @gt_glflcma
   WHERE t~tplnr_fl   IN @s_tplnr[]
     AND t~ownshp     EQ @c_ownership-own
     AND a~cmnum      EQ @p_cmnum
     AND a~varia      EQ @p_varia
     AND a~astat      NE 'C'
     AND a~iwerk      IN @s_iwerk[]
     AND a~loevm      EQ @abap_false
     AND a~zzprevisto EQ @abap_true.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    IF p_varia EQ c_crop_season-manutencao.
      INSERT INITIAL LINE INTO TABLE lrt_atnam
        ASSIGNING  FIELD-SYMBOL(<lrs_atnam>).
      IF sy-subrc EQ 0.
        <lrs_atnam> = 'IEQ'.
        <lrs_atnam>-low = 'INV-ATIVIDADE'.
      ENDIF.

      SELECT atinn, adzhl, atnam,
             atfor, anzst, anzdz
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
       WHERE atnam IN @lrt_atnam[].

      lrt_atinn = CORRESPONDING #( lt_cabn MAPPING low = atinn ).
      lrs_atinn = 'IEQ'.
      MODIFY lrt_atinn FROM lrs_atinn TRANSPORTING sign option WHERE low <> ''.

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~equnr, h~mpgrp,
             h~mdate, h~mtime, v~atzhl, v~atwrt,
             v~atflv, c~atinn, c~atnam
        INTO TABLE @gt_glmdhdr
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE h~mdtyp    EQ 'ZARV'
         AND h~aslvl    EQ 'A'
         AND h~tplnr_fl EQ @gt_glflcma-tplnr_fl
         AND h~contr    EQ @gt_glflcma-contr
         AND h~cmnum    EQ @gt_glflcma-cmnum
         AND h~mpgrp    EQ 'FAZ-INV-PLANTAS'
         AND h~ustat    EQ 'E0004'
         AND h~canceled EQ @abap_false
         AND v~atinn    IN @lrt_atinn[]
         AND v~atwrt    EQ '104'.

      SORT gt_glmdhdr BY tplnr_fl contr.

      LOOP AT gt_glflcma INTO DATA(ls_glflcma).
        DATA(lv_tabix) = sy-tabix.
        READ TABLE gt_glmdhdr INTO DATA(ls_glmdhdr)
          WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                   contr    = ls_glflcma-contr BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE gt_glflcma INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ELSEIF p_varia EQ c_crop_season-formacao.
*-- Seleciona Épocas de Cultura do mês anterior à data da tela de seleção
      lv_begda_month = p_date(6) && '01'.
      ls_duration-durmm = 1.
      CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
        EXPORTING
          im_date     = lv_begda_month
          im_operator = '-'
          im_duration = ls_duration
        IMPORTING
          ex_date     = lv_begda_month.

      CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
        EXPORTING
          i_day_in            = lv_begda_month
        IMPORTING
          e_last_day_of_month = lv_endda_month.

      DELETE gt_glflcma WHERE zzfazplantio
        NOT BETWEEN lv_begda_month AND lv_endda_month.

      SELECT *
        FROM /agri/glflcma AS a
        INNER JOIN /agri/glflot AS t
        ON a~tplnr_fl EQ t~tplnr_fl
        APPENDING CORRESPONDING FIELDS OF TABLE @gt_glflcma
       WHERE t~tplnr_fl IN @s_tplnr[]
         AND t~ownshp   EQ @c_ownership-own
         AND a~cmnum    EQ 'OUTRAS_CUL'
         AND a~varia    EQ 'AREA_LIVRE'
         AND a~astat    NE 'C'
         AND a~iwerk    IN @s_iwerk[]
         AND a~loevm    EQ @abap_false.

      SORT gt_glflcma BY tplnr_fl contr.
      DELETE ADJACENT DUPLICATES FROM gt_glflcma COMPARING tplnr_fl contr.

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~equnr, h~mpgrp,
             h~mdate, h~mtime, v~atzhl, v~atwrt,
             v~atflv, c~atinn, c~atnam
        INTO TABLE @gt_glmdhdr
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE h~mdtyp    EQ 'ZPTA'
         AND h~aslvl    EQ 'A'
         AND h~tplnr_fl EQ @gt_glflcma-tplnr_fl
         AND h~contr    EQ @gt_glflcma-contr
         AND h~cmnum    EQ @gt_glflcma-cmnum
         AND h~mpgrp    EQ 'FAZ-PLANTIO'
         AND h~canceled EQ @abap_false.

      SORT gt_glmdhdr BY tplnr_fl ASCENDING
                         contr    ASCENDING
                         mdocm    DESCENDING.

      DELETE ADJACENT DUPLICATES FROM gt_glmdhdr COMPARING tplnr_fl contr mdocm.

      SORT gt_glmdhdr BY tplnr_fl contr.
    ENDIF.
  ENDIF.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
*-- Fetch all CPROS assigned to crop and variant
    SELECT c~cmnum, c~varia, c~cpros, c~matnr,
           c~midur, c~miunt, t~descr
      FROM /agri/glcmprs AS c
      INNER JOIN /agri/glcmprst AS t
      ON  c~cmnum = t~cmnum
      AND c~varia = t~varia
      AND c~cpros = t~cpros
      INTO TABLE @gt_cpros
      FOR ALL ENTRIES IN @gt_glflcma
     WHERE c~cmnum EQ @gt_glflcma-cmnum
       AND c~varia EQ @gt_glflcma-varia
       AND t~spras EQ @sy-langu.

    SORT gt_cpros BY cmnum varia cpros.

*-- Process orders to do Techo (Technically Complete Order)
    SELECT aufnr, auart, autyp, tplnr_fl, contr,
           tplma, cmnum, varia, cpros, iwerk, tecom
      FROM /agri/fmfphdr
      INTO TABLE @gt_fmfphdr
      FOR ALL ENTRIES IN @gt_glflcma
     WHERE tplnr_fl EQ @gt_glflcma-tplnr_fl
       AND cmnum    EQ @gt_glflcma-cmnum
       AND varia    EQ @gt_glflcma-varia
       AND iwerk    EQ @gt_glflcma-iwerk
       AND tecom    EQ @abap_false.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_SEASON_ENH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_season_enh .

  DATA: lrt_atinn      TYPE RANGE OF atinn,
        lrt_atnam      TYPE RANGE OF atnam,
        lrt_varia      TYPE RANGE OF /agri/glvaria,
        lrs_atinn      LIKE LINE OF lrt_atinn,
        ls_duration    TYPE psen_duration,
        lv_begda_month TYPE begda,
        lv_endda_month TYPE endda.

  IF p_varia EQ c_crop_season-formacao.
*-- Fecth all crop season
    SELECT *
      FROM /agri/glflcma AS a
      INNER JOIN /agri/glflot AS t
      ON a~tplnr_fl EQ t~tplnr_fl
      INTO CORRESPONDING FIELDS OF TABLE @gt_glflcma
     WHERE t~tplnr_fl   IN @s_tplnr[]
       AND t~ownshp     EQ @c_ownership-own
       AND a~cmnum      EQ @p_cmnum
       AND a~varia      EQ @p_varia
       AND a~astat      NE 'C'
       AND a~iwerk      IN @s_iwerk[]
       AND a~loevm      EQ @abap_false.

    IF gt_glflcma[] IS NOT INITIAL.
*-- Fetch all CPROS assigned to crop and variant
      SELECT c~cmnum, c~varia, c~cpros, c~matnr,
             c~midur, c~miunt, t~descr
        FROM /agri/glcmprs AS c
        INNER JOIN /agri/glcmprst AS t
        ON  c~cmnum = t~cmnum
        AND c~varia = t~varia
        AND c~cpros = t~cpros
        INTO TABLE @gt_cpros
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE c~cmnum EQ @gt_glflcma-cmnum
         AND c~varia EQ @gt_glflcma-varia
         AND t~spras EQ @sy-langu.

      SORT gt_cpros BY cmnum varia cpros.

*-- Process orders to do Techo (Technically Complete Order)
      SELECT aufnr, auart, autyp, tplnr_fl, contr,
             tplma, cmnum, varia, cpros, iwerk, tecom
        FROM /agri/fmfphdr
        INTO TABLE @gt_fmfphdr
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE tplnr_fl EQ @gt_glflcma-tplnr_fl
         AND cmnum    EQ @gt_glflcma-cmnum
         AND varia    EQ @gt_glflcma-varia
         AND iwerk    EQ @gt_glflcma-iwerk
         AND tecom    EQ @abap_false.
    ENDIF.
  ELSEIF p_varia EQ c_crop_season-manutencao.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      INSERT INITIAL LINE INTO TABLE lrt_varia
        ASSIGNING FIELD-SYMBOL(<lrs_varia>).
      IF sy-subrc EQ 0.
        <lrs_varia> = 'IEQ'.
        CASE lv_index.
          WHEN 1.
            <lrs_varia>-low = c_crop_season-manutencao.
          WHEN 2.
            <lrs_varia>-low = c_crop_season-formacao.
        ENDCASE.
      ENDIF.
    ENDDO.

*-- Fecth all crop season
    SELECT *
      FROM /agri/glflcma AS a
      INNER JOIN /agri/glflot AS t
      ON a~tplnr_fl EQ t~tplnr_fl
      INTO CORRESPONDING FIELDS OF TABLE @gt_glflcma
     WHERE t~tplnr_fl   IN @s_tplnr[]
       AND t~ownshp     EQ @c_ownership-own
       AND a~cmnum      EQ @p_cmnum
       AND a~varia      IN @lrt_varia[]
       AND a~astat      NE 'C'
       AND a~iwerk      IN @s_iwerk[]
       AND a~loevm      EQ @abap_false.

    IF p_enh EQ abap_true
    AND p_enh1 EQ abap_true.
      IF p_date+4(4) EQ '0201'.
        DELETE gt_glflcma WHERE astat NE 'A'.
      ENDIF.
      IF p_varia EQ 'MANUT&COLHEITA'.
        DATA(lv_current) = p_date(4).
        DATA(lv_previous) = lv_current - 1.
        LOOP AT gt_glflcma INTO DATA(ls_glflcma).
          DATA(lv_tabix) = sy-tabix.
          IF ls_glflcma-datab(4) NE lv_current
          AND ls_glflcma-datab(4) NE lv_previous.
            DELETE gt_glflcma INDEX lv_tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF gt_glflcma[] IS NOT INITIAL.
*-- Fetch all CPROS assigned to crop and variant
      SELECT c~cmnum, c~varia, c~cpros, c~matnr,
             c~midur, c~miunt, t~descr
        FROM /agri/glcmprs AS c
        INNER JOIN /agri/glcmprst AS t
        ON  c~cmnum = t~cmnum
        AND c~varia = t~varia
        AND c~cpros = t~cpros
        INTO TABLE @gt_cpros
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE c~cmnum EQ @gt_glflcma-cmnum
         AND c~varia EQ @gt_glflcma-varia
         AND t~spras EQ @sy-langu.

      SORT gt_cpros BY cmnum varia cpros.

*-- Process orders to do Techo (Technically Complete Order)
      SELECT aufnr, auart, autyp, tplnr_fl, contr,
             tplma, cmnum, varia, cpros, iwerk, tecom
        FROM /agri/fmfphdr
        INTO TABLE @gt_fmfphdr
        FOR ALL ENTRIES IN @gt_glflcma
       WHERE tplnr_fl EQ @gt_glflcma-tplnr_fl
         AND cmnum    EQ @gt_glflcma-cmnum
         AND varia    EQ @gt_glflcma-varia
         AND iwerk    EQ @gt_glflcma-iwerk
         AND tecom    EQ @abap_false.
    ENDIF.
  ELSE.
*-- Variante &1 não prevista para ampliação!
    MESSAGE i361(zfmfp) WITH p_varia.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas de Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_SEASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_season .

  DATA: lt_flcma_create   TYPE /agri/t_glflcma,
        lt_csdoc_create   TYPE /agri/t_glcs_doc,
        lt_aufnr          TYPE /agri/t_fmaufnr,
        lt_msg_create     TYPE /agri/t_gprolog,
        lt_cskey_temp     TYPE /agri/t_glcs_key,
        lt_csdoc_temp     TYPE /agri/t_glcs_doc,
        lt_csdoc_change   TYPE /agri/t_glcs_doc,
        lt_fpdoc_tfor     TYPE /agri/t_fmfp_doc,
        lt_fpdoc_timp     TYPE /agri/t_fmfp_doc,
        lt_msg_change     TYPE /agri/t_gprolog,
        lt_orders         TYPE TABLE OF bapi_order_key,
        lt_factdays       TYPE STANDARD TABLE OF rke_dat INITIAL SIZE 0,
        lt_return_teco    TYPE cocf_t_bapi_return,
        lt_msg_teco       TYPE /agri/t_gprolog,
        ls_duration       TYPE psen_duration,
        ls_status         TYPE bapiret2,
        lv_log_create     TYPE balognr,
        lv_tplnr_fl       TYPE /agri/gltplnr_fl,
        lv_aufnr_out      TYPE /agri/glyaufnr,
        lv_subrc          TYPE sysubrc,
        lv_begda_month    TYPE begda,
        lv_data_aux       TYPE sy-datum,
        lv_endda_month    TYPE endda,
        lv_ref_begda      TYPE begda,
        lv_null_date      TYPE sydatum,
        lv_next_month     TYPE sydatum,
        lv_previous_month TYPE sydatum,
        lv_first_day      TYPE sydatum,
        lv_last_day       TYPE sydatum,
        lv_third_day      TYPE sydatum.

  FIELD-SYMBOLS: <ls_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  IF gt_glflcma[] IS INITIAL.
    RETURN.
  ENDIF.

  SELECT *
    FROM /agri/glseason
    INTO TABLE @DATA(lt_season).

  SORT gt_glflcma BY tplnr_fl contr.

  lt_cskey_temp = CORRESPONDING #( gt_glflcma ).

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      it_cskey       = lt_cskey_temp
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    IF sy-msgid IS NOT INITIAL
    AND sy-msgty IS NOT INITIAL
    AND sy-msgno IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.

      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        <ls_message>-msgid = sy-msgid.
        <ls_message>-msgno = sy-msgno.
        <ls_message>-msgty = sy-msgty.
        <ls_message>-msgv1 = sy-msgv1.
        <ls_message>-msgv2 = sy-msgv2.
        <ls_message>-msgv3 = sy-msgv3.
        <ls_message>-msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
  ELSE.
    SORT lt_csdoc_temp BY tplnr_fl contr.
    DATA(lt_terrain_distinct) = lt_csdoc_temp[].
    DELETE ADJACENT DUPLICATES FROM lt_terrain_distinct COMPARING tplnr_fl.

*-- Leitura Talhões
    LOOP AT lt_terrain_distinct INTO DATA(ls_terrain_distinct).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_terrain_distinct-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.
*-- Talhão: &1
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '357'.
        <ls_message>-msgty = 'I'.
        <ls_message>-msgv1 = lv_tplnr_fl.
      ENDIF.
*-- Verifica Fechamento MANUTENÇÃO
*-- Verifica Criação AREA_LIVRE
      REFRESH: lt_csdoc_change, lt_flcma_create,
               lt_msg_change, lt_msg_create.
      READ TABLE lt_csdoc_temp INTO DATA(ls_csdoc_temp)
        WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        DATA(lv_contr) = ls_csdoc_temp-contr.
        DATA(lv_third_factday) = abap_false.

        UNASSIGN <ls_csdoc>.
        ASSIGN ls_csdoc_temp TO <ls_csdoc>.
        IF <ls_csdoc> IS ASSIGNED.
          READ TABLE gt_glflcma INTO DATA(ls_glflcma)
            WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                     contr    = <ls_csdoc>-contr BINARY SEARCH.
          IF sy-subrc EQ 0.
*===================================================================*
*-- Época de Cultura [MANUTENÇÃO]
            IF <ls_csdoc>-x-cshdr-varia EQ c_crop_season-manutencao.
              CLEAR: lv_last_day.

*-- Verifica último dia do mês da Data de Medição
              READ TABLE gt_glmdhdr INTO DATA(ls_glmdhdr)
                WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                         contr    = <ls_csdoc>-contr BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF ls_glmdhdr-mdate IS NOT INITIAL.
                  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                    EXPORTING
                      date                      = ls_glmdhdr-mdate
                    EXCEPTIONS
                      plausibility_check_failed = 1
                      OTHERS                    = 2.
                  IF sy-subrc EQ 0.
*-- Último dia do mês da data de medição
                    CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                      EXPORTING
                        i_day_in            = ls_glmdhdr-mdate
                      IMPORTING
                        e_last_day_of_month = lv_last_day.

                    IF gv_third_day NE lv_null_date
                    AND lv_last_day NE lv_null_date
                    AND p_date EQ gv_third_day.
                      lv_third_factday = abap_true.
*-- Último dia do mês da data de medição -> Data Final Safra
                      <ls_csdoc>-x-cshdr-datbi = lv_last_day.
*-- Último dia do mês da data de medição -> Data Final (Processos Época Cultura)
                      LOOP AT <ls_csdoc>-x-csprs ASSIGNING FIELD-SYMBOL(<ls_csprs>).
                        <ls_csprs>-updkz = 'U'.
                        <ls_csprs>-enddt = lv_last_day.
                      ENDLOOP.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_third_factday = abap_true.
                <ls_csdoc>-x-cshdr-updkz = 'U'.
*-- Fecha Época de Cultura
                <ls_csdoc>-x-cshdr-astat = 'C'.
                CLEAR: <ls_csdoc>-x-cshdr-zzprev_errad,
                       <ls_csdoc>-x-cshdr-zzprevisto.
                APPEND <ls_csdoc> TO lt_csdoc_change.
*-------------------------------------------------------------------*
                PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                           CHANGING lt_csdoc_change
                                                    lt_msg_change
                                                    lv_subrc.
*-------------------------------------------------------------------*
*-- Encerra todas as ordens [MANUT&COLHEITA]
                PERFORM mass_teco USING c_crop_season-manutencao
                                        c_crop_process-close_all
                                        ls_glflcma.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio [MANUT&COLHEITA]
                PERFORM yard_tech_complete USING ls_glflcma-yaufnr.
*-------------------------------------------------------------------*
                DATA(lv_forma) = abap_false.
*-- Verifica Linha Ativa Prevista [FORMAÇÃO]
                READ TABLE lt_csdoc_temp ASSIGNING FIELD-SYMBOL(<ls_csdoc_forma>)
                  WITH KEY tplnr_fl           =  ls_terrain_distinct-tplnr_fl
                           x-cshdr-cmnum      = 'CITROS'
                           x-cshdr-varia      = c_crop_season-formacao
                           x-cshdr-zzprevisto = abap_true.
                IF sy-subrc EQ 0.
                  lv_forma = abap_true.
                ENDIF.

*-- Cria Época de Cultura de Área Livre
*-- Cultura [OUTRAS_CUL]; Variante [AREA_LIVRE]
                INSERT INITIAL LINE INTO TABLE lt_flcma_create
                  ASSIGNING FIELD-SYMBOL(<ls_flcma_create>).
                IF sy-subrc EQ 0.
                  <ls_flcma_create>-tplnr_fl = <ls_csdoc>-tplnr_fl.
                  <ls_flcma_create>-cmnum = 'OUTRAS_CUL'.
                  <ls_flcma_create>-varia = 'AREA_LIVRE'.
*                  <ls_flcma_create>-season = 'SAFRA PROD'.
                  <ls_flcma_create>-season = 'SAFRA PLAN'.
                  <ls_flcma_create>-updkz = c_updkz_new.
                  <ls_flcma_create>-astat = 'I'.
*-- Data Inicial Área Livre = Primeiro dia do mês seguinte à Data Final de Manutenção
                  lv_ref_begda = <ls_csdoc>-x-cshdr-datbi(6) && '01'.

                  CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
                    EXPORTING
                      months  = '1'
                      olddate = lv_ref_begda
                    IMPORTING
                      newdate = lv_ref_begda.

                  <ls_flcma_create>-datab = lv_ref_begda.
*-- Caso NÃO exista Época de Cultura [FORMAÇÃO]
*-- Data Final Área Livre = 31.12.9999
                  IF lv_forma EQ abap_false.
                    <ls_flcma_create>-datbi = '99991231'.
                  ELSE.
                    IF <ls_csdoc_forma>-x-cshdr-datab IS NOT INITIAL.
*-- Caso EXISTA Épcoa de Cultura [FORMAÇÃO]
*-- Subtrai 1 dia da Data Inicial [FORMAÇÃO]
                      ls_duration-durdd = 1.
                      CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                        EXPORTING
                          im_date     = <ls_csdoc_forma>-x-cshdr-datab
                          im_operator = '-'
                          im_duration = ls_duration
                        IMPORTING
                          ex_date     = lv_data_aux.
                      <ls_flcma_create>-datbi = lv_data_aux.
                    ENDIF.
                  ENDIF.
                  <ls_flcma_create>-class = '1'.
                  <ls_flcma_create>-zzdate = <ls_flcma_create>-datbi.
                ENDIF.
              ENDIF.
*===================================================================*
*-- Época de Cultura FORMAÇÃO
            ELSEIF <ls_csdoc>-x-cshdr-varia EQ c_crop_season-formacao.
*-- Verifica último dia do mês da Data de Medição
              CLEAR ls_glmdhdr.
              READ TABLE gt_glmdhdr INTO ls_glmdhdr
                WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                         contr    = <ls_csdoc>-contr BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF ls_glmdhdr-mdate IS NOT INITIAL.
                  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                    EXPORTING
                      date                      = ls_glmdhdr-mdate
                    EXCEPTIONS
                      plausibility_check_failed = 1
                      OTHERS                    = 2.
                  IF sy-subrc EQ 0.
                    CLEAR: ls_duration, lv_first_day, lv_next_month,
                           lv_last_day, lv_third_day.
                    ls_duration-durmm = 1.
                    lv_first_day = ls_glmdhdr-mdate(6) && '01'.

                    CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                      EXPORTING
                        im_date     = lv_first_day
                        im_operator = '+'
                        im_duration = ls_duration
                      IMPORTING
                        ex_date     = lv_first_day.

*-- Verifica 3o. dia útil do mês seguinte à data de medição
                    lv_last_day = lv_first_day.
                    ADD 15 TO lv_last_day.

                    REFRESH lt_factdays.
                    CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
                      EXPORTING
                        i_datab               = lv_first_day
                        i_datbi               = lv_last_day
                        i_factid              = gv_factid
                      TABLES
                        eth_dats              = lt_factdays
                      EXCEPTIONS
                        date_conversion_error = 1
                        OTHERS                = 2.

                    IF sy-subrc <> 0.
*-- Erro ao ler calendário fábrica!
                      MESSAGE i362(zfmfp).
                      LEAVE LIST-PROCESSING.
                    ELSE.
                      LOOP AT lt_factdays INTO DATA(ls_factday).
                        IF sy-tabix EQ 3.
                          lv_third_day = ls_factday.
                        ENDIF.
                      ENDLOOP.
                    ENDIF.

                    IF p_date NE lv_null_date
                    AND p_date EQ lv_third_day.
                      lv_third_factday = abap_true.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_third_factday EQ abap_true.
                CLEAR: lv_begda_month, lv_endda_month, ls_duration,
                       <ls_csdoc>-x-cshdr-zzprevisto,
                       <ls_csdoc>-x-cshdr-zzprev_plantio.

                <ls_csdoc>-x-cshdr-updkz = 'U'.

*-- Verifica Linha Ativa Prevista de FORMAÇÃO
                READ TABLE lt_csdoc_temp ASSIGNING FIELD-SYMBOL(<ls_area_livre>)
                  WITH KEY tplnr_fl      =  ls_terrain_distinct-tplnr_fl
                           x-cshdr-cmnum = 'OUTRAS_CUL'
                           x-cshdr-varia = 'AREA_LIVRE'.
                IF sy-subrc EQ 0.
                  <ls_area_livre>-x-cshdr-updkz = 'U'.
*-- Fecha Época de Cultura [AREA_LIVRE]
                  <ls_area_livre>-x-cshdr-astat = 'C'.
*-- A referência é o mês Data Plantio [FORMAÇÃO]
                  IF <ls_csdoc>-x-cshdr-zzfazplantio IS NOT INITIAL.
                    lv_begda_month = <ls_csdoc>-x-cshdr-zzfazplantio(6) && '01'.

                    CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                      EXPORTING
                        i_day_in            = lv_begda_month
                      IMPORTING
                        e_last_day_of_month = lv_endda_month.

*-- Último dia mês Data Plantio [FORMAÇÃO] -> Data Final [AREA_LIVRE]
                    <ls_area_livre>-x-cshdr-datbi = lv_endda_month.
                    <ls_area_livre>-x-cshdr-updkz = 'U'.
*-- Último dia mês Data Plantio [FORMAÇÃO] -> Data Final (Proc.Épocas Cultura) [AREA_LIVRE]
                    LOOP AT <ls_area_livre>-x-csprs ASSIGNING <ls_csprs>.
                      <ls_csprs>-updkz = 'U'.
                      <ls_csprs>-enddt = lv_endda_month.
                    ENDLOOP.
                    APPEND <ls_area_livre> TO lt_csdoc_change.
                  ENDIF.
                ENDIF.

*-- 1o. dia mês posterior Data Plantio [FORMAÇÃO] -> Data Inicial [FORMAÇÃO]
                IF <ls_csdoc>-x-cshdr-zzfazplantio IS NOT INITIAL.
                  CLEAR: lv_begda_month, lv_endda_month, ls_duration.
                  lv_begda_month = <ls_csdoc>-x-cshdr-zzfazplantio(6) && '01'.
                  ls_duration-durmm = 1.

                  CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                    EXPORTING
                      im_date     = lv_begda_month
                      im_operator = '+'
                      im_duration = ls_duration
                    IMPORTING
                      ex_date     = lv_begda_month.

                  <ls_csdoc>-x-cshdr-updkz = 'U'.
                  <ls_csdoc>-x-cshdr-datab = lv_begda_month.
                  <ls_csdoc>-x-cshdr-exhad = lv_begda_month.
                  <ls_csdoc>-x-cshdr-astat = 'A'.

*-- Recálculo Data Final de acordo com /AGRI/GLSEASON
*-- SAFRA PLAN [FORMAÇÃO]
                  READ TABLE lt_season INTO DATA(ls_season_form)
                    WITH KEY season = 'SAFRA PLAN'.
                  IF sy-subrc EQ 0.
                    IF ls_season_form-offst LE 999.
                      CLEAR ls_duration.
                      ls_duration-duryy = ls_season_form-offst.
                      CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                        EXPORTING
                          im_date     = <ls_csdoc>-x-cshdr-zzfazplantio
                          im_operator = '+'
                          im_duration = ls_duration
                        IMPORTING
                          ex_date     = lv_endda_month.

                      CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                        EXPORTING
                          i_day_in            = lv_endda_month
                        IMPORTING
                          e_last_day_of_month = lv_endda_month.

                      <ls_csdoc>-x-cshdr-datbi = lv_endda_month.

*-- Data Final [FORMAÇÃO] -> Data Final (Processos de épocas de cultura)
                      LOOP AT <ls_csdoc>-x-csprs ASSIGNING <ls_csprs>.
                        <ls_csprs>-strdt = <ls_csdoc>-x-cshdr-datab.
                        <ls_csprs>-enddt = <ls_csdoc>-x-cshdr-datbi.
                        <ls_csprs>-updkz = 'U'.
                      ENDLOOP.
                    ENDIF.
                  ENDIF.
                  APPEND <ls_csdoc> TO lt_csdoc_change.
                ENDIF.
*-------------------------------------------------------------------*
                PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                           CHANGING lt_csdoc_change
                                                    lt_msg_change
                                                    lv_subrc.
*-------------------------------------------------------------------*
* Technical Complete
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: IMPLANT
*-------------------------------------------------------------------*
*-- Encerra ordens [FORMAÇÃO] [IMPLANT]
                PERFORM mass_teco USING c_crop_season-formacao
                                        c_crop_process-implantacao
                                        ls_glflcma.
*-------------------------------------------------------------------*
* Create new Process Order for FORMAÇÃO
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: FORMAÇÃO
*-------------------------------------------------------------------*
                PERFORM create_process_order USING ls_glflcma
                                                   c_crop_process-formacao
                                                   lv_tplnr_fl
                                                   <ls_csdoc>-x-cshdr-datab
                                          CHANGING lt_fpdoc_tfor.
*-------------------------------------------------------------------*
* Create new Task Order for TFORGEN/TINSP0001
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: FORMAÇÃO
*-------------------------------------------------------------------*
                READ TABLE lt_fpdoc_tfor INTO DATA(ls_fpdoc_tfor) INDEX 1.
                IF sy-subrc EQ 0.
                  PERFORM document_infocus_lock USING ls_fpdoc_tfor-aufnr
                                             CHANGING lv_subrc.
                  IF lv_subrc EQ 0.
                    PERFORM create_task_order USING lv_tplnr_fl
                                                    'TFORGEN'
                                           CHANGING lt_fpdoc_tfor.

                    PERFORM create_task_order USING lv_tplnr_fl
                                                    'TINSP0001'
                                           CHANGING lt_fpdoc_tfor.

                    PERFORM document_infocus_unlock USING ls_fpdoc_tfor-aufnr
                                                          ls_fpdoc_tfor-x-fphdr-rsnum.
                  ENDIF.
                ENDIF.
*-------------------------------------------------------------------*
* Create new Process Order for COLHEITA
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: COLHEITA
*-------------------------------------------------------------------*
                PERFORM create_process_order USING ls_glflcma
                                                   c_crop_process-colheita
                                                   lv_tplnr_fl
                                                   <ls_csdoc>-x-cshdr-datab
                                          CHANGING lt_fpdoc_timp.
*-------------------------------------------------------------------*
* Create Yard Order
*-------------------------------------------------------------------*
*/AGRI/SAPLGLCSM -> FCODE_MYCRE -> PERFORM create_yard_order CHANGING lv_subrc.
                IF <ls_csdoc>-x-cshdr-yaufnr IS INITIAL.
                  CLEAR lv_subrc.
                  PERFORM create_yard_order CHANGING <ls_csdoc>
                                                     lv_subrc.
                  IF lv_subrc EQ 0
                  AND <ls_csdoc>-x-cshdr-yaufnr IS NOT INITIAL.
                    <ls_csdoc>-x-cshdr-updkz = c_updkz_update.
                    APPEND <ls_csdoc> TO lt_csdoc_change.
                  ENDIF.
                ELSE.
*-- Talhão &1: Ordem Pátio &2 existe para Época de Cultura 'FORMAÇÃO'.
                  INSERT INITIAL LINE INTO TABLE gt_message
                    ASSIGNING <ls_message>.
                  IF sy-subrc EQ 0.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = <ls_csdoc>-x-cshdr-yaufnr
                      IMPORTING
                        output = lv_aufnr_out.

                    <ls_message>-msgid = 'ZFMFP'.
                    <ls_message>-msgno = '370'.
                    <ls_message>-msgty = 'S'.
                    <ls_message>-msgv1 = lv_tplnr_fl.
                    <ls_message>-msgv2 = lv_aufnr_out.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE lt_csdoc_temp INTO ls_csdoc_temp
          INDEX lv_tabix COMPARING tplnr_fl.
      ENDWHILE.

      PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                 CHANGING lt_csdoc_change
                                          lt_msg_change
                                          lv_subrc.

*-- Cria Época de Cultura de Área Livre
*-- Cultura [OUTRAS_CUL]; Variante [AREA_LIVRE]
      PERFORM flcma_create    USING lv_tplnr_fl
                           CHANGING lt_flcma_create
                                    lt_csdoc_create
                                    lt_msg_create
                                    lv_log_create
                                    lv_subrc.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ENHANCE_SEASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM enhance_season .

  IF p_varia EQ c_crop_season-formacao.
*-- Amplia a Safra DE 'FORMAÇÃO' PARA 'MANUT&COLHEITA'
*-- ASTAT = 'I' (Inativo) + ZZPREVISTO = abap_true
*-- ANLNR = space + KOSTL = Tabela ZABS_CENTRO_CUSTO
    IF p_date+4(4) EQ '0201'.
      PERFORM close_formacao.
    ENDIF.
  ELSEIF p_varia EQ c_crop_season-manutencao.
*-- Manutenção -> Manutenção
    IF p_enh1 EQ abap_true.
      PERFORM enhance_manutencao.
*-- Formação -> Manutenção
    ELSEIF p_enh2 EQ abap_true.
      PERFORM activate_manutencao.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  DATA: lt_factdays       TYPE STANDARD TABLE OF rke_dat INITIAL SIZE 0,
        ls_duration       TYPE psen_duration,
        lv_previous_month TYPE sydatum,
        lv_first_day      TYPE sydatum,
        lv_last_day       TYPE sydatum.

  IF p_cmnum IS INITIAL.
*-- Informar Mestre de Cultura!
    MESSAGE i352(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_varia IS INITIAL.
*-- Informar Variante!
    MESSAGE i353(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_upd EQ abap_true.
    IF p_varia EQ c_crop_season-manutencao.
      CLEAR: gv_third_day, gv_monday, gv_sunday.

      lv_first_day = p_date(6) && '01'.
      lv_last_day = lv_first_day.
      ADD 15 TO lv_last_day.

      CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
        EXPORTING
          i_datab               = lv_first_day
          i_datbi               = lv_last_day
          i_factid              = gv_factid
        TABLES
          eth_dats              = lt_factdays
        EXCEPTIONS
          date_conversion_error = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
*-- Erro ao ler calendário fábrica!
        MESSAGE i362(zfmfp).
        LEAVE LIST-PROCESSING.
      ELSE.
        LOOP AT lt_factdays INTO DATA(ls_factday).
          IF sy-tabix EQ 3.
            gv_third_day = ls_factday.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date   = lv_first_day
        IMPORTING
          monday = gv_monday
          sunday = gv_sunday.

      IF p_date NE gv_third_day.
*-- Não há dados para processamento! 3o. dia útil do mês: &1.
        MESSAGE i393(zfmfp) WITH gv_third_day.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_MSGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_CONSTANTS_MSGTY_SUCCESS
*&      --> P_
*&      --> LV_MSGV1
*&      --> SPACE
*&      <-- GT_MESSAGE[]
*&---------------------------------------------------------------------*
FORM build_msgs USING pv_msgty    TYPE msgty
                      pv_msgno    TYPE msgno
                      pv_msgv1    TYPE msgv1
                      pv_msgv2    TYPE msgv2
             CHANGING ct_messages TYPE /agri/t_gprolog.

*-- Local work areas
  DATA: ls_messages TYPE /agri/s_gprolog.

  ls_messages-msgid = 'ZFMFP'.
  ls_messages-msgty = pv_msgty.
  ls_messages-msgno = pv_msgno.
  ls_messages-msgv1 = pv_msgv1.
  ls_messages-msgv2 = pv_msgv2.
  APPEND ls_messages TO ct_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------**
FORM document_infocus_lock  USING lv_aufnr TYPE /agri/fmfpnum
                         CHANGING lv_subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  CALL FUNCTION 'ENQUEUE_ESORDER'
    EXPORTING
      aufnr          = lv_aufnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc IS NOT INITIAL.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    INSERT INITIAL LINE INTO TABLE gt_message
      ASSIGNING FIELD-SYMBOL(<ls_message>).
    IF sy-subrc EQ 0.
*-- A ordem &1 está bloqueada pelo usuário &2
      <ls_message>-msgid = '/AGRI/FMFP'.
      <ls_message>-msgno = '037'.
      <ls_message>-msgty = 'I'.
      <ls_message>-msgv1 = lv_aufnr.
      <ls_message>-msgv2 = lv_msgv1.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock USING lv_aufnr TYPE /agri/fmfpnum
                                   lv_rsnum TYPE rsnum.

  CALL FUNCTION 'DEQUEUE_ESORDER'
    EXPORTING
      aufnr     = lv_aufnr
      _synchron = abap_true.

  CALL FUNCTION 'DEQUEUE_EMRKPF'
    EXPORTING
      rsnum     = lv_rsnum
      _synchron = abap_true.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK

*&---------------------------------------------------------------------*
*& Form CREATE_TASK_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FPDOC
*&---------------------------------------------------------------------*
FORM create_task_order USING lv_tplnr_fl TYPE /agri/gltplnr_fl
                             lv_matnr    TYPE matnr
                    CHANGING lt_fpdoc    TYPE /agri/t_fmfp_doc.

  DATA: lt_fpitm    TYPE /agri/t_fmfpitm,
        lt_messages TYPE /agri/t_gprolog,
        lv_aufnr    TYPE /agri/fmfpnum.

  READ TABLE lt_fpdoc ASSIGNING FIELD-SYMBOL(<ls_fpdoc>) INDEX 1.
  IF sy-subrc EQ 0.
*-- LTXA1: TAREFA GENERICA PARA CUSTOS DE FORMAÇÃO ou MANUTENÇÃO
    READ TABLE <ls_fpdoc>-x-fpcom INTO DATA(ls_fpcom)
      WITH KEY matnr = lv_matnr.
    IF sy-subrc EQ 0.
      READ TABLE <ls_fpdoc>-x-fpitm ASSIGNING FIELD-SYMBOL(<ls_fpitm>)
        WITH KEY posnr = ls_fpcom-posnr
                 vornr = ls_fpcom-vornr.
      IF sy-subrc EQ 0.
        IF <ls_fpitm>-aufnr_to IS INITIAL.
          <ls_fpdoc>-x-fphdr-updkz = c_updkz_update.
          <ls_fpitm>-matnr = ls_fpcom-matnr.
          <ls_fpitm>-schdt = sy-datum.
          <ls_fpitm>-updkz = c_updkz_update.

          REFRESH lt_messages.
          CALL FUNCTION '/AGRI/FMFP_SAVE'
            EXPORTING
*             I_SET_UPDATE_TASK = 'X'
              i_commit_work = 'X'
            CHANGING
              ct_fpdoc      = lt_fpdoc
              ct_messages   = lt_messages
            EXCEPTIONS
              no_change     = 1
              OTHERS        = 2.

          APPEND LINES OF lt_messages TO gt_message.

          REFRESH lt_fpitm.
          INSERT INITIAL LINE INTO TABLE lt_fpitm
            ASSIGNING FIELD-SYMBOL(<ls_fpitm_new>).
          IF sy-subrc EQ 0.
            <ls_fpitm_new> = <ls_fpitm>.
            <ls_fpitm_new>-tomng = <ls_fpitm_new>-gamng.
          ENDIF.

          IF lt_fpitm[] IS NOT INITIAL.
            REFRESH lt_messages.
            CALL FUNCTION '/AGRI/FMFP_TASK_ORDER_CREATE'
              EXPORTING
*               I_SAVE_MESSAGES   = ' '
                i_commit_work     = 'X'
                it_fpitm          = lt_fpitm
              IMPORTING
                et_todoc          = lt_fpdoc
                et_messages       = lt_messages
              EXCEPTIONS
                inconsistent_data = 1
                no_data           = 2
                OTHERS            = 3.

            APPEND LINES OF lt_messages TO gt_message.
          ENDIF.
        ELSE.
*-- Talhão &1: Ordem Processo &2 Ordem Tarefa &3 existentes para &4!
          INSERT INITIAL LINE INTO TABLE gt_message
            ASSIGNING FIELD-SYMBOL(<ls_message>).
          IF sy-subrc EQ 0.
            <ls_message>-msgid = 'ZFMFP'.
            <ls_message>-msgno = '365'.
            <ls_message>-msgty = 'I'.
            <ls_message>-msgv1 = lv_tplnr_fl.
            <ls_message>-msgv2 = <ls_fpitm>-aufnr.
            <ls_message>-msgv3 = <ls_fpitm>-aufnr_to.
            <ls_message>-msgv4 = lv_matnr.
          ENDIF.
        ENDIF.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <ls_fpdoc>-aufnr
          IMPORTING
            output = lv_aufnr.

*-- Talhão &1: Não existe material &2 em Ordem Processo &3!
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '366'.
          <ls_message>-msgty = 'I'.
          <ls_message>-msgv1 = lv_tplnr_fl.
          <ls_message>-msgv2 = lv_matnr.
          <ls_message>-msgv3 = lv_aufnr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLOSE_FORMACAO
*&---------------------------------------------------------------------*
*& Amplia a Safra DE 'FORMAÇÃO' PARA 'MANUT&COLHEITA'
*& ASTAT = 'I' (Inativo) + ZZPREVISTO = abap_true
*& ANLNR = space + KOSTL = Tabela ZABS_CENTRO_CUSTO
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM close_formacao .

  DATA: lt_flcma_create TYPE /agri/t_glflcma,
        lt_csdoc_create TYPE /agri/t_glcs_doc,
        lt_aufnr        TYPE /agri/t_fmaufnr,
        lt_msg_create   TYPE /agri/t_gprolog,
        lt_cskey_temp   TYPE /agri/t_glcs_key,
        lt_csdoc_temp   TYPE /agri/t_glcs_doc,
        lt_csdoc_change TYPE /agri/t_glcs_doc,
        lt_msg_change   TYPE /agri/t_gprolog,
        lt_msg_teco     TYPE /agri/t_gprolog,
        lt_msg_fmfp     TYPE /agri/t_gprolog,
        lt_fpdoc        TYPE /agri/t_fmfp_doc,
        lt_factdays     TYPE STANDARD TABLE OF rke_dat INITIAL SIZE 0,
        lt_cskey        TYPE STANDARD TABLE OF /agri/s_glcs_key INITIAL SIZE 0,
        ls_duration     TYPE psen_duration,
        lv_log_create   TYPE balognr,
        lv_tplnr_fl     TYPE /agri/gltplnr_fl,
        lv_null_date    TYPE sydatum,
        lv_endda_form1  TYPE sydatum,
        lv_endda_form2  TYPE sydatum,
        lv_begda_manu1  TYPE sydatum,
        lv_endda_manu1  TYPE sydatum,
        lv_begda_manu2  TYPE sydatum,
        lv_endda_manu2  TYPE sydatum,
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2,
        lv_subrc        TYPE sysubrc,
        lv_begda        TYPE abdatab,
        lv_endda        TYPE abdatbi,
        lv_gstrp        TYPE co_gstrp.

  FIELD-SYMBOLS: <ls_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas de  Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    RETURN.
  ENDIF.

  SELECT *
    FROM /agri/glseason
    INTO TABLE @DATA(lt_season).

*-- SAFRA PLAN [FORMAÇÃO]
  READ TABLE lt_season INTO DATA(ls_formacao)
    WITH KEY season = 'SAFRA PLAN'.
  IF sy-subrc EQ 0.
*-- Período final da época (DD MM)
    lv_endda_form1 = p_date(4) && ls_formacao-eperd.
    DATA(lv_next_year) = p_date(4) + 1.
    lv_endda_form2 = lv_next_year && ls_formacao-eperd.
  ENDIF.

  IF gt_glflcma[] IS NOT INITIAL.
    SELECT *
      FROM zabs_csks
      INTO TABLE @DATA(lt_csks)
      FOR ALL ENTRIES IN @gt_glflcma
     WHERE cmnum = @gt_glflcma-cmnum
       AND varia = @c_crop_season-manutencao
       AND iwerk = @gt_glflcma-iwerk.

    SORT lt_csks BY cmnum varia iwerk.
  ENDIF.

  SORT gt_glflcma BY tplnr_fl contr.

  lt_cskey_temp = CORRESPONDING #( gt_glflcma ).

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      it_cskey       = lt_cskey_temp
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    IF sy-msgid IS NOT INITIAL
    AND sy-msgty IS NOT INITIAL
    AND sy-msgno IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.

      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        <ls_message>-msgid = sy-msgid.
        <ls_message>-msgno = sy-msgno.
        <ls_message>-msgty = sy-msgty.
        <ls_message>-msgv1 = sy-msgv1.
        <ls_message>-msgv2 = sy-msgv2.
        <ls_message>-msgv3 = sy-msgv3.
        <ls_message>-msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
  ELSE.
    SORT lt_csdoc_temp BY tplnr_fl contr.
    DATA(lt_terrain_distinct) = lt_csdoc_temp[].
    DELETE ADJACENT DUPLICATES FROM lt_terrain_distinct COMPARING tplnr_fl.

*-- Leitura Talhões
    LOOP AT lt_terrain_distinct INTO DATA(ls_terrain_distinct).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_terrain_distinct-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.
*-- Talhão: &1
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '357'.
        <ls_message>-msgty = 'I'.
        <ls_message>-msgv1 = lv_tplnr_fl.
      ENDIF.

      REFRESH: lt_csdoc_change, lt_flcma_create,
               lt_msg_change, lt_msg_create,
               lt_aufnr, lt_msg_teco.
      READ TABLE lt_csdoc_temp INTO DATA(ls_csdoc_temp)
        WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        DATA(lv_contr) = ls_csdoc_temp-contr.

        UNASSIGN <ls_csdoc>.
        ASSIGN ls_csdoc_temp TO <ls_csdoc>.
        IF <ls_csdoc> IS ASSIGNED.
          READ TABLE gt_glflcma INTO DATA(ls_glflcma)
            WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                     contr    = <ls_csdoc>-contr BINARY SEARCH.
          IF sy-subrc EQ 0.
*===================================================================*
*-- Época de Cultura/Variante: [FORMAÇÃO]
            IF <ls_csdoc>-x-cshdr-varia EQ c_crop_season-formacao.
              CLEAR: lv_begda_manu1, lv_begda_manu2,
                     lv_endda_manu1, lv_endda_manu2.

              DATA(lv_create1) = abap_true.
*-- 01.04.2021~31.03.2022 > 30.06.2021 ?
              IF <ls_csdoc>-x-cshdr-datbi GE lv_endda_form1.
                lv_create1 = abap_false.
              ENDIF.

              DATA(lv_create2) = abap_true.
*-- 01.04.2021~31.03.2022 > 30.06.2022 ?
              IF <ls_csdoc>-x-cshdr-datbi GE lv_endda_form2.
                lv_create2 = abap_false.
              ENDIF.

*-- SAFRA PROD [MANUTENÇÃO]
              READ TABLE lt_season INTO DATA(ls_manutencao)
                WITH KEY season = 'SAFRA PROD'.
              IF sy-subrc EQ 0.
*-- Dt.Final Varia FORMAÇÃO 01.04.2021 [DATAB] < Dt.Final Conf. FORMAÇÃO 30.06.2021
                IF lv_create1 EQ abap_true.
                  lv_begda_manu1 = <ls_csdoc>-x-cshdr-datbi + 1.
                  lv_endda_manu1 = lv_endda_form1.

                  CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                    EXPORTING
                      i_day_in            = lv_endda_manu1
                    IMPORTING
                      e_last_day_of_month = lv_endda_manu1.

*-- Cria Época de Cultura de Manutenção
*-- Cultura [CITROS]; Variante [MANUT&COLHEITA]
                  INSERT INITIAL LINE INTO TABLE lt_flcma_create
                    ASSIGNING FIELD-SYMBOL(<ls_flcma_create>).
                  IF sy-subrc EQ 0.
                    <ls_flcma_create>-tplnr_fl = <ls_csdoc>-tplnr_fl.
                    <ls_flcma_create>-cmnum = 'CITROS'.
                    <ls_flcma_create>-varia = c_crop_season-manutencao.
                    <ls_flcma_create>-season = 'SAFRA PROD'.
                    <ls_flcma_create>-updkz = c_updkz_new.
                    <ls_flcma_create>-astat = 'I'.
                    <ls_flcma_create>-zzprevisto = abap_true.
                    <ls_flcma_create>-datab = lv_begda_manu1.
                    <ls_flcma_create>-datbi = lv_endda_manu1.
                    <ls_flcma_create>-class = '1'.
                    <ls_flcma_create>-zzdate = <ls_flcma_create>-datbi.
                    <ls_flcma_create>-zzmatnr = <ls_csdoc>-x-cshdr-zzmatnr.
                    <ls_flcma_create>-ymatnr = <ls_csdoc>-x-cshdr-ymatnr.
                    <ls_flcma_create>-zzfazplantio = <ls_csdoc>-x-cshdr-zzfazplantio.
                    <ls_flcma_create>-zzfazvartecnia = <ls_csdoc>-x-cshdr-zzfazvartecnia.
                    <ls_flcma_create>-zzporta_enxerto = <ls_csdoc>-x-cshdr-zzporta_enxerto.
                    <ls_flcma_create>-zzesp_rua = <ls_csdoc>-x-cshdr-zzesp_rua.
                    <ls_flcma_create>-zzesp_pes = <ls_csdoc>-x-cshdr-zzesp_pes.
                    <ls_flcma_create>-zzqtd_plantas = <ls_csdoc>-x-cshdr-zzqtd_plantas.
                    <ls_flcma_create>-zzproc_muda = <ls_csdoc>-x-cshdr-zzproc_muda.
                    <ls_flcma_create>-zzproc_genetica = <ls_csdoc>-x-cshdr-zzproc_genetica.
                    <ls_flcma_create>-zzlarg_copa = <ls_csdoc>-x-cshdr-zzlarg_copa.
                    <ls_flcma_create>-zzaltura_copa = <ls_csdoc>-x-cshdr-zzaltura_copa.
                    <ls_flcma_create>-exhad = lv_begda_manu1.
*-- Nº principal do imobilizado
                    CLEAR <ls_flcma_create>-anlnr.
*-- Centro de custo
                    READ TABLE lt_csks INTO DATA(ls_csks)
                      WITH KEY cmnum = ls_glflcma-cmnum
                               varia = c_crop_season-manutencao
                               iwerk = ls_glflcma-iwerk BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      <ls_flcma_create>-kostl = ls_csks-kostl.
                    ENDIF.
                  ENDIF.
                ENDIF.

                IF lv_create2 EQ abap_true.
                  IF lv_create1 EQ abap_true.
                    lv_begda_manu2 = lv_endda_manu1 + 1.
                  ELSE.
                    lv_begda_manu2 = <ls_csdoc>-x-cshdr-datbi + 1.
                  ENDIF.

                  IF ls_manutencao-offst LE 999.
                    CLEAR ls_duration.
                    ls_duration-duryy = ls_manutencao-offst.
                    CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                      EXPORTING
                        im_date     = lv_begda_manu2
                        im_operator = '+'
                        im_duration = ls_duration
                      IMPORTING
                        ex_date     = lv_endda_manu2.

                    CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                      EXPORTING
                        i_day_in            = lv_endda_manu2
                      IMPORTING
                        e_last_day_of_month = lv_endda_manu2.
*-- Período final da época (DD MM)
                    lv_endda_manu2 = lv_endda_manu2(4) && ls_manutencao-eperd.
                    IF lv_endda_manu2 GT lv_endda_form2.
                      lv_endda_manu2 = lv_endda_form2.
                    ENDIF.
                  ENDIF.

*-- Cria Época de Cultura de Manutenção
*-- Cultura [CITROS]; Variante [MANUT&COLHEITA]
                  INSERT INITIAL LINE INTO TABLE lt_flcma_create
                    ASSIGNING <ls_flcma_create>.
                  IF sy-subrc EQ 0.
                    <ls_flcma_create>-tplnr_fl = <ls_csdoc>-tplnr_fl.
                    <ls_flcma_create>-cmnum = 'CITROS'.
                    <ls_flcma_create>-varia = c_crop_season-manutencao.
                    <ls_flcma_create>-season = 'SAFRA PROD'.
                    <ls_flcma_create>-updkz = c_updkz_new.
                    <ls_flcma_create>-astat = 'I'.
                    <ls_flcma_create>-zzprevisto = abap_true.
                    <ls_flcma_create>-datab = lv_begda_manu2.
                    <ls_flcma_create>-datbi = lv_endda_manu2.
                    <ls_flcma_create>-class = '1'.
                    <ls_flcma_create>-zzdate = <ls_flcma_create>-datbi.
                    <ls_flcma_create>-zzmatnr = <ls_csdoc>-x-cshdr-zzmatnr.
                    <ls_flcma_create>-ymatnr = <ls_csdoc>-x-cshdr-ymatnr.
                    <ls_flcma_create>-zzfazplantio = <ls_csdoc>-x-cshdr-zzfazplantio.
                    <ls_flcma_create>-zzfazvartecnia = <ls_csdoc>-x-cshdr-zzfazvartecnia.
                    <ls_flcma_create>-zzporta_enxerto = <ls_csdoc>-x-cshdr-zzporta_enxerto.
                    <ls_flcma_create>-zzesp_rua = <ls_csdoc>-x-cshdr-zzesp_rua.
                    <ls_flcma_create>-zzesp_pes = <ls_csdoc>-x-cshdr-zzesp_pes.
                    <ls_flcma_create>-zzqtd_plantas = <ls_csdoc>-x-cshdr-zzqtd_plantas.
                    <ls_flcma_create>-zzproc_muda = <ls_csdoc>-x-cshdr-zzproc_muda.
                    <ls_flcma_create>-zzproc_genetica = <ls_csdoc>-x-cshdr-zzproc_genetica.
                    <ls_flcma_create>-zzlarg_copa = <ls_csdoc>-x-cshdr-zzlarg_copa.
                    <ls_flcma_create>-zzaltura_copa = <ls_csdoc>-x-cshdr-zzaltura_copa.
                    <ls_flcma_create>-exhad = lv_begda_manu2.
*-- Nº principal do imobilizado
                    CLEAR <ls_flcma_create>-anlnr.
*-- Centro de custo
                    READ TABLE lt_csks INTO ls_csks
                      WITH KEY cmnum = ls_glflcma-cmnum
                               varia = c_crop_season-manutencao
                               iwerk = ls_glflcma-iwerk BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      <ls_flcma_create>-kostl = ls_csks-kostl.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
*===================================================================*
          ENDIF.
        ENDIF.

        READ TABLE lt_csdoc_temp INTO ls_csdoc_temp
          INDEX lv_tabix COMPARING tplnr_fl.
      ENDWHILE.

      PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                 CHANGING lt_csdoc_change
                                          lt_msg_change
                                          lv_subrc.

      PERFORM flcma_create    USING lv_tplnr_fl
                           CHANGING lt_flcma_create
                                    lt_csdoc_create
                                    lt_msg_create
                                    lv_log_create
                                    lv_subrc.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIABLES_INITIATOR
*&---------------------------------------------------------------------*
FORM display_messages  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-cprog.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR lv_initiator.

ENDFORM.                    " MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*& Form CREATE_PROCESS_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_GLFLCMA_CMNUM
*&      --> LS_GLFLCMA_VARIA
*&      --> P_
*&---------------------------------------------------------------------*
FORM create_process_order USING ls_glflcma  TYPE /agri/glflcma
                                lv_cpros    TYPE /agri/glcpros
                                lv_tplnr_fl TYPE /agri/gltplnr_fl
                                lv_begda    TYPE sydatum
                       CHANGING lt_fpdoc    TYPE /agri/t_fmfp_doc.

  DATA: lt_aufnr     TYPE /agri/t_fmaufnr,
        lt_cskey     TYPE STANDARD TABLE OF /agri/s_glcs_key INITIAL SIZE 0,
        lt_msg_fmfp  TYPE /agri/t_gprolog,
        lv_msgno     TYPE msgno,
        lv_gstrp     TYPE co_gstrp,
        lv_null_date TYPE sydatum,
        lv_msgv1     TYPE msgv1,
        lv_msgv2     TYPE msgv2.

*-- Create process orders for crop process
  READ TABLE gt_cpros INTO DATA(ls_cpros)
    WITH KEY cmnum = ls_glflcma-cmnum
             varia = ls_glflcma-varia
             cpros = lv_cpros.
  IF sy-subrc EQ 0.
*-- Check if any active process order exists for that cpros, variant
    READ TABLE gt_fmfphdr INTO DATA(ls_fmfphdr)
         WITH KEY autyp    = 'AO'
                  tplnr_fl = ls_glflcma-tplnr_fl
                  contr    = ls_glflcma-contr
                  cmnum    = ls_glflcma-cmnum
                  varia    = ls_glflcma-varia
                  cpros    = lv_cpros.
    IF sy-subrc EQ 0
    AND ls_fmfphdr-tecom EQ abap_false.
      CLEAR: lv_msgv1, lv_msgv2.
      lv_msgv1 = lv_tplnr_fl.
      lv_msgv2 = ls_fmfphdr-aufnr.

      IF lv_cpros EQ c_crop_process-formacao.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura FORMAÇÃO!
        lv_msgno = '363'.
      ELSEIF lv_cpros EQ c_crop_process-manutencao.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura MANUTENÇÃO!
        lv_msgno = '372'.
      ELSEIF lv_cpros EQ c_crop_process-colheita.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura COLHEITA!
        lv_msgno = '367'.
      ENDIF.

*-- Talhão &1: Existe ordem p/ processo de cultura FORMAÇÃO/MANUTEÇÃO/COLHEITA!
      PERFORM build_msgs USING 'I' lv_msgno
                               lv_msgv1 lv_msgv2
                      CHANGING gt_message[].

      INSERT INITIAL LINE INTO TABLE lt_aufnr
        ASSIGNING FIELD-SYMBOL(<ls_aufnr>).
      IF sy-subrc EQ 0.
        <ls_aufnr>-aufnr = ls_fmfphdr-aufnr.

        CALL FUNCTION '/AGRI/FMFP_VIEW'
          EXPORTING
            it_aufnr       = lt_aufnr
          IMPORTING
            et_fpdoc       = lt_fpdoc
          EXCEPTIONS
            no_data_exists = 1
            OTHERS         = 2.
      ENDIF.
    ELSE.
      INSERT INITIAL LINE INTO TABLE lt_cskey
        ASSIGNING FIELD-SYMBOL(<ls_cskey>).
      IF sy-subrc EQ 0.
        <ls_cskey>-tplnr_fl = ls_glflcma-tplnr_fl.
        <ls_cskey>-contr    = ls_glflcma-contr.
      ENDIF.

*-- If not active process order found, then create process order
      IF lv_begda NE lv_null_date.
        lv_gstrp = lv_begda.
      ELSE.
        lv_gstrp = sy-datum.
      ENDIF.

      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = abap_true
          i_commit_work         = abap_true
          i_cpros               = ls_cpros-cpros
          i_gstrp               = lv_gstrp
*         i_release             = 'X'
          it_cskey              = lt_cskey[]
        IMPORTING
          et_fpdoc              = lt_fpdoc[]
          et_messages           = lt_msg_fmfp[]
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.

      IF sy-subrc EQ 0.
*-- Build messages
        LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
          CLEAR: lv_msgv1, lv_msgv2.
          lv_msgv1 = lv_tplnr_fl.
          lv_msgv2 = ls_fpdoc-aufnr.

          IF lv_cpros EQ c_crop_process-formacao.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura FORMAÇÃO!
            lv_msgno = '364'.
          ELSEIF lv_cpros EQ c_crop_process-manutencao.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura MANUTENÇÃO!
            lv_msgno = '373'.
          ELSEIF lv_cpros EQ c_crop_process-colheita.
            lv_msgno = '368'.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura COLHEITA!
          ENDIF.

*-- Talhão &1: Ordem &2 criada p/ processo de cultura FORMAÇÃO!
          PERFORM build_msgs USING 'S' lv_msgno
                                   lv_msgv1 lv_msgv2
                          CHANGING gt_message[].
          CLEAR: lv_msgv1.
        ENDLOOP.

        IF ls_fpdoc-aufnr IS NOT INITIAL.
          REFRESH: lt_aufnr, lt_fpdoc.
          INSERT INITIAL LINE INTO TABLE lt_aufnr
            ASSIGNING <ls_aufnr>.
          IF sy-subrc EQ 0.
            <ls_aufnr>-aufnr = ls_fpdoc-aufnr.

            CALL FUNCTION '/AGRI/FMFP_VIEW'
              EXPORTING
                it_aufnr       = lt_aufnr
              IMPORTING
                et_fpdoc       = lt_fpdoc
              EXCEPTIONS
                no_data_exists = 1
                OTHERS         = 2.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lt_fpdoc IS INITIAL.
        APPEND LINES OF lt_msg_fmfp TO gt_message.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_YARD_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FPDOC_X_CSHDR
*&---------------------------------------------------------------------*
FORM create_yard_order CHANGING ls_csdoc TYPE /agri/s_glcs_doc
                                lv_subrc TYPE sysubrc.

  DATA: lt_messtab    TYPE tab_bdcmsgcoll,
        ls_order_comm TYPE /agri/s_glpocomm,
        lv_tplnr_fl   TYPE /agri/gltplnr_fl,
        lv_aufnr      TYPE aufnr.

  IF ls_csdoc-x-cshdr-ymatnr IS INITIAL.
    lv_subrc = 4.
    IF ls_csdoc-x-cshdr-tplnr_fl IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_csdoc-x-cshdr-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.
    ENDIF.

*-- Talhão &1: Verificar mat.agríc. na Époc.Cult. p/ criar Ordem Pátio!
    INSERT INITIAL LINE INTO TABLE gt_message
      ASSIGNING FIELD-SYMBOL(<ls_message>).
    IF sy-subrc EQ 0.
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = '369'.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgv1 = lv_tplnr_fl.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM /agri/glflot
      INTO @DATA(ls_glflot)
     WHERE tplnr_fl EQ @ls_csdoc-x-cshdr-tplnr_fl
       AND loevm    EQ @space.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_glflot TO ls_order_comm.
      MOVE-CORRESPONDING ls_csdoc-x-cshdr TO ls_order_comm.
      ls_order_comm-matnr = ls_csdoc-x-cshdr-ymatnr.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = ls_order_comm-matnr
        IMPORTING
          output       = ls_order_comm-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      ls_order_comm-gstrp = ls_csdoc-x-cshdr-datab.
      ls_order_comm-gltrp = ls_csdoc-x-cshdr-datbi.
      ls_order_comm-gamng = ls_csdoc-x-cshdr-eston.
      ls_order_comm-gmein = ls_csdoc-x-cshdr-esuom.

      CALL METHOD /agri/cl_glco_process=>order_create
        EXPORTING
          i_commit_work = space
          is_order_comm = ls_order_comm
        IMPORTING
          e_aufnr       = lv_aufnr
          et_messages   = lt_messtab.

      IF lv_aufnr IS INITIAL.
        lv_subrc = 4.
        LOOP AT lt_messtab INTO DATA(ls_messtab)
          WHERE msgtyp EQ c_msg_type-error.
          INSERT INITIAL LINE INTO TABLE gt_message
            ASSIGNING <ls_message>.
          IF sy-subrc EQ 0.
            <ls_message>-msgid = ls_messtab-msgid.
            <ls_message>-msgno = ls_messtab-msgnr.
            <ls_message>-msgty = ls_messtab-msgtyp.
            <ls_message>-msgv1 = ls_messtab-msgv1.
            <ls_message>-msgv2 = ls_messtab-msgv2.
            <ls_message>-msgv3 = ls_messtab-msgv3.
            <ls_message>-msgv4 = ls_messtab-msgv4.
          ENDIF.
        ENDLOOP.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
*-- Criação de ordem do pátio falhou
          <ls_message>-msgid = '/AGRI/GLCS'.
          <ls_message>-msgno = '036'.
          <ls_message>-msgty = c_msg_type-error.
        ENDIF.
      ELSE.
*-- Ordem do pátio &1 criada
        ls_csdoc-x-cshdr-yaufnr = lv_aufnr = lv_aufnr.
        ls_csdoc-x-cshdr-updkz = c_updkz_update.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = '/AGRI/GLCS'.
          <ls_message>-msgno = '037'.
          <ls_message>-msgty = c_msg_type-success.
          <ls_message>-msgv1 = lv_aufnr .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ACTIVATE_MANUTENCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM activate_manutencao .

  DATA: lt_flcma_create TYPE /agri/t_glflcma,
        lt_csdoc_create TYPE /agri/t_glcs_doc,
        lt_aufnr        TYPE /agri/t_fmaufnr,
        lt_msg_create   TYPE /agri/t_gprolog,
        lt_cskey_temp   TYPE /agri/t_glcs_key,
        lt_csdoc_temp   TYPE /agri/t_glcs_doc,
        lt_csdoc_form   TYPE /agri/t_glcs_doc,
        lt_csdoc_change TYPE /agri/t_glcs_doc,
        lt_msg_change   TYPE /agri/t_gprolog,
        lt_msg_teco     TYPE /agri/t_gprolog,
        lt_msg_fmfp     TYPE /agri/t_gprolog,
        lt_fpdoc        TYPE /agri/t_fmfp_doc,
        lt_fpdoc_tman   TYPE /agri/t_fmfp_doc,
        lt_fpdoc_tcol   TYPE /agri/t_fmfp_doc,
        lt_orders       TYPE TABLE OF bapi_order_key,
        lt_factdays     TYPE STANDARD TABLE OF rke_dat INITIAL SIZE 0,
        lt_return_teco  TYPE cocf_t_bapi_return,
        lt_cskey        TYPE STANDARD TABLE OF /agri/s_glcs_key INITIAL SIZE 0,
        ls_duration     TYPE psen_duration,
        ls_status       TYPE bapiret2,
        lv_log_create   TYPE balognr,
        lv_tplnr_fl     TYPE /agri/gltplnr_fl,
        lv_aufnr_out    TYPE /agri/fmfpnum,
        lv_null_date    TYPE sydatum,
        lv_first_date   TYPE sydatum,
        lv_last_date    TYPE sydatum,
        lv_begda        TYPE sydatum,
        lv_endda        TYPE sydatum,
        lv_begda_manu   TYPE sydatum,
        lv_endda_manu   TYPE sydatum,
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2,
        lv_subrc        TYPE sysubrc,
        lv_gstrp        TYPE co_gstrp,
        lv_manutencao   TYPE i,
        lv_monday       TYPE sydatum,
        lv_sunday       TYPE sydatum.

  FIELD-SYMBOLS: <ls_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas de  Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    RETURN.
  ENDIF.

  SELECT *
    FROM /agri/glseason
    INTO TABLE @DATA(lt_season).

  IF gt_glflcma[] IS NOT INITIAL.
    SELECT *
      FROM zabs_csks
      INTO TABLE @DATA(lt_csks)
      FOR ALL ENTRIES IN @gt_glflcma
     WHERE cmnum = @gt_glflcma-cmnum
       AND iwerk = @gt_glflcma-iwerk.

    SORT lt_csks BY cmnum varia iwerk.
  ENDIF.

  SORT gt_glflcma BY tplnr_fl contr.

*-- Época de Cultura FORMAÇÂO =>>
  DATA(lt_glflcma_form) = gt_glflcma[].
  DELETE lt_glflcma_form WHERE varia NE c_crop_season-formacao.
  SORT lt_glflcma_form BY tplnr_fl contr.
  lt_cskey_temp = CORRESPONDING #( lt_glflcma_form ).

  IF lt_cskey_temp[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLCS_VIEW'
      EXPORTING
        it_cskey       = lt_cskey_temp
      IMPORTING
        et_csdoc       = lt_csdoc_form
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    SORT lt_csdoc_form BY tplnr_fl contr.
  ENDIF.
*-- <<= Época de Cultura FORMAÇÂO

  REFRESH lt_cskey_temp.
  lt_cskey_temp = CORRESPONDING #( gt_glflcma ).

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      it_cskey       = lt_cskey_temp
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    IF sy-msgid IS NOT INITIAL
    AND sy-msgty IS NOT INITIAL
    AND sy-msgno IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.

      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        <ls_message>-msgid = sy-msgid.
        <ls_message>-msgno = sy-msgno.
        <ls_message>-msgty = sy-msgty.
        <ls_message>-msgv1 = sy-msgv1.
        <ls_message>-msgv2 = sy-msgv2.
        <ls_message>-msgv3 = sy-msgv3.
        <ls_message>-msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
  ELSE.
    SORT lt_csdoc_temp BY tplnr_fl contr.
    DATA(lt_terrain_distinct) = lt_csdoc_temp[].
    DELETE ADJACENT DUPLICATES FROM lt_terrain_distinct COMPARING tplnr_fl.

*-- Leitura Talhões
    LOOP AT lt_terrain_distinct INTO DATA(ls_terrain_distinct).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_terrain_distinct-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.
*-- Talhão: &1
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '357'.
        <ls_message>-msgty = 'I'.
        <ls_message>-msgv1 = lv_tplnr_fl.
      ENDIF.

      REFRESH: lt_csdoc_change, lt_flcma_create,
               lt_msg_change, lt_msg_create,
               lt_aufnr, lt_msg_teco.
      CLEAR: lv_manutencao.
      READ TABLE lt_csdoc_temp INTO DATA(ls_csdoc_temp)
        WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        DATA(lv_contr) = ls_csdoc_temp-contr.

        UNASSIGN <ls_csdoc>.
        ASSIGN ls_csdoc_temp TO <ls_csdoc>.
        IF <ls_csdoc> IS ASSIGNED.
          READ TABLE gt_glflcma INTO DATA(ls_glflcma)
            WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                     contr    = <ls_csdoc>-contr BINARY SEARCH.
          IF sy-subrc EQ 0.
*===================================================================*
*-- Época de Cultura/Variante: [MANUT&COLHEITA]
            IF <ls_csdoc>-x-cshdr-varia EQ c_crop_season-manutencao.
              REFRESH: lt_fpdoc_tman, lt_fpdoc_tcol, lt_factdays.
              CLEAR: lv_first_date, lv_begda, lv_endda, lv_begda_manu,
                     lv_endda_manu, lv_monday, lv_sunday.

              ADD 1 TO lv_manutencao.

*-------------------------------------------------------------------*
* Verificar primeiro domingo da semana de início da MANUTENÇÃO
*-------------------------------------------------------------------*
              IF <ls_csdoc>-x-cshdr-datab IS NOT INITIAL.
                CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
                  EXPORTING
                    date   = <ls_csdoc>-x-cshdr-datab
                  IMPORTING
                    monday = lv_monday
                    sunday = lv_sunday.

                IF lv_monday LT <ls_csdoc>-x-cshdr-datab.
                  lv_monday = lv_monday + 7.
                ENDIF.

                IF lv_sunday LT <ls_csdoc>-x-cshdr-datab.
                  lv_sunday = lv_sunday + 7.
                ENDIF.
              ENDIF.

*-------------------------------------------------------------------*
* No 3o. dia útil, fechar a safra de FORMAÇÃO e encerrar ordens
*-------------------------------------------------------------------*
              lv_begda = lv_endda = <ls_csdoc>-x-cshdr-datab.
              ADD 10 TO lv_endda.

              CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
                EXPORTING
                  i_datab               = lv_begda
                  i_datbi               = lv_endda
                  i_factid              = gv_factid
                TABLES
                  eth_dats              = lt_factdays
                EXCEPTIONS
                  date_conversion_error = 1
                  OTHERS                = 2.

              IF sy-subrc <> 0.
*-- Erro ao ler calendário fábrica!
                MESSAGE i362(zfmfp).
                LEAVE LIST-PROCESSING.
              ELSE.
                LOOP AT lt_factdays INTO DATA(ls_factday).
                  IF sy-tabix EQ 3.
                    lv_endda = ls_factday.
                  ENDIF.
                ENDLOOP.
              ENDIF.

*-- Verifica data mais próxima (1o.domingo ou 3o.dia útil)
              IF lv_sunday NE lv_null_date
              AND lv_endda NE lv_null_date.
                IF lv_sunday LT lv_endda.
                  lv_first_date = lv_sunday.
                  lv_last_date = lv_endda.
                ELSE.
                  lv_first_date = lv_endda.
                  lv_last_date = lv_sunday.
                ENDIF.
              ENDIF.

              IF lv_manutencao EQ 1.
                DATA(ls_csdoc_manu1) = ls_csdoc_temp.
                DATA(ls_glflcma_manu1) = ls_glflcma.

*-- 1o. Domingo: Encerra tecnicamente Ordem de Produção COLHEITA e Ordem Pátio
*                IF p_date EQ lv_sunday.
                IF p_date EQ lv_endda.
*-------------------------------------------------------------------*
                  READ TABLE lt_csdoc_form INTO DATA(ls_csdoc_form)
                    WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
                  IF sy-subrc EQ 0.
*-- Encerra Ordens de Processo COLHEITA da Época de Cultura FORMAÇÃO
                    READ TABLE lt_glflcma_form INTO DATA(ls_glflcma_form)
                      WITH KEY tplnr_fl = ls_csdoc_form-tplnr_fl
                               contr    = ls_csdoc_form-contr BINARY SEARCH.
                    IF sy-subrc EQ 0.
*-------------------------------------------------------------------*
*-- Encerra ordens [FORMAÇÃO] [COLHEITA]
                      PERFORM mass_teco USING c_crop_season-formacao
                                              c_crop_process-colheita
                                              ls_glflcma_form.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio [FORMAÇÃO]
                      PERFORM yard_tech_complete USING ls_glflcma_form-yaufnr.
*-------------------------------------------------------------------*
                    ENDIF.
                  ENDIF.
                ENDIF.

*-- Data Tela Seleção = 3o dia útil após início Varia [MANUT&COLHEITA]?
                IF p_date EQ lv_endda.
*-------------------------------------------------------------------*
* Technical Complete
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: FORMAÇÃO e IMPLANT
*-------------------------------------------------------------------*
                  READ TABLE lt_csdoc_form INTO ls_csdoc_form
                    WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    READ TABLE lt_glflcma_form INTO ls_glflcma_form
                      WITH KEY tplnr_fl = ls_csdoc_form-tplnr_fl
                               contr    = ls_csdoc_form-contr BINARY SEARCH.
                    IF sy-subrc EQ 0.
*-- Encerra ordens [FORMAÇÃO] [FORMAÇÃO]
                      PERFORM mass_teco USING c_crop_season-formacao
                                              c_crop_process-formacao
                                              ls_glflcma_form.
*-- Encerra ordens [FORMAÇÃO] [IMPLANT]
                      PERFORM mass_teco USING c_crop_season-formacao
                                              c_crop_process-implantacao
                                              ls_glflcma_form.
                    ENDIF.
                  ENDIF.
                ENDIF.

*                IF lv_last_date NE lv_null_date
*                AND lv_last_date EQ p_date.
                IF lv_endda EQ p_date.
*-- Fecha Época de Cultura de FORMAÇÃO
                  READ TABLE lt_csdoc_form INTO ls_csdoc_form
                    WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    ls_csdoc_form-x-cshdr-updkz = 'U'.
                    ls_csdoc_form-x-cshdr-astat = 'C'.
                    APPEND ls_csdoc_form TO lt_csdoc_change.
*-------------------------------------------------------------------*
                    PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                               CHANGING lt_csdoc_change
                                                        lt_msg_change
                                                        lv_subrc.
*-------------------------------------------------------------------*
                  ENDIF.
                ENDIF.

*-- Dt.Início Varia [MANUT&COLHEITA] = Data Tela Seleção?
*                IF <ls_csdoc>-x-cshdr-datab EQ p_date.
                IF lv_endda EQ p_date.
                  <ls_csdoc>-x-cshdr-updkz = 'U'.
                  CLEAR <ls_csdoc>-x-cshdr-zzprevisto.
                  <ls_csdoc>-x-cshdr-astat = 'A'.
                  APPEND <ls_csdoc> TO lt_csdoc_change.
*-------------------------------------------------------------------*
                  PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                             CHANGING lt_csdoc_change
                                                      lt_msg_change
                                                      lv_subrc.
*-------------------------------------------------------------------*
* Create new Process Order for FORMAÇÃO
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                  PERFORM create_process_order USING ls_glflcma
                                                     c_crop_process-manutencao
                                                     lv_tplnr_fl
                                                     <ls_csdoc>-x-cshdr-datab
                                            CHANGING lt_fpdoc_tman.
*-------------------------------------------------------------------*
* Create new Task Order for TMANGEN/TINSP0001
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                  READ TABLE lt_fpdoc_tman INTO DATA(ls_fpdoc_tman) INDEX 1.
                  IF sy-subrc EQ 0.
                    PERFORM document_infocus_lock USING ls_fpdoc_tman-aufnr
                                               CHANGING lv_subrc.
                    IF lv_subrc EQ 0.
                      PERFORM create_task_order USING lv_tplnr_fl
                                                      'TMANGEN'
                                             CHANGING lt_fpdoc_tman.

                      PERFORM create_task_order USING lv_tplnr_fl
                                                      'TINSP0001'
                                             CHANGING lt_fpdoc_tman.

                      PERFORM document_infocus_unlock USING ls_fpdoc_tman-aufnr
                                                            ls_fpdoc_tman-x-fphdr-rsnum.
                    ENDIF.
                  ENDIF.
*-------------------------------------------------------------------*
* Create new Process Order for COLHEITA
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: COLHEITA
*-------------------------------------------------------------------*
                  PERFORM create_process_order USING ls_glflcma
                                                     c_crop_process-colheita
                                                     lv_tplnr_fl
                                                     <ls_csdoc>-x-cshdr-datab
                                            CHANGING lt_fpdoc_tcol.
*-------------------------------------------------------------------*
* Create Yard Order
*-------------------------------------------------------------------*
*/AGRI/SAPLGLCSM -> FCODE_MYCRE -> PERFORM create_yard_order CHANGING lv_subrc.
                  IF <ls_csdoc>-x-cshdr-yaufnr IS INITIAL.
                    CLEAR lv_subrc.
                    PERFORM create_yard_order CHANGING <ls_csdoc>
                                                       lv_subrc.
                    IF lv_subrc EQ 0
                    AND <ls_csdoc>-x-cshdr-yaufnr IS NOT INITIAL.
                      IF lt_csdoc_change[] IS INITIAL.
                        <ls_csdoc>-x-cshdr-updkz = c_updkz_update.
                        APPEND <ls_csdoc> TO lt_csdoc_change.
                      ELSE.
                        READ TABLE lt_csdoc_change
                          ASSIGNING FIELD-SYMBOL(<ls_csdoc_change>) INDEX 1.
                        IF sy-subrc EQ 0.
                          <ls_csdoc_change> = <ls_csdoc>.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ELSE.
*-- Talhão &1: Ordem Pátio &2 existe para Época de Cultura 'MANUT&COLHEITA'.
                    INSERT INITIAL LINE INTO TABLE gt_message
                      ASSIGNING <ls_message>.
                    IF sy-subrc EQ 0.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = <ls_csdoc>-x-cshdr-yaufnr
                        IMPORTING
                          output = lv_aufnr_out.

                      <ls_message>-msgid = 'ZFMFP'.
                      <ls_message>-msgno = '374'.
                      <ls_message>-msgty = 'S'.
                      <ls_message>-msgv1 = lv_tplnr_fl.
                      <ls_message>-msgv2 = lv_aufnr_out.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSEIF lv_manutencao EQ 2.
*-- 1o. Domingo: Encerra tecnicamente Ordem de Produção COLHEITA e Ordem Pátio
                IF p_date EQ lv_sunday.
*-------------------------------------------------------------------*
*-- Encerra ordens [MANUT&COLHEITA] [COLHEITA]
                  PERFORM mass_teco USING c_crop_season-manutencao
                                          c_crop_process-colheita
                                          ls_glflcma_manu1.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio [MANUT&COLHEITA]
                  PERFORM yard_tech_complete USING ls_glflcma_manu1-yaufnr.
*-------------------------------------------------------------------*
                ENDIF.

*-- Data Tela Seleção = 3o dia útil após início Varia [MANUT&COLHEITA]?
                IF p_date EQ lv_endda.
*-------------------------------------------------------------------*
* Technical Complete
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
*-- Encerra ordens [MANUT&COLHEITA] [MANUTEÇÃO]
                  PERFORM mass_teco USING c_crop_season-manutencao
                                          c_crop_process-manutencao
                                          ls_glflcma_manu1.
                ENDIF.

                IF lv_last_date NE lv_null_date
                AND lv_last_date EQ p_date.
*-- Fecha Época de Cultura de MANUTENÇÃO anterior
                  IF ls_csdoc_manu1 IS NOT INITIAL.
                    ls_csdoc_manu1-x-cshdr-updkz = 'U'.
                    ls_csdoc_manu1-x-cshdr-astat = 'C'.
                    APPEND ls_csdoc_manu1 TO lt_csdoc_change.
*-------------------------------------------------------------------*
                    PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                               CHANGING lt_csdoc_change
                                                        lt_msg_change
                                                        lv_subrc.
*-------------------------------------------------------------------*
                  ENDIF.
                ENDIF.

*-- Dt.Início Varia [MANUT&COLHEITA] = Data Tela Seleção?
                IF <ls_csdoc>-x-cshdr-datab EQ p_date.
                  <ls_csdoc>-x-cshdr-updkz = 'U'.
                  CLEAR <ls_csdoc>-x-cshdr-zzprevisto.
                  <ls_csdoc>-x-cshdr-astat = 'A'.
                  APPEND <ls_csdoc> TO lt_csdoc_change.
*-------------------------------------------------------------------*
                  PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                             CHANGING lt_csdoc_change
                                                      lt_msg_change
                                                      lv_subrc.
*-------------------------------------------------------------------*
* Create new Process Order for FORMAÇÃO
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                  PERFORM create_process_order USING ls_glflcma
                                                     c_crop_process-manutencao
                                                     lv_tplnr_fl
                                                     <ls_csdoc>-x-cshdr-datab
                                            CHANGING lt_fpdoc_tman.
*-------------------------------------------------------------------*
* Create new Task Order for TMANGEN/TINSP0001
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                  READ TABLE lt_fpdoc_tman INTO ls_fpdoc_tman INDEX 1.
                  IF sy-subrc EQ 0.
                    PERFORM document_infocus_lock USING ls_fpdoc_tman-aufnr
                                               CHANGING lv_subrc.
                    IF lv_subrc EQ 0.
                      PERFORM create_task_order USING lv_tplnr_fl
                                                      'TMANGEN'
                                             CHANGING lt_fpdoc_tman.


                      PERFORM create_task_order USING lv_tplnr_fl
                                                      'TINSP0001'
                                             CHANGING lt_fpdoc_tman.

                      PERFORM document_infocus_unlock USING ls_fpdoc_tman-aufnr
                                                            ls_fpdoc_tman-x-fphdr-rsnum.
                    ENDIF.
                  ENDIF.
*-------------------------------------------------------------------*
* Create new Process Order for COLHEITA
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: COLHEITA
*-------------------------------------------------------------------*
                  PERFORM create_process_order USING ls_glflcma
                                                     c_crop_process-colheita
                                                     lv_tplnr_fl
                                                     <ls_csdoc>-x-cshdr-datab
                                            CHANGING lt_fpdoc_tcol.
*-------------------------------------------------------------------*
* Create Yard Order
*-------------------------------------------------------------------*
*/AGRI/SAPLGLCSM -> FCODE_MYCRE -> PERFORM create_yard_order CHANGING lv_subrc.
                  IF <ls_csdoc>-x-cshdr-yaufnr IS INITIAL.
                    CLEAR lv_subrc.
                    PERFORM create_yard_order CHANGING <ls_csdoc>
                                                       lv_subrc.
                    IF lv_subrc EQ 0
                    AND <ls_csdoc>-x-cshdr-yaufnr IS NOT INITIAL.
                      IF lt_csdoc_change[] IS INITIAL.
                        <ls_csdoc>-x-cshdr-updkz = c_updkz_update.
                        APPEND <ls_csdoc> TO lt_csdoc_change.
                      ELSE.
                        READ TABLE lt_csdoc_change
                          ASSIGNING <ls_csdoc_change> INDEX 1.
                        IF sy-subrc EQ 0.
                          <ls_csdoc_change> = <ls_csdoc>.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ELSE.
*-- Talhão &1: Ordem Pátio &2 existe para Época de Cultura 'MANUT&COLHEITA'.
                    INSERT INITIAL LINE INTO TABLE gt_message
                      ASSIGNING <ls_message>.
                    IF sy-subrc EQ 0.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = <ls_csdoc>-x-cshdr-yaufnr
                        IMPORTING
                          output = lv_aufnr_out.

                      <ls_message>-msgid = 'ZFMFP'.
                      <ls_message>-msgno = '374'.
                      <ls_message>-msgty = 'S'.
                      <ls_message>-msgv1 = lv_tplnr_fl.
                      <ls_message>-msgv2 = lv_aufnr_out.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
*===================================================================*
          ENDIF.
        ENDIF.

        READ TABLE lt_csdoc_temp INTO ls_csdoc_temp
          INDEX lv_tabix COMPARING tplnr_fl.
      ENDWHILE.

      PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                 CHANGING lt_csdoc_change
                                          lt_msg_change
                                          lv_subrc.

      PERFORM flcma_create    USING lv_tplnr_fl
                           CHANGING lt_flcma_create
                                    lt_csdoc_create
                                    lt_msg_create
                                    lv_log_create
                                    lv_subrc.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ENHANCE_MANUTENCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM enhance_manutencao .

  DATA: lt_flcma_create TYPE /agri/t_glflcma,
        lt_csdoc_create TYPE /agri/t_glcs_doc,
        lt_aufnr        TYPE /agri/t_fmaufnr,
        lt_msg_create   TYPE /agri/t_gprolog,
        lt_cskey_temp   TYPE /agri/t_glcs_key,
        lt_csdoc_temp   TYPE /agri/t_glcs_doc,
        lt_csdoc_form   TYPE /agri/t_glcs_doc,
        lt_csdoc_change TYPE /agri/t_glcs_doc,
        lt_msg_change   TYPE /agri/t_gprolog,
        lt_msg_teco     TYPE /agri/t_gprolog,
        lt_msg_fmfp     TYPE /agri/t_gprolog,
        lt_fpdoc        TYPE /agri/t_fmfp_doc,
        lt_fpdoc_tman   TYPE /agri/t_fmfp_doc,
        lt_fpdoc_tcol   TYPE /agri/t_fmfp_doc,
        lt_orders       TYPE TABLE OF bapi_order_key,
        lt_factdays     TYPE STANDARD TABLE OF rke_dat INITIAL SIZE 0,
        lt_return_teco  TYPE cocf_t_bapi_return,
        lt_cskey        TYPE STANDARD TABLE OF /agri/s_glcs_key INITIAL SIZE 0,
        ls_duration     TYPE psen_duration,
        ls_status       TYPE bapiret2,
        lv_log_create   TYPE balognr,
        lv_tplnr_fl     TYPE /agri/gltplnr_fl,
        lv_aufnr_out    TYPE /agri/fmfpnum,
        lv_null_date    TYPE sydatum,
        lv_first_date   TYPE sydatum,
        lv_last_date    TYPE sydatum,
        lv_begda        TYPE sydatum,
        lv_endda        TYPE sydatum,
        lv_begda_manu   TYPE sydatum,
        lv_endda_manu   TYPE sydatum,
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2,
        lv_subrc        TYPE sysubrc,
        lv_gstrp        TYPE co_gstrp,
        lv_manutencao   TYPE i,
        lv_monday       TYPE sydatum,
        lv_sunday       TYPE sydatum.

  FIELD-SYMBOLS: <ls_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  IF gt_glflcma[] IS INITIAL.
*-- Não existem Épocas de  Cultura com status <> 'C' (Fechado)!
    MESSAGE i356(zfmfp).
    RETURN.
  ENDIF.

  SELECT *
    FROM /agri/glseason
    INTO TABLE @DATA(lt_season).

  IF gt_glflcma[] IS NOT INITIAL.
    SELECT *
      FROM zabs_csks
      INTO TABLE @DATA(lt_csks)
      FOR ALL ENTRIES IN @gt_glflcma
     WHERE cmnum = @gt_glflcma-cmnum
       AND iwerk = @gt_glflcma-iwerk.

    SORT lt_csks BY cmnum varia iwerk.
  ENDIF.

  SORT gt_glflcma BY tplnr_fl contr.

*-- Época de Cultura FORMAÇÂO =>>
  DATA(lt_glflcma_form) = gt_glflcma[].
  DELETE lt_glflcma_form WHERE varia NE c_crop_season-formacao.
  SORT lt_glflcma_form BY tplnr_fl contr.
  lt_cskey_temp = CORRESPONDING #( lt_glflcma_form ).

  IF lt_cskey_temp[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLCS_VIEW'
      EXPORTING
        it_cskey       = lt_cskey_temp
      IMPORTING
        et_csdoc       = lt_csdoc_form
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    SORT lt_csdoc_form BY tplnr_fl contr.
  ENDIF.
*-- <<= Época de Cultura FORMAÇÂO

  REFRESH lt_cskey_temp.
  lt_cskey_temp = CORRESPONDING #( gt_glflcma ).

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      it_cskey       = lt_cskey_temp
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    IF sy-msgid IS NOT INITIAL
    AND sy-msgty IS NOT INITIAL
    AND sy-msgno IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.

      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        <ls_message>-msgid = sy-msgid.
        <ls_message>-msgno = sy-msgno.
        <ls_message>-msgty = sy-msgty.
        <ls_message>-msgv1 = sy-msgv1.
        <ls_message>-msgv2 = sy-msgv2.
        <ls_message>-msgv3 = sy-msgv3.
        <ls_message>-msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
  ELSE.
    SORT lt_csdoc_temp BY tplnr_fl contr.
    DATA(lt_terrain_distinct) = lt_csdoc_temp[].
    DELETE ADJACENT DUPLICATES FROM lt_terrain_distinct COMPARING tplnr_fl.

*-- Leitura Talhões
    LOOP AT lt_terrain_distinct INTO DATA(ls_terrain_distinct).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_terrain_distinct-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.
*-- Talhão: &1
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '357'.
        <ls_message>-msgty = 'I'.
        <ls_message>-msgv1 = lv_tplnr_fl.
      ENDIF.

      REFRESH: lt_csdoc_change, lt_flcma_create,
               lt_msg_change, lt_msg_create,
               lt_aufnr, lt_msg_teco.
      CLEAR: lv_manutencao.
      READ TABLE lt_csdoc_temp INTO DATA(ls_csdoc_temp)
        WITH KEY tplnr_fl = ls_terrain_distinct-tplnr_fl BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        DATA(lv_contr) = ls_csdoc_temp-contr.

        UNASSIGN <ls_csdoc>.
        ASSIGN ls_csdoc_temp TO <ls_csdoc>.
        IF <ls_csdoc> IS ASSIGNED.
          READ TABLE gt_glflcma INTO DATA(ls_glflcma)
            WITH KEY tplnr_fl = <ls_csdoc>-tplnr_fl
                     contr    = <ls_csdoc>-contr BINARY SEARCH.
          IF sy-subrc EQ 0.
*===================================================================*
*-- Época de Cultura/Variante: [MANUT&COLHEITA]
            IF <ls_csdoc>-x-cshdr-varia EQ c_crop_season-manutencao.
              REFRESH: lt_fpdoc_tman, lt_fpdoc_tcol, lt_factdays.
              CLEAR: lv_first_date, lv_begda, lv_endda, lv_begda_manu,
                     lv_endda_manu, lv_monday, lv_sunday.

              ADD 1 TO lv_manutencao.

*-------------------------------------------------------------------*
* Verificar primeiro domingo da semana de início da FORMAÇÃO
*-------------------------------------------------------------------*
              IF <ls_csdoc>-x-cshdr-datab IS NOT INITIAL.
                CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
                  EXPORTING
                    date   = <ls_csdoc>-x-cshdr-datab
                  IMPORTING
                    monday = lv_monday
                    sunday = lv_sunday.

                IF lv_monday LT <ls_csdoc>-x-cshdr-datab.
                  lv_monday = lv_monday + 7.
                ENDIF.

                IF lv_sunday LT <ls_csdoc>-x-cshdr-datab.
                  lv_sunday = lv_sunday + 7.
                ENDIF.
              ENDIF.

*-------------------------------------------------------------------*
* No 3o. dia útil, fechar a safra de FORMAÇÃO e encerrar ordens
*-------------------------------------------------------------------*
              lv_begda = lv_endda = <ls_csdoc>-x-cshdr-datab.
              ADD 10 TO lv_endda.

              CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
                EXPORTING
                  i_datab               = lv_begda
                  i_datbi               = lv_endda
                  i_factid              = gv_factid
                TABLES
                  eth_dats              = lt_factdays
                EXCEPTIONS
                  date_conversion_error = 1
                  OTHERS                = 2.

              IF sy-subrc <> 0.
*-- Erro ao ler calendário fábrica!
                MESSAGE i362(zfmfp).
                LEAVE LIST-PROCESSING.
              ELSE.
                LOOP AT lt_factdays INTO DATA(ls_factday).
                  IF sy-tabix EQ 3.
                    lv_endda = ls_factday.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDIF.

*-- Verifica data mais próxima (1o.domingo ou 3o.dia útil)
              IF lv_sunday NE lv_null_date
              AND lv_endda NE lv_null_date.
                IF lv_sunday LT lv_endda.
                  lv_first_date = lv_sunday.
                  lv_last_date = lv_endda.
                ELSE.
                  lv_first_date = lv_endda.
                  lv_last_date = lv_sunday.
                ENDIF.
              ENDIF.

              IF lv_manutencao EQ 1.
                DATA(ls_csdoc_manu1) = ls_csdoc_temp.
                DATA(ls_glflcma_manu1) = ls_glflcma.
              ENDIF.

*===================================================================*
* 3: 1o. Domingo Encerra tecn. Ordem de Prod. COLHEITA e Ordem Pátio
* da Época de Cultura MANUT&COLHEITA anterior
*===================================================================*
              IF p_date EQ lv_sunday.
                IF ls_glflcma_manu1 IS NOT INITIAL
                AND lv_manutencao EQ 2.
*-------------------------------------------------------------------*
*-- Encerra ordens [MANUT&COLHEITA] [COLHEITA]
                  PERFORM mass_teco USING c_crop_season-manutencao
                                          c_crop_process-colheita
                                          ls_glflcma_manu1.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio da Época de Cultura FORMAÇÃO
                  PERFORM yard_tech_complete USING ls_glflcma_manu1-yaufnr.
*-------------------------------------------------------------------*
                ENDIF.
              ENDIF.

*===================================================================*
* 4: Data Tela Seleção = 3o dia útil após início Varia [MANUT&COLHEITA]?
*===================================================================*
              IF p_date EQ lv_endda.
*-------------------------------------------------------------------*
* Technical Complete
* CMNUM: CITROS / VARIA: [MANUT&COLHEITA] / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                IF ls_glflcma_manu1 IS NOT INITIAL
                AND lv_manutencao EQ 2.
*-- Encerrar ordens [MANUT&COLHEITA] [MANUTEÇÃO]
                  PERFORM mass_teco USING c_crop_season-manutencao
                                          c_crop_process-manutencao
                                          ls_glflcma_manu1.
                ENDIF.
              ENDIF.

              IF lv_last_date NE lv_null_date
              AND lv_last_date EQ p_date.
*-- Fecha Época de Cultura de MANUTENÇÃO anterior
                IF ls_csdoc_manu1 IS NOT INITIAL
                AND lv_manutencao EQ 2.
*-- Verificação Adicional: todas as ordens devem estar encerradas
                  PERFORM mass_teco USING c_crop_season-manutencao
                                          c_crop_process-close_all
                                          ls_glflcma_manu1.
                  ls_csdoc_manu1-x-cshdr-updkz = 'U'.
                  ls_csdoc_manu1-x-cshdr-astat = 'C'.
                  APPEND ls_csdoc_manu1 TO lt_csdoc_change.
*-------------------------------------------------------------------*
                  PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                             CHANGING lt_csdoc_change
                                                      lt_msg_change
                                                      lv_subrc.
*-------------------------------------------------------------------*
                ENDIF.
              ENDIF.

*===================================================================*
* 1: Data Tela Seleção está no período [MANUT&COLHEITA]?
*===================================================================*
              IF p_date+4(4) EQ '0201'
              AND p_date BETWEEN <ls_csdoc>-x-cshdr-datab
                            AND <ls_csdoc>-x-cshdr-datbi
              AND <ls_csdoc>-x-cshdr-astat EQ 'A'.
                DATA(lv_enhance) = abap_false.
                IF ( ( <ls_csdoc>-x-cshdr-zzprev_errad NE lv_null_date AND
                       NOT <ls_csdoc>-x-cshdr-zzprev_errad
                       BETWEEN <ls_csdoc>-x-cshdr-datab
                           AND <ls_csdoc>-x-cshdr-datbi )
                OR ( <ls_csdoc>-x-cshdr-zzprev_errad EQ lv_null_date ) ).
                  lv_enhance = abap_true.
                ENDIF.
*                IF <ls_csdoc>-x-cshdr-zzprev_errad NE lv_null_date
*                AND lv_manutencao EQ 1.
                IF lv_enhance = abap_true.
*                  DATA(lv_date_low) = <ls_csdoc>-x-cshdr-zzprev_errad.
                  DATA(lv_date_low) = p_date.
                  lv_date_low = lv_date_low(4) && '0201'.
*                  DATA(lv_date_high) = <ls_csdoc>-x-cshdr-zzprev_errad.
                  DATA(lv_date_high) = p_date.
                  lv_date_high = lv_date_high(4) && '0630'.
*-- Amplia [MANUT&COLHEITA] para [MANUT&COLHEITA]
*-- ASTAT = I (Inativo) e ZZPREVISTO = 'X'
*                  IF <ls_csdoc>-x-cshdr-zzprev_errad
*                  BETWEEN lv_date_low AND lv_date_high.
                  CLEAR: lv_begda_manu, lv_endda_manu.
*-- SAFRA PROD [MANUTENÇÃO]
                  READ TABLE lt_season INTO DATA(ls_manutencao)
                    WITH KEY season = 'SAFRA PROD'.
                  IF sy-subrc EQ 0.
                    IF ls_manutencao-offst LE 999.
                      lv_begda_manu = <ls_csdoc>-x-cshdr-datbi + 1.
                      CLEAR ls_duration.
                      ls_duration-duryy = ls_manutencao-offst.

                      CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                        EXPORTING
                          im_date     = lv_begda_manu
                          im_operator = '+'
                          im_duration = ls_duration
                        IMPORTING
                          ex_date     = lv_endda_manu.

                      CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                        EXPORTING
                          i_day_in            = lv_endda_manu
                        IMPORTING
                          e_last_day_of_month = lv_endda_manu.

*-- Período final da época (DD MM)
                      lv_endda_manu = lv_endda_manu(4) && ls_manutencao-eperd.

*-- Cria Época de Cultura de Manutenção
*-- Cultura [CITROS]; Variante [MANUT&COLHEITA]
                      INSERT INITIAL LINE INTO TABLE lt_flcma_create
                        ASSIGNING FIELD-SYMBOL(<ls_flcma_create>).
                      IF sy-subrc EQ 0.
                        <ls_flcma_create>-tplnr_fl = <ls_csdoc>-tplnr_fl.
                        <ls_flcma_create>-cmnum = 'CITROS'.
                        <ls_flcma_create>-varia = c_crop_season-manutencao.
                        <ls_flcma_create>-season = 'SAFRA PROD'.
                        <ls_flcma_create>-updkz = c_updkz_new.
                        <ls_flcma_create>-astat = 'I'.
                        <ls_flcma_create>-zzprevisto = abap_true.
                        <ls_flcma_create>-datab = lv_begda_manu.
                        <ls_flcma_create>-datbi = lv_endda_manu.
                        <ls_flcma_create>-class = '1'.
                        <ls_flcma_create>-zzdate = <ls_flcma_create>-datbi.
                        <ls_flcma_create>-zzmatnr = <ls_csdoc>-x-cshdr-zzmatnr.
                        <ls_flcma_create>-ymatnr = <ls_csdoc>-x-cshdr-ymatnr.
                        <ls_flcma_create>-zzfazplantio = <ls_csdoc>-x-cshdr-zzfazplantio.
                        <ls_flcma_create>-zzfazvartecnia = <ls_csdoc>-x-cshdr-zzfazvartecnia.
                        <ls_flcma_create>-zzporta_enxerto = <ls_csdoc>-x-cshdr-zzporta_enxerto.
                        <ls_flcma_create>-zzesp_rua = <ls_csdoc>-x-cshdr-zzesp_rua.
                        <ls_flcma_create>-zzesp_pes = <ls_csdoc>-x-cshdr-zzesp_pes.
                        <ls_flcma_create>-zzqtd_plantas = <ls_csdoc>-x-cshdr-zzqtd_plantas.
                        <ls_flcma_create>-zzproc_muda = <ls_csdoc>-x-cshdr-zzproc_muda.
                        <ls_flcma_create>-zzproc_genetica = <ls_csdoc>-x-cshdr-zzproc_genetica.
                        <ls_flcma_create>-zzlarg_copa = <ls_csdoc>-x-cshdr-zzlarg_copa.
                        <ls_flcma_create>-zzaltura_copa = <ls_csdoc>-x-cshdr-zzaltura_copa.
                        <ls_flcma_create>-datab_ref = <ls_csdoc>-x-cshdr-datab_ref.
                        <ls_flcma_create>-exhad = lv_begda_manu.
*-- Nº principal do imobilizado
                        CLEAR <ls_flcma_create>-anlnr.
*-- Centro de custo
                        READ TABLE lt_csks INTO DATA(ls_csks)
                          WITH KEY cmnum = ls_glflcma-cmnum
                                   varia = c_crop_season-manutencao
                                   iwerk = ls_glflcma-iwerk BINARY SEARCH.
                        IF sy-subrc EQ 0.
                          <ls_flcma_create>-kostl = ls_csks-kostl.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
*                  ENDIF.
                ENDIF.
              ENDIF.
*===================================================================*
* 2: Data Tela Seleção = Data Inicial [MANUT&COLHEITA]?
*===================================================================*
              IF p_date EQ <ls_csdoc>-x-cshdr-datab.
                IF <ls_csdoc>-x-cshdr-zzprevisto EQ abap_true
                OR <ls_csdoc>-x-cshdr-astat EQ 'I'.
                  IF <ls_csdoc>-x-cshdr-zzprev_errad NE lv_null_date.
                    IF NOT ( <ls_csdoc>-x-cshdr-zzprev_errad
                    BETWEEN <ls_csdoc>-x-cshdr-datab
                        AND <ls_csdoc>-x-cshdr-datbi ).
                      CLEAR <ls_csdoc>-x-cshdr-zzprevisto.
                    ENDIF.
                  ELSEIF <ls_csdoc>-x-cshdr-zzprev_errad EQ lv_null_date.
                    CLEAR <ls_csdoc>-x-cshdr-zzprevisto.
                  ENDIF.

                  <ls_csdoc>-x-cshdr-updkz = 'U'.
                  <ls_csdoc>-x-cshdr-astat = 'A'.
                  APPEND <ls_csdoc> TO lt_csdoc_change.
*-------------------------------------------------------------------*
                  PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                             CHANGING lt_csdoc_change
                                                      lt_msg_change
                                                      lv_subrc.
*-------------------------------------------------------------------*
                ENDIF.
*-------------------------------------------------------------------*
* Create new Process Order for FORMAÇÃO
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                REFRESH lt_fpdoc_tman.
                PERFORM create_process_order USING ls_glflcma
                                                   c_crop_process-manutencao
                                                   lv_tplnr_fl
                                                   <ls_csdoc>-x-cshdr-datab
                                          CHANGING lt_fpdoc_tman.
*-------------------------------------------------------------------*
* Create new Task Order for TMANGEN/TINSP0001
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: MANUTEÇÃO
*-------------------------------------------------------------------*
                READ TABLE lt_fpdoc_tman INTO DATA(ls_fpdoc_tman) INDEX 1.
                IF sy-subrc EQ 0.
                  PERFORM document_infocus_lock USING ls_fpdoc_tman-aufnr
                                             CHANGING lv_subrc.
                  IF lv_subrc EQ 0.
                    PERFORM create_task_order USING lv_tplnr_fl
                                                    'TMANGEN'
                                           CHANGING lt_fpdoc_tman.

                    PERFORM create_task_order USING lv_tplnr_fl
                                                    'TINSP0001'
                                           CHANGING lt_fpdoc_tman.

                    PERFORM document_infocus_unlock USING ls_fpdoc_tman-aufnr
                                                          ls_fpdoc_tman-x-fphdr-rsnum.
                  ENDIF.
                ENDIF.
                REFRESH lt_fpdoc_tcol.
*-------------------------------------------------------------------*
* Create new Process Order for COLHEITA
* CMNUM: CITROS / VARIA: MANUT&COLHEITA / CPROS: COLHEITA
*-------------------------------------------------------------------*
                PERFORM create_process_order USING ls_glflcma
                                                   c_crop_process-colheita
                                                   lv_tplnr_fl
                                                   <ls_csdoc>-x-cshdr-datab
                                          CHANGING lt_fpdoc_tcol.
*-------------------------------------------------------------------*
* Create Yard Order
*-------------------------------------------------------------------*
*/AGRI/SAPLGLCSM -> FCODE_MYCRE -> PERFORM create_yard_order CHANGING lv_subrc.
                IF <ls_csdoc>-x-cshdr-yaufnr IS INITIAL.
                  CLEAR lv_subrc.
                  PERFORM create_yard_order CHANGING <ls_csdoc>
                                                     lv_subrc.
                  IF lv_subrc EQ 0
                  AND <ls_csdoc>-x-cshdr-yaufnr IS NOT INITIAL.
                    IF lt_csdoc_change[] IS INITIAL.
                      <ls_csdoc>-x-cshdr-updkz = c_updkz_update.
                      APPEND <ls_csdoc> TO lt_csdoc_change.
                    ELSE.
                      READ TABLE lt_csdoc_change
                        ASSIGNING FIELD-SYMBOL(<ls_csdoc_change>) INDEX 1.
                      IF sy-subrc EQ 0.
                        <ls_csdoc_change> = <ls_csdoc>.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ELSE.
*-- Talhão &1: Ordem Pátio &2 existe para Época de Cultura 'MANUT&COLHEITA'.
                  INSERT INITIAL LINE INTO TABLE gt_message
                    ASSIGNING <ls_message>.
                  IF sy-subrc EQ 0.
                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = <ls_csdoc>-x-cshdr-yaufnr
                      IMPORTING
                        output = lv_aufnr_out.

                    <ls_message>-msgid = 'ZFMFP'.
                    <ls_message>-msgno = '374'.
                    <ls_message>-msgty = 'S'.
                    <ls_message>-msgv1 = lv_tplnr_fl.
                    <ls_message>-msgv2 = lv_aufnr_out.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE lt_csdoc_temp INTO ls_csdoc_temp
          INDEX lv_tabix COMPARING tplnr_fl.
      ENDWHILE.

      PERFORM save_csdoc_changes    USING lv_tplnr_fl
                                 CHANGING lt_csdoc_change
                                          lt_msg_change
                                          lv_subrc.

      PERFORM flcma_create    USING lv_tplnr_fl
                           CHANGING lt_flcma_create
                                    lt_csdoc_create
                                    lt_msg_create
                                    lv_log_create
                                    lv_subrc.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHANGE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_screen .

  LOOP AT SCREEN.
    IF p_enh EQ abap_true.
      IF screen-group1 = 'ID2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.
    ELSEIF p_enh EQ abap_false.
      IF screen-group1 = 'ID2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_CSDOC_CHANGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_CSDOC_CHANGE
*&      <-- LT_MSG_CHANGE
*&---------------------------------------------------------------------*
FORM save_csdoc_changes USING    lv_tplnr_fl     TYPE /agri/gltplnr_fl
                        CHANGING lt_csdoc_change TYPE /agri/t_glcs_doc
                                 lt_msg_change   TYPE /agri/t_gprolog
                                 lv_subrc        TYPE sysubrc.

*-- Grava Modificações na Época de Cultura
  IF lt_csdoc_change[] IS NOT INITIAL.
    REFRESH lt_msg_change.

    CALL FUNCTION '/AGRI/GLCS_SAVE'
      EXPORTING
        i_set_update_task  = abap_true
        i_commit_work      = 'X'
      CHANGING
        ct_csdoc           = lt_csdoc_change
        ct_messages        = lt_msg_change
      EXCEPTIONS
        no_change          = 1
        error_while_saving = 2
        OTHERS             = 3.

    IF sy-subrc NE 0.
      IF sy-msgid IS NOT INITIAL
      AND sy-msgty IS NOT INITIAL
      AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.

        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgid = sy-msgid.
          <ls_message>-msgno = sy-msgno.
          <ls_message>-msgty = sy-msgty.
          <ls_message>-msgv1 = sy-msgv1.
          <ls_message>-msgv2 = sy-msgv2.
          <ls_message>-msgv3 = sy-msgv3.
          <ls_message>-msgv4 = sy-msgv4.
        ENDIF.
      ENDIF.
      lv_subrc = 4.
    ELSE.
*-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
      READ TABLE lt_csdoc_change INTO DATA(ls_csdoc_change) INDEX 1.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '358'.
          <ls_message>-msgty = 'S'.
          <ls_message>-msgv1 = lv_tplnr_fl.
          <ls_message>-msgv2 = ls_csdoc_change-x-cshdr-cmnum.
          <ls_message>-msgv3 = ls_csdoc_change-x-cshdr-varia.
          <ls_message>-msgv4 = ls_csdoc_change-x-cshdr-astat.
        ENDIF.
      ENDIF.
      lv_subrc = 0.
    ENDIF.

    LOOP AT lt_msg_change INTO DATA(ls_msg_change).
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = ls_msg_change-msgid.
        <ls_message>-msgno = ls_msg_change-msgno.
        <ls_message>-msgty = ls_msg_change-msgty.
        <ls_message>-msgv1 = ls_msg_change-msgv1.
        <ls_message>-msgv2 = ls_msg_change-msgv2.
        <ls_message>-msgv3 = ls_msg_change-msgv3.
        <ls_message>-msgv4 = ls_msg_change-msgv4.
      ENDIF.
    ENDLOOP.

    REFRESH lt_csdoc_change.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FLCMA_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FLCMA_CREATE
*&      <-- LT_CSDOC_CREATE
*&      <-- LT_MSG_CREATE
*&      <-- LV_LOG_CREATE
*&---------------------------------------------------------------------*
FORM flcma_create USING    lv_tplnr_fl     TYPE /agri/gltplnr_fl
                  CHANGING lt_flcma_create TYPE /agri/t_glflcma
                           lt_csdoc_create TYPE /agri/t_glcs_doc
                           lt_msg_create   TYPE /agri/t_gprolog
                           lv_log_create   TYPE balognr
                           lv_subrc        TYPE sysubrc.

*-- Validação Adicional anterior à criação da Época de Cultura MANUTENÇÂO
  IF lt_flcma_create[] IS NOT INITIAL.
    SELECT tplnr_fl, contr, cmnum, varia,
           season, datab, datbi, loevm
      FROM /agri/glflcma
      INTO TABLE @DATA(lt_glflcma_db)
      FOR ALL ENTRIES IN @lt_flcma_create
     WHERE tplnr_fl = @lt_flcma_create-tplnr_fl
       AND cmnum    = @lt_flcma_create-cmnum
       AND varia    = @lt_flcma_create-varia
       AND season   = @lt_flcma_create-season
       AND datab    = @lt_flcma_create-datab
       AND datbi    = @lt_flcma_create-datbi
       AND loevm    = @abap_false.

    SORT lt_glflcma_db BY tplnr_fl cmnum varia season datab datbi loevm.

    IF sy-subrc EQ 0.
      LOOP AT lt_flcma_create INTO DATA(ls_flcma_create).
        DATA(lv_tabix) = sy-tabix.
        READ TABLE lt_glflcma_db INTO DATA(ls_glflcma_db)
          WITH KEY tplnr_fl = ls_flcma_create-tplnr_fl
                   cmnum    = ls_flcma_create-cmnum
                   varia    = ls_flcma_create-varia
                   season   = ls_flcma_create-season
                   datab    = ls_flcma_create-datab
                   datbi    = ls_flcma_create-datbi BINARY SEARCH.
        IF sy-subrc EQ 0.
          INSERT INITIAL LINE INTO TABLE gt_message
            ASSIGNING FIELD-SYMBOL(<ls_message>).
          IF sy-subrc EQ 0.
*-- Talhão &1: Época de Cultura &2 já existe.
            <ls_message>-msgid = 'ZFMFP'.
            <ls_message>-msgno = '376'.
            <ls_message>-msgty = 'S'.
            <ls_message>-msgv1 = lv_tplnr_fl.
            <ls_message>-msgv2 = ls_flcma_create-varia.
          ENDIF.
          DELETE lt_flcma_create INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-- Cria Época de Cultura
  IF lt_flcma_create[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLCS_CREATE'
      EXPORTING
        it_flcma                = lt_flcma_create
      IMPORTING
        et_csdoc                = lt_csdoc_create
        et_messages             = lt_msg_create
        e_log_number            = lv_log_create
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.

    IF sy-subrc NE 0.
      IF sy-msgid IS NOT INITIAL
      AND sy-msgty IS NOT INITIAL
      AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.

        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = sy-msgid.
          <ls_message>-msgno = sy-msgno.
          <ls_message>-msgty = sy-msgty.
          <ls_message>-msgv1 = sy-msgv1.
          <ls_message>-msgv2 = sy-msgv2.
          <ls_message>-msgv3 = sy-msgv3.
          <ls_message>-msgv4 = sy-msgv4.
        ENDIF.
      ENDIF.
      lv_subrc = 4.
    ELSE.
      lv_subrc = 0.
    ENDIF.

    LOOP AT lt_msg_create INTO DATA(ls_msg_create).
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = ls_msg_create-msgid.
        <ls_message>-msgno = ls_msg_create-msgno.
        <ls_message>-msgty = ls_msg_create-msgty.
        <ls_message>-msgv1 = ls_msg_create-msgv1.
        <ls_message>-msgv2 = ls_msg_create-msgv2.
        <ls_message>-msgv3 = ls_msg_create-msgv3.
        <ls_message>-msgv4 = ls_msg_create-msgv4.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MASS_TECO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> C_CROP_SEASON_MANUTENCAO
*&      --> LS_GLFLCMA_MANU1
*&      --> ABAP_TRUE
*&---------------------------------------------------------------------*
FORM mass_teco USING lv_crop_season  TYPE /agri/glvaria
                     lv_crop_process TYPE /agri/glcpros
                     ls_glflcma      TYPE /agri/glflcma.

  DATA: lt_aufnr    TYPE /agri/t_fmaufnr,
        lt_msg_teco TYPE /agri/t_gprolog.

  DATA(lt_fmfphdr_aux) = gt_fmfphdr[].
  DELETE lt_fmfphdr_aux WHERE varia NE lv_crop_season
                           OR tplnr_fl NE ls_glflcma-tplnr_fl.

*-- Verificação Adicional: todas as ordens devem ser encerradas
  IF lv_crop_process EQ c_crop_process-close_all.
    DELETE lt_fmfphdr_aux WHERE tplnr_fl NE ls_glflcma-tplnr_fl
                             OR contr    NE ls_glflcma-contr
                             OR cmnum    NE ls_glflcma-cmnum
                             OR varia    NE ls_glflcma-varia
                             OR iwerk    NE ls_glflcma-iwerk
                             OR tecom    EQ abap_true.
  ELSE.
    DELETE lt_fmfphdr_aux WHERE tplnr_fl NE ls_glflcma-tplnr_fl
                             OR contr    NE ls_glflcma-contr
                             OR cmnum    NE ls_glflcma-cmnum
                             OR varia    NE ls_glflcma-varia
                             OR cpros    NE lv_crop_process
                             OR iwerk    NE ls_glflcma-iwerk
                             OR tecom    EQ abap_true.
  ENDIF.

  IF lt_fmfphdr_aux[] IS NOT INITIAL.
    lt_aufnr = CORRESPONDING #( lt_fmfphdr_aux ).
*-- Order Mass Technical Complete
    CALL FUNCTION 'ZABS_FM_ORDER_MASS_TECO'
      EXPORTING
        it_aufnr    = lt_aufnr
      IMPORTING
        et_messages = lt_msg_teco.

    APPEND LINES OF lt_msg_teco TO gt_message.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form YARD_TECH_COMPLETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_GLFLCMA_MANU1_YAUFNR
*&---------------------------------------------------------------------*
FORM yard_tech_complete USING lv_yaufnr TYPE /agri/glyaufnr.

  DATA: lt_orders TYPE TABLE OF bapi_order_key,
        lt_return TYPE cocf_t_bapi_return,
        ls_status TYPE bapiret2.

*-- Encerra Ordem Pátio
  IF lv_yaufnr IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lt_orders
      ASSIGNING FIELD-SYMBOL(<ls_order>).
    IF sy-subrc EQ 0.
      <ls_order>-order_number = lv_yaufnr.
      CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
        IMPORTING
          return        = ls_status
        TABLES
          orders        = lt_orders
          detail_return = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*-- Ordem Pátio &1.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '375'.
          <ls_message>-msgty = 'S'.
          <ls_message>-msgv1 = lv_yaufnr.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      LOOP AT lt_return INTO DATA(ls_return).
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = ls_return-id.
          <ls_message>-msgno = ls_return-number.
          <ls_message>-msgty = ls_return-type.
          <ls_message>-msgv1 = ls_return-message_v1.
          <ls_message>-msgv2 = ls_return-message_v2.
          <ls_message>-msgv3 = ls_return-message_v3.
          <ls_message>-msgv4 = ls_return-message_v4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
