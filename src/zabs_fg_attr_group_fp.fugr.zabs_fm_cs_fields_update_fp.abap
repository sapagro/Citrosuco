************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_FM_CS_FIELDS_UPDATE                        *
* Tcode             :                                                  *
* Created By        :  Helio Kababe                                    *
* Requested by      :  Daniele Janes                                   *
* Created on        :  13.05.2021                                      *
* TR                :  C4DK934157                                      *
* Version           :  001                                             *
* Description       :  Tipo'ZPTA' e Grupo 'FAZ_PREVISAO'               *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
FUNCTION zabs_fm_cs_fields_update_fp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_MDHDR) TYPE  /AGRI/S_GLMDHDR
*"     REFERENCE(IT_MDITM) TYPE  /AGRI/T_GLMDITM
*"     REFERENCE(IT_MDATV) TYPE  /AGRI/T_GLMDATV
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ly_atributos,
           variedade_tecnica    TYPE atinn,
           porta_enxerto        TYPE atinn,
           espacamento_rua      TYPE atinn,
           espacamento_pes      TYPE atinn,
           qtd_plantas          TYPE atinn,
           area_talhao          TYPE atinn,
           data_plantio         TYPE atinn,
           previsao_erradicacao TYPE atinn,
           safra                TYPE atinn,
           cancel_previsao      TYPE atinn,
         END OF ly_atributos,

         BEGIN OF ly_datas,
           varia TYPE /agri/glvaria,
           index TYPE syindex,
           begda TYPE begda,
           endda TYPE endda,
           data  TYPE sydatum,
         END OF ly_datas.

*--Internal table declarations
  DATA: lt_tplnr        TYPE /agri/t_gltplnr,
        lt_datas        TYPE STANDARD TABLE OF ly_datas,
        lt_fmfphdr      TYPE gy_fmfphdr_tab,
        lt_msg_teco     TYPE /agri/t_gprolog,
        lt_fldoc        TYPE /agri/t_glfl_doc,
        lt_clint        TYPE /agri/t_gclint,
        lt_atnam        TYPE /agri/t_gatnam,
        lt_mdclass      TYPE /agri/t_glatgrp,
        lt_messages     TYPE /agri/t_gprolog,
        lt_cskey        TYPE /agri/t_glcs_key,
        lt_csdoc        TYPE /agri/t_glcs_doc,
        lt_csdoc_temp   TYPE /agri/t_glcs_doc,
        lt_csdoc_del    TYPE /agri/t_glcs_doc,
        lt_klah         TYPE tt_klah,
        lt_cs_afn       TYPE /plmb/t_fieldname,
        lt_flcma_create TYPE /agri/t_glflcma,
*--Workarea declarations
        lwa_duration    TYPE psen_duration,
        lwa_atnam       TYPE /agri/s_gatnam,
        lwa_atributo    TYPE ly_atributos,
        lwa_clint       TYPE /agri/s_gclint,
        lwa_attr_val    TYPE auspdata,
        lwa_flatv       TYPE /agri/s_glflatv,
        lwa_glflcmaca   TYPE zabs_str_glflcmaca,
        lwa_cs_afn      TYPE /plmb/s_fieldname,
        lwa_comp        TYPE abap_compdescr,
*--Local Reference declarations
        lref_descr      TYPE REF TO cl_abap_structdescr,
        lref_date       TYPE REF TO data,
*--Local Variables
        lv_tplnr_fl     TYPE /agri/gltplnr_fl,
        lv_datum8       TYPE char8,
        lv_datum        TYPE sy-datum,
        lv_begda        TYPE begda,
        lv_endda        TYPE endda,
        lv_gyear        TYPE char4,
        lv_begda_manu   TYPE sydatum,
        lv_endda_manu   TYPE sydatum,
        lv_date_null    TYPE sydatum,
        lv_prev_errad   TYPE sydatum,
        lv_data_errad   TYPE sydatum,
        ls_duration     TYPE psen_duration,
        lv_i            TYPE i.

  FIELD-SYMBOLS: <fs_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  DO 9 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
*-- Variedade Técnica
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_VAR_MP_TECNICA'
          IMPORTING
            output = lwa_atributo-variedade_tecnica.
*-- Porta-Enxerto
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-PORTA-ENXERTO'
          IMPORTING
            output = lwa_atributo-porta_enxerto.
*-- Espaçamento entre Rua
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESPACAMENTO-RUA'
          IMPORTING
            output = lwa_atributo-espacamento_rua.
*-- Espaçamento entre Pés
      WHEN 4.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESPACAMENTO-PES'
          IMPORTING
            output = lwa_atributo-espacamento_pes.
*-- Quantidade de Plantas
      WHEN 5.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_QUANTIDADE_PLANTAS'
          IMPORTING
            output = lwa_atributo-qtd_plantas.
*-- Área do talhão
      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_AREA_TALHAO'
          IMPORTING
            output = lwa_atributo-area_talhao.
*-- Data de Plantio
      WHEN 7.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_FIM_MP_PLANTIO'
          IMPORTING
            output = lwa_atributo-data_plantio.
*-- Previsão de Erradicação
      WHEN 8.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_PREV_ERRADICACAO'
          IMPORTING
            output = lwa_atributo-previsao_erradicacao.
*-- BOC T_T.KONNO-07.12.21
**-- Safra
*      WHEN 9.
*        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*          EXPORTING
*            input  = 'CIT-SAFRA'
*          IMPORTING
*            output = lwa_atributo-safra.
*-- Cancelamento Previsão
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_CAL_PREV'
          IMPORTING
            output = lwa_atributo-cancel_previsao.
*-- EOC T_T.KONNO-07.12.21
    ENDCASE.
  ENDDO.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
    EXPORTING
      input  = is_mdhdr-tplnr_fl
    IMPORTING
      output = lv_tplnr_fl.

*--Get Fieldnames from additional fileds structure of crop season
  lref_descr ?= cl_abap_structdescr=>describe_by_data( lwa_glflcmaca ).
  LOOP AT lref_descr->components INTO lwa_comp.
    lwa_cs_afn-fieldname = lwa_comp-name.
    APPEND lwa_cs_afn TO lt_cs_afn.
  ENDLOOP.

*--Fetch MD attribute, attribute group and filedname from custom table
  SELECT *
    FROM zabst_md_atr_map
    INTO TABLE @DATA(lt_md_attr_map)
    FOR ALL ENTRIES IN @lt_cs_afn
    WHERE fieldname = @lt_cs_afn-fieldname
      AND mdclass   = @is_mdhdr-mpgrp.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    SORT lt_md_attr_map BY mdclass mdatnam.
  ENDIF.

*--Fetch class header data
  SELECT *
    FROM klah
    INTO TABLE lt_klah
    WHERE class = is_mdhdr-mpgrp.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*--Calling meathod to get attribute header data
  CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
    EXPORTING
      it_klah  = lt_klah
      i_agtyp  = zcl_abs_abap_maintain=>c_agtyp_measurement_doc
    IMPORTING
      et_athdr = DATA(lt_athdr).
  IF lt_athdr IS NOT INITIAL.
    SORT lt_athdr BY atnam.
  ENDIF.

*-- BOC T_T.KONNO-07.12.21
*  SELECT *
*    FROM /agri/glflcma
*    INTO TABLE @DATA(lt_glflcma)
*   WHERE tplnr_fl EQ @is_mdhdr-tplnr_fl
*     AND astat    NE 'C'.
  DATA(lv_canc_prev) = abap_false.
*-- EOC T_T.KONNO-07.12.21

  DATA(lt_mdatv) = it_mdatv[].
*-- BOC T_T.KONNO-07.12.21
*  READ TABLE lt_mdatv ASSIGNING FIELD-SYMBOL(<lwa_mdatv>)
*    WITH KEY atinn = lwa_atributo-safra.
*  IF sy-subrc EQ 0.
*    CONDENSE <lwa_mdatv>-atwrt NO-GAPS.
*    lv_gyear = <lwa_mdatv>-atwrt(4).
*    LOOP AT lt_glflcma INTO DATA(ls_glflcma).
*      lv_index = sy-tabix.
*      IF lv_gyear NE ls_glflcma-datab(4).
*        DELETE lt_glflcma INDEX lv_index.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*-- Cancelamento Previsão
  READ TABLE lt_mdatv INTO DATA(ls_canc_prev)
    WITH KEY atinn = lwa_atributo-cancel_previsao.
  IF sy-subrc EQ 0.
    IF ls_canc_prev-atwrt IS NOT INITIAL.
      lv_canc_prev = abap_true.
    ENDIF.
  ENDIF.

  IF lv_canc_prev = abap_false.
    SELECT *
      FROM /agri/glflcma
      INTO TABLE @DATA(lt_glflcma)
     WHERE tplnr_fl EQ @is_mdhdr-tplnr_fl
       AND zzprevisto EQ 'X'
       AND loevm      NE 'X'.
  ELSE.
*-- Cancelamento de Previsão Erradicação
    IF ls_canc_prev-atwrt EQ 'E'.
      SELECT *
        FROM /agri/glflcma
        INTO TABLE @lt_glflcma
       WHERE tplnr_fl     EQ @is_mdhdr-tplnr_fl
         AND cmnum        EQ 'CITROS'
         AND varia        EQ 'MANUT&COLHEITA'
         AND season       EQ 'SAFRA PROD'
         AND zzprevisto   EQ 'X'
         AND loevm        NE 'X'
         AND zzprev_errad NE @lv_date_null.
*-- Cancelamento de Previsão Plantio
    ELSEIF ls_canc_prev-atwrt EQ 'P'.
      SELECT *
        FROM /agri/glflcma
        INTO TABLE @lt_glflcma
       WHERE tplnr_fl       EQ @is_mdhdr-tplnr_fl
         AND cmnum          EQ 'CITROS'
         AND varia          EQ 'FORMAÇÃO'
         AND season         EQ 'SAFRA PLAN'
         AND zzprevisto     EQ 'X'
         AND loevm          NE 'X'
         AND zzprev_plantio NE @lv_date_null.
    ENDIF.
  ENDIF.
*-- EOC T_T.KONNO-07.12.21

  DATA(lv_form) = abap_true.
  IF lv_canc_prev EQ abap_true.
*-- Cancelamento de Previsão Erradicação
    IF ls_canc_prev-atwrt EQ 'E'.
      lv_form = abap_false.
*-- Cancelamento de Previsão Plantio
    ELSEIF ls_canc_prev-atwrt EQ 'P'.
      lv_form = abap_true.
    ENDIF.
  ELSE.
*-- Previsão de Erradicação Preenchida - MANUT&COLHEITA
*-- Data de Plantio Preenchida - FORMAÇÃO
    READ TABLE lt_mdatv INTO DATA(ls_mdatv)
      WITH KEY atinn = lwa_atributo-previsao_erradicacao."MANUTENÇÃO
    IF sy-subrc EQ 0.
      IF ls_mdatv-atflv IS NOT INITIAL.
        lv_form = abap_false.
        CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
          EXPORTING
            float_imp  = ls_mdatv-atflv
            format_imp = lv_i
          IMPORTING
            char_exp   = lv_datum8.

        lv_data_errad = lv_datum8.
        CLEAR lv_datum8.
      ENDIF.
    ENDIF.
  ENDIF.

*-- BOC T_T.KONNO-07.12.21
*  IF lv_form EQ abap_true.
*    DELETE lt_glflcma WHERE varia NE 'FORMAÇÃO'.
*  ELSE.
*    DELETE lt_glflcma WHERE varia NE 'MANUT&COLHEITA'.
*  ENDIF.
  IF lv_form EQ abap_true.
    DELETE lt_glflcma WHERE varia NE 'FORMAÇÃO'
                         OR zzprev_plantio IS INITIAL.
  ELSE.
    DELETE lt_glflcma WHERE varia NE 'MANUT&COLHEITA'
                         OR zzprev_errad IS INITIAL.
  ENDIF.

  DATA(lv_exception) = abap_false.
  IF lv_canc_prev EQ abap_false
  AND lv_form EQ abap_false
  AND lv_data_errad NE lv_date_null.

    READ TABLE lt_glflcma INTO DATA(ls_glflcma) INDEX 1.
    IF sy-subrc EQ 0.

      lv_begda_manu = ls_glflcma-datab  + 1.

      SELECT SINGLE inicio_safra
        INTO @DATA(lv_ini_safra)
        FROM zfmfpsafras
       WHERE ano_safra = @lv_begda_manu(4)
         AND tipo_safra = 'C'.

      IF lv_ini_safra > lv_data_errad.

        "      IF lv_data_errad NOT BETWEEN ls_glflcma-datab AND ls_glflcma-datbi.
        lt_cskey = CORRESPONDING #( lt_glflcma ).

        IF lt_cskey[] IS NOT INITIAL.
          CALL FUNCTION '/AGRI/GLCS_VIEW'
            EXPORTING
              it_cskey       = lt_cskey
            IMPORTING
              et_csdoc       = lt_csdoc_temp
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.

          IF sy-subrc EQ 0.
            LOOP AT lt_csdoc_temp INTO DATA(lwa_csdoc_temp).
              DATA(lv_contr) = lwa_csdoc_temp-contr.
              READ TABLE lt_glflcma INTO ls_glflcma
                WITH KEY tplnr_fl = lwa_csdoc_temp-tplnr_fl
                         contr    = lwa_csdoc_temp-contr.
              IF sy-subrc NE 0.
                CLEAR ls_glflcma.
                CONTINUE.
              ENDIF.

              UNASSIGN <fs_csdoc>.
              ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
              IF <fs_csdoc> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              <fs_csdoc>-x-cshdr-updkz = 'U'.
              <fs_csdoc>-updkz = abap_true.
              <fs_csdoc>-x-cshdr-loevm = abap_true.
              APPEND <fs_csdoc> TO lt_csdoc.
            ENDLOOP.

            IF lt_csdoc[] IS NOT INITIAL.
              CALL FUNCTION '/AGRI/GLCS_SAVE'
                EXPORTING
                  i_set_update_task  = abap_true
                  i_commit_work      = 'X'
                CHANGING
                  ct_csdoc           = lt_csdoc
                  ct_messages        = lt_messages
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

                  INSERT INITIAL LINE INTO TABLE et_messages
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
*-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
                READ TABLE lt_csdoc INTO DATA(ls_csdoc) INDEX 1.
                IF sy-subrc EQ 0.
                  INSERT INITIAL LINE INTO TABLE et_messages
                    ASSIGNING <ls_message>.
                  IF sy-subrc EQ 0.
                    <ls_message>-msgid = 'ZFMFP'.
                    <ls_message>-msgno = '358'.
                    <ls_message>-msgty = 'S'.
                    <ls_message>-msgv1 = lv_tplnr_fl.
                    <ls_message>-msgv2 = ls_csdoc-x-cshdr-cmnum.
                    <ls_message>-msgv3 = ls_csdoc-x-cshdr-varia.
                    <ls_message>-msgv4 = ls_csdoc-x-cshdr-astat.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        REFRESH: lt_cskey, lt_csdoc_temp, lt_csdoc,
                 lt_messages, lt_glflcma.

        SELECT *
          FROM /agri/glflcma
          INTO TABLE @lt_glflcma
         WHERE tplnr_fl     EQ @is_mdhdr-tplnr_fl
           AND cmnum        EQ 'CITROS'
           AND varia        EQ 'MANUT&COLHEITA'
           AND season       EQ 'SAFRA PROD'
           AND astat        NE 'C'
           AND zzprevisto   EQ @abap_false
           AND loevm        NE 'X'
           AND zzprev_errad EQ @lv_date_null.

        LOOP AT lt_glflcma INTO ls_glflcma.
          DATA(lv_tabix) = sy-tabix.
          IF lv_data_errad NOT BETWEEN ls_glflcma-datab AND ls_glflcma-datbi.
            DELETE lt_glflcma INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

        lv_exception = abap_true.
        REFRESH lt_md_attr_map.

        lt_cskey = CORRESPONDING #( lt_glflcma ).

        IF lt_cskey[] IS NOT INITIAL.
          CALL FUNCTION '/AGRI/GLCS_VIEW'
            EXPORTING
              it_cskey       = lt_cskey
            IMPORTING
              et_csdoc       = lt_csdoc_temp
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.

          IF sy-subrc EQ 0.
            LOOP AT lt_csdoc_temp INTO lwa_csdoc_temp.
              lv_contr = lwa_csdoc_temp-contr.
              READ TABLE lt_glflcma INTO ls_glflcma
                WITH KEY tplnr_fl = lwa_csdoc_temp-tplnr_fl
                         contr    = lwa_csdoc_temp-contr.
              IF sy-subrc NE 0.
                CLEAR ls_glflcma.
                CONTINUE.
              ENDIF.

              UNASSIGN <fs_csdoc>.
              ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
              IF <fs_csdoc> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              <fs_csdoc>-x-cshdr-updkz = 'U'.
              <fs_csdoc>-updkz = abap_true.
              <fs_csdoc>-x-cshdr-zzprevisto = abap_true.
              <fs_csdoc>-x-cshdr-datbi = lv_data_errad.
              <fs_csdoc>-x-cshdr-zzprev_errad = lv_data_errad.
*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
              LOOP AT <fs_csdoc>-x-csprs
                ASSIGNING FIELD-SYMBOL(<fs_csprs>).
                <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
                <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
                <fs_csprs>-updkz = 'U'.
              ENDLOOP.
              APPEND <fs_csdoc> TO lt_csdoc.
            ENDLOOP.

            IF lt_csdoc[] IS NOT INITIAL.
              CALL FUNCTION '/AGRI/GLCS_SAVE'
                EXPORTING
                  i_set_update_task  = abap_true
                  i_commit_work      = 'X'
                CHANGING
                  ct_csdoc           = lt_csdoc
                  ct_messages        = lt_messages
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

                  INSERT INITIAL LINE INTO TABLE et_messages
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
              ELSE.
*-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
                READ TABLE lt_csdoc INTO ls_csdoc INDEX 1.
                IF sy-subrc EQ 0.
                  INSERT INITIAL LINE INTO TABLE et_messages
                    ASSIGNING <ls_message>.
                  IF sy-subrc EQ 0.
                    <ls_message>-msgid = 'ZFMFP'.
                    <ls_message>-msgno = '358'.
                    <ls_message>-msgty = 'S'.
                    <ls_message>-msgv1 = lv_tplnr_fl.
                    <ls_message>-msgv2 = ls_csdoc-x-cshdr-cmnum.
                    <ls_message>-msgv3 = ls_csdoc-x-cshdr-varia.
                    <ls_message>-msgv4 = ls_csdoc-x-cshdr-astat.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        REFRESH: lt_cskey, lt_csdoc_temp, lt_csdoc,
                 lt_messages, lt_glflcma.
      ENDIF.
    ENDIF.
  ENDIF.
*-- EOC T_T.KONNO-07.12.21

  IF lt_glflcma[] IS INITIAL.
    RETURN.
  ELSE.
*-- Fetch all CPROS assigned to crop and variant
    SELECT c~cmnum, c~varia, c~cpros, c~matnr,
           c~midur, c~miunt, t~descr
      FROM /agri/glcmprs AS c
      INNER JOIN /agri/glcmprst AS t
      ON  c~cmnum = t~cmnum
      AND c~varia = t~varia
      AND c~cpros = t~cpros
      INTO TABLE @DATA(lt_cpros)
      FOR ALL ENTRIES IN @lt_glflcma
     WHERE c~cmnum EQ @lt_glflcma-cmnum
       AND c~varia EQ @lt_glflcma-varia
       AND t~spras EQ @sy-langu.

    SORT lt_cpros BY cmnum varia cpros.

*-- Process orders
    SELECT aufnr, auart, autyp, tplnr_fl, contr,
           tplma, cmnum, varia, cpros, iwerk, tecom
      FROM /agri/fmfphdr
      INTO TABLE @lt_fmfphdr
      FOR ALL ENTRIES IN @lt_glflcma
     WHERE tplnr_fl EQ @lt_glflcma-tplnr_fl
       AND cmnum    EQ @lt_glflcma-cmnum
       AND varia    EQ @lt_glflcma-varia
       AND iwerk    EQ @lt_glflcma-iwerk
       AND tecom    EQ @abap_false.
  ENDIF.

  DO 2 TIMES.
    lv_index = sy-index.
    CASE lv_index.
      WHEN 1.
*-- Previsão de Erradicação MANUTENÇÃO (MANUT&COLHEITA)
        READ TABLE lt_mdatv ASSIGNING FIELD-SYMBOL(<lwa_mdatv>)
          WITH KEY atinn = lwa_atributo-previsao_erradicacao.
        IF sy-subrc NE 0.
          UNASSIGN <lwa_mdatv>.
        ENDIF.
      WHEN 2.
*-- Data de Plantio FORMAÇÃO (FORMAÇÃO)
        READ TABLE lt_mdatv ASSIGNING <lwa_mdatv>
          WITH KEY atinn = lwa_atributo-data_plantio.
        IF sy-subrc NE 0.
          UNASSIGN <lwa_mdatv>.
        ENDIF.
    ENDCASE.

    CLEAR: lv_datum8, lv_datum, lv_begda, lv_endda.

    INSERT INITIAL LINE INTO TABLE lt_datas
      ASSIGNING FIELD-SYMBOL(<ls_data>).
    IF sy-subrc EQ 0.
      <ls_data>-index = lv_index.

      IF lv_index EQ 1.
        <ls_data>-varia = 'MANUT&COLHEITA'.
      ELSEIF lv_index = 2.
        <ls_data>-varia = 'FORMAÇÃO'.
      ENDIF.

      IF <lwa_mdatv> IS ASSIGNED
      AND <lwa_mdatv>-atwrt IS INITIAL.
        IF <lwa_mdatv>-atflv IS NOT INITIAL.
          CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
            EXPORTING
              float_imp  = <lwa_mdatv>-atflv
              format_imp = lv_i
            IMPORTING
              char_exp   = lv_datum8.

          lv_datum = lv_datum8.
          <ls_data>-data = lv_datum.
          lv_begda = lv_datum(6) && '01'.
          CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
            EXPORTING
              i_day_in            = lv_begda
            IMPORTING
              e_last_day_of_month = lv_endda.

          IF <ls_data> IS ASSIGNED.
            <ls_data>-begda = lv_begda.
            <ls_data>-endda = lv_endda.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.

  lt_cskey = CORRESPONDING #( lt_glflcma ).

  IF lt_cskey[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      it_cskey       = lt_cskey
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_csdoc_temp BY tplnr_fl contr.

  IF lt_glflcma[] IS NOT INITIAL.
    SELECT *
      FROM zabs_csks
      INTO TABLE @DATA(lt_csks)
      FOR ALL ENTRIES IN @lt_glflcma
     WHERE cmnum = @lt_glflcma-cmnum
       AND iwerk = @lt_glflcma-iwerk.

    SORT lt_csks BY cmnum varia iwerk.
  ENDIF.

  LOOP AT lt_csdoc_temp INTO lwa_csdoc_temp.
    lv_contr = lwa_csdoc_temp-contr.

    UNASSIGN <fs_csdoc>.
    ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
    IF <fs_csdoc> IS NOT ASSIGNED.
      CONTINUE.
    ELSE.
      READ TABLE lt_datas INTO DATA(ls_data)
        WITH KEY varia = lwa_csdoc_temp-x-cshdr-varia.
      IF sy-subrc NE 0.
        CLEAR ls_data.
      ENDIF.
    ENDIF.

    READ TABLE lt_glflcma INTO ls_glflcma
      WITH KEY tplnr_fl = lwa_csdoc_temp-tplnr_fl
               contr    = lwa_csdoc_temp-contr.
    IF sy-subrc NE 0.
      CLEAR ls_glflcma.
      CONTINUE.
    ENDIF.

    LOOP AT lt_md_attr_map INTO DATA(lwa_md_attr_map).
      IF lwa_md_attr_map-mdatnam EQ 'DUMMY'.
        <fs_csdoc>-x-cshdr-zzprevisto = abap_true.
        <fs_csdoc>-x-cshdr-updkz = 'U'.
*-- Previsão de Data de Erradicação
        IF ls_data-index EQ 1.
          IF ls_data-endda IS NOT INITIAL.
*-- Previsão Erradicação -> Data Final Safra
            <fs_csdoc>-x-cshdr-datbi = ls_data-endda.
*-- Previsão Erradicação -> Data Final (Processos de épocas de cultura)
            LOOP AT <fs_csdoc>-x-csprs ASSIGNING FIELD-SYMBOL(<ls_csprs>).
              <ls_csprs>-updkz = 'U'.
              <ls_csprs>-enddt = ls_data-endda.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_glflcma-varia EQ 'MANUT&COLHEITA'.
        IF lwa_md_attr_map-fieldname NE 'ZZPREV_ERRAD'.
          CONTINUE.
        ENDIF.
      ELSEIF ls_glflcma-varia EQ 'FORMAÇÃO'.
        IF lwa_md_attr_map-fieldname EQ 'ZZPREV_ERRAD'.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_athdr INTO DATA(ls_athdr)
        WITH KEY atnam = lwa_md_attr_map-mdatnam BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_mdatv INTO DATA(lwa_mdatv)
        WITH KEY atinn = ls_athdr-atinn.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*--Convert float to char value.
      IF lwa_mdatv-atwrt IS INITIAL
      AND lwa_mdatv-atflv IS NOT INITIAL."--Added on 08/10/2020 T_C.KARANAM
        CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
        ASSIGN lref_date->* TO <fs_date>.
        <fs_date> = lwa_mdatv-atflv.
        lwa_mdatv-atwrt = <fs_date>.
        CONDENSE lwa_mdatv-atwrt NO-GAPS.
      ENDIF.

      ASSIGN COMPONENT lwa_md_attr_map-fieldname
        OF STRUCTURE <fs_csdoc>-x-cshdr TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = lwa_mdatv-atwrt.
        <fs_csdoc>-x-cshdr-updkz = 'U'.
        <fs_csdoc>-updkz = abap_true.

        IF lwa_md_attr_map-fieldname EQ 'ZZFAZVARTECNIA'
        AND lwa_md_attr_map-mdatnam EQ 'FAZ_VAR_MP_TECNICA'
        AND <fs_value> IS NOT INITIAL.
          <fs_csdoc>-x-cshdr-ymatnr = <fs_value>.
          IF <fs_csdoc>-x-cshdr-ymatnr IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <fs_csdoc>-x-cshdr-ymatnr
              IMPORTING
                output       = <fs_csdoc>-x-cshdr-ymatnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*-- BOC T_T.KONNO-07.12.21
*    IF <fs_csdoc>-x-cshdr-varia EQ 'FORMAÇÃO'
*    AND <fs_csdoc>-x-cshdr-astat EQ 'I'
*    AND <fs_csdoc>-x-cshdr-zzprevisto EQ 'X'
*    AND <fs_csdoc>-y-cshdr-zzprev_plantio IS NOT INITIAL.
*      READ TABLE lt_mdatv ASSIGNING <lwa_mdatv>
*        WITH KEY atinn = lwa_atributo-data_plantio.
*      IF sy-subrc = 0
*      AND ( <lwa_mdatv>-atflv IS NOT INITIAL OR
*            <lwa_mdatv>-atwrt IS NOT INITIAL ).
*        <fs_csdoc>-x-cshdr-datab = <fs_csdoc>-x-cshdr-zzprev_plantio.
**-- Recálculo Data Final de acordo com /AGRI/GLCMPRS
*        READ TABLE lt_cpros INTO DATA(ls_cpros)
*          WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
*                   varia = <fs_csdoc>-x-cshdr-varia
*                   cpros = c_crop_process-formacao BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          CLEAR lwa_duration.
*          CASE ls_cpros-miunt.
*            WHEN 'TAG'. "DIA
*              lwa_duration-durdd = ls_cpros-midur.
*            WHEN 'JHR'. "ANS
*              lwa_duration-duryy = ls_cpros-midur.
*          ENDCASE.
*
*          CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
*            EXPORTING
*              im_date     = <fs_csdoc>-x-cshdr-datab
*              im_operator = '+'
*              im_duration = lwa_duration
*            IMPORTING
*              ex_date     = <fs_csdoc>-x-cshdr-datbi.
*        ENDIF.
*
**-- Data Colheita
*        <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.
**-- Data Final [FORMAÇÃO] -> Data Final (Processos de épocas de cultura)
*        LOOP AT <fs_csdoc>-x-csprs ASSIGNING FIELD-SYMBOL(<fs_csprs>).
*          <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
*          <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datab.
*          READ TABLE lt_cpros INTO ls_cpros
*            WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
*                     varia = <fs_csdoc>-x-cshdr-varia
*                     cpros = <fs_csprs>-cpros BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            lv_i = ls_cpros-midur.
*            ADD lv_i TO <fs_csprs>-enddt.
*          ENDIF.
*          <fs_csprs>-updkz = 'U'.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
    IF <fs_csdoc>-x-cshdr-varia EQ 'FORMAÇÃO'
    AND <fs_csdoc>-x-cshdr-zzprevisto EQ 'X'
    AND <fs_csdoc>-y-cshdr-zzprev_plantio IS NOT INITIAL.
      IF <fs_csdoc>-x-cshdr-astat EQ 'I'.
        IF lv_canc_prev = abap_false.
          READ TABLE lt_mdatv ASSIGNING <lwa_mdatv>
            WITH KEY atinn = lwa_atributo-data_plantio.
          IF sy-subrc = 0
          AND ( <lwa_mdatv>-atflv IS NOT INITIAL OR
                <lwa_mdatv>-atwrt IS NOT INITIAL ).
            <fs_csdoc>-x-cshdr-datab = <fs_csdoc>-x-cshdr-zzprev_plantio.
*-- Recálculo Data Final de acordo com /AGRI/GLCMPRS
            READ TABLE lt_cpros INTO DATA(ls_cpros)
              WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
                       varia = <fs_csdoc>-x-cshdr-varia
                       cpros = c_crop_process-formacao BINARY SEARCH.
            IF sy-subrc EQ 0.
              CLEAR lwa_duration.
              CASE ls_cpros-miunt.
                WHEN 'TAG'. "DIA
                  lwa_duration-durdd = ls_cpros-midur.
                WHEN 'JHR'. "ANS
                  lwa_duration-duryy = ls_cpros-midur.
              ENDCASE.

              CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                EXPORTING
                  im_date     = <fs_csdoc>-x-cshdr-datab
                  im_operator = '+'
                  im_duration = lwa_duration
                IMPORTING
                  ex_date     = <fs_csdoc>-x-cshdr-datbi.
            ENDIF.

*-- Data Colheita
            <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.
*-- Data Final [FORMAÇÃO] -> Data Final (Processos de épocas de cultura)
            LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
              <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
              <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datab.
              READ TABLE lt_cpros INTO ls_cpros
                WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
                         varia = <fs_csdoc>-x-cshdr-varia
                         cpros = <fs_csprs>-cpros BINARY SEARCH.
              IF sy-subrc EQ 0.
                lv_i = ls_cpros-midur.
                ADD lv_i TO <fs_csprs>-enddt.
              ENDIF.
              <fs_csprs>-updkz = 'U'.
            ENDLOOP.
          ENDIF.
*-- Cancelamento de Previsão Plantio
        ELSEIF lv_canc_prev = abap_true.
          IF ls_canc_prev-atwrt EQ 'P'.
            <fs_csdoc>-x-cshdr-updkz = 'U'.
            <fs_csdoc>-updkz = abap_true.
            <fs_csdoc>-x-cshdr-loevm = abap_true.
          ENDIF.
        ENDIF.
      ELSEIF <fs_csdoc>-x-cshdr-astat EQ 'A'.
*-- Cancelamento de Previsão Plantio
        IF lv_canc_prev = abap_true.
          IF ls_canc_prev-atwrt EQ 'P'.
            <fs_csdoc>-x-cshdr-updkz = 'U'.
            <fs_csdoc>-updkz = abap_true.
            <fs_csdoc>-x-cshdr-loevm = abap_true.
*-------------------------------------------------------------------*
*-- Encerra todas as ordens FORMAÇÃO]
            REFRESH lt_msg_teco.
            PERFORM mass_teco USING lt_fmfphdr
                                    c_crop_season-formacao
                                    c_crop_process-close_all
                                    ls_glflcma
                           CHANGING lt_msg_teco.

            APPEND LINES OF lt_msg_teco TO et_messages.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio [MANUT&COLHEITA]
            REFRESH lt_msg_teco.
            PERFORM yard_tech_complete USING ls_glflcma-yaufnr
                                    CHANGING lt_msg_teco.

            APPEND LINES OF lt_msg_teco TO et_messages.
*-------------------------------------------------------------------*
*           CLEAR <fs_csdoc>-x-cshdr-zzprevisto.
*           CLEAR <fs_csdoc>-x-cshdr-zzprev_plantio.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*-- EOC T_T.KONNO-07.12.21

*-AQUI
**-- Cancelamento de Previsão Erradicação
*        ELSEIF ls_canc_prev-atwrt EQ 'E'.
*          <fs_csdoc>-x-cshdr-updkz = 'U'.
*          <fs_csdoc>-updkz = abap_true.
*          CLEAR <fs_csdoc>-x-cshdr-zzprevisto.
***-- Previsão Erradicação -> Data Final Safra
**          <fs_csdoc>-x-cshdr-datbi = ls_data-endda.
***-- Previsão Erradicação -> Data Final (Processos de épocas de cultura)
**          LOOP AT <fs_csdoc>-x-csprs ASSIGNING FIELD-SYMBOL(<ls_csprs>).
**            <ls_csprs>-updkz = 'U'.
**            <ls_csprs>-enddt = ls_data-endda.
**          ENDLOOP.
*
    IF <fs_csdoc>-x-cshdr-varia EQ 'MANUT&COLHEITA'
    AND <fs_csdoc>-x-cshdr-astat NE 'C'
    AND <fs_csdoc>-x-cshdr-zzprevisto EQ 'X'
    AND <fs_csdoc>-y-cshdr-zzprev_errad IS NOT INITIAL.
      IF lv_canc_prev = abap_false.
        READ TABLE lt_mdatv ASSIGNING <lwa_mdatv>
          WITH KEY atinn = lwa_atributo-previsao_erradicacao.
        IF sy-subrc = 0.
          IF <lwa_mdatv>-atflv IS NOT INITIAL.
            CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
              EXPORTING
                float = <lwa_mdatv>-atflv
              IMPORTING
                date  = <lwa_mdatv>-atwrt.
          ENDIF.

          lv_prev_errad = <lwa_mdatv>-atwrt.

          IF <fs_csdoc>-y-cshdr-zzprev_errad > lv_prev_errad.
            <fs_csdoc>-x-cshdr-datbi = <fs_csdoc>-x-cshdr-zzprev_errad.
*-- Data errad
            <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.
*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
            LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
              <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
              <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
              <fs_csprs>-updkz = 'U'.
            ENDLOOP.
          ELSE.
            lv_begda_manu = <fs_csdoc>-x-cshdr-datab + 1.

            SELECT SINGLE fim_safra
              INTO @DATA(lv_fim_safra)
              FROM zfmfpsafras
             WHERE ano_safra = @lv_begda_manu(4)
               AND tipo_safra = 'C'.

            IF lv_fim_safra < lv_prev_errad.
              <fs_csdoc>-x = <fs_csdoc>-y.
              CLEAR <fs_csdoc>-x-cshdr-zzprev_errad.
              CLEAR <fs_csdoc>-x-cshdr-zzprevisto.
              <fs_csdoc>-x-cshdr-datbi = lv_fim_safra.
*-- Data errad
              <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.

*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
              LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
                <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
                <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
                <fs_csprs>-updkz = 'U'.
              ENDLOOP.

*Amplia epoca de cultura
              CLEAR: lv_begda_manu, lv_endda_manu.
*-- SAFRA PROD [MANUTENÇÃO]
              SELECT SINGLE *
                FROM /agri/glseason
                INTO @DATA(ls_manutencao)
               WHERE season = 'SAFRA PROD'.

              IF sy-subrc EQ 0.
                IF ls_manutencao-offst LE 999.
                  lv_begda_manu = lv_fim_safra + 1.
                  "lv_begda_manu = lv_begda_manu(4) && ls_manutencao-sperd.
                  CLEAR ls_duration.
                  ls_duration-duryy = ls_manutencao-offst.

                  CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                    EXPORTING
                      im_date     = lv_begda_manu
                      im_operator = '+'
                      im_duration = ls_duration
                    IMPORTING
                      ex_date     = lv_endda_manu.

                  lv_endda_manu = lv_endda_manu - 1.

*-- Cria Época de Cultura de Manutenção
*-- Cultura [CITROS]; Variante [MANUT&COLHEITA]
                  INSERT INITIAL LINE INTO TABLE lt_flcma_create
                    ASSIGNING FIELD-SYMBOL(<ls_flcma_create>).
                  IF sy-subrc EQ 0.
                    <ls_flcma_create>-tplnr_fl = <fs_csdoc>-tplnr_fl.
                    <ls_flcma_create>-cmnum = 'CITROS'.
                    <ls_flcma_create>-varia = 'MANUT&COLHEITA'.
                    <ls_flcma_create>-season = 'SAFRA PROD'.
                    <ls_flcma_create>-updkz = 'I'.
                    <ls_flcma_create>-astat = 'I'.
                    <ls_flcma_create>-zzprevisto = abap_true.
                    <ls_flcma_create>-datab = lv_begda_manu.
                    <ls_flcma_create>-datbi = lv_endda_manu.
                    <ls_flcma_create>-class = '1'.
                    <ls_flcma_create>-zzdate = <ls_flcma_create>-datbi.
                    <ls_flcma_create>-zzmatnr = <fs_csdoc>-y-cshdr-zzmatnr.
                    <ls_flcma_create>-ymatnr = <fs_csdoc>-y-cshdr-ymatnr.
                    <ls_flcma_create>-zzfazplantio = <fs_csdoc>-y-cshdr-zzfazplantio.
                    <ls_flcma_create>-zzfazvartecnia = <fs_csdoc>-y-cshdr-zzfazvartecnia.
                    <ls_flcma_create>-zzporta_enxerto = <fs_csdoc>-y-cshdr-zzporta_enxerto.
                    <ls_flcma_create>-zzesp_rua = <fs_csdoc>-y-cshdr-zzesp_rua.
                    <ls_flcma_create>-zzesp_pes = <fs_csdoc>-y-cshdr-zzesp_pes.
                    <ls_flcma_create>-zzqtd_plantas = <fs_csdoc>-y-cshdr-zzqtd_plantas.
                    <ls_flcma_create>-zzproc_muda = <fs_csdoc>-y-cshdr-zzproc_muda.
                    <ls_flcma_create>-zzproc_genetica = <fs_csdoc>-y-cshdr-zzproc_genetica.
                    <ls_flcma_create>-zzlarg_copa = <fs_csdoc>-y-cshdr-zzlarg_copa.
                    <ls_flcma_create>-zzaltura_copa = <fs_csdoc>-y-cshdr-zzaltura_copa.
                    <ls_flcma_create>-datab_ref = <fs_csdoc>-y-cshdr-datab_ref.
                    <ls_flcma_create>-zzprev_errad  = lv_prev_errad.
                    <ls_flcma_create>-exhad = lv_begda_manu.
*-- Nº principal do imobilizado
                    CLEAR <ls_flcma_create>-anlnr.
*-- Centro de custo
                    READ TABLE lt_csks INTO DATA(ls_csks)
                      WITH KEY cmnum = ls_glflcma-cmnum
                               varia = 'MANUT&COLHEITA'
                               iwerk = ls_glflcma-iwerk BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      <ls_flcma_create>-kostl = ls_csks-kostl.
                    ENDIF.
                  ENDIF.
                ENDIF.


                SELECT *
                  FROM /agri/glflcma
                  INTO TABLE @DATA(lt_glflcma_del)
                 WHERE tplnr_fl       EQ @<fs_csdoc>-tplnr_fl
                   AND varia          EQ 'FORMAÇÃO'
                   AND loevm          NE 'X'.

                IF sy-subrc = 0.
                  lt_cskey = CORRESPONDING #( lt_glflcma_del ).

                  IF lt_cskey[] IS NOT INITIAL.
                    CALL FUNCTION '/AGRI/GLCS_VIEW'
                      EXPORTING
                        it_cskey       = lt_cskey
                      IMPORTING
                        et_csdoc       = lt_csdoc_del
                      EXCEPTIONS
                        no_data_exists = 1
                        OTHERS         = 2.
                  ENDIF.

                  LOOP AT lt_csdoc_del ASSIGNING FIELD-SYMBOL(<fs_csdoc_del>).
                    <fs_csdoc_del>-x-cshdr-updkz = 'U'.
                    <fs_csdoc_del>-updkz = abap_true.
                    <fs_csdoc_del>-x-cshdr-loevm = abap_true.
                    APPEND <fs_csdoc_del> TO lt_csdoc.
                  ENDLOOP.

                ENDIF.

              ENDIF.

*Amplia epoca de cultura
            ELSE.
              <fs_csdoc>-x-cshdr-datbi = <fs_csdoc>-x-cshdr-zzprev_errad.
*-- Data errad
              <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.
*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
              LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
                <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
                <fs_csprs>-updkz = 'U'.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF lv_canc_prev = abap_true.
*-- Cancelamento de Previsão Erradicação
        IF ls_canc_prev-atwrt EQ 'E'.
          <fs_csdoc>-x-cshdr-updkz = 'U'.
          <fs_csdoc>-updkz = abap_true.
          IF <fs_csdoc>-x-cshdr-astat EQ 'A'.
            CLEAR <fs_csdoc>-x-cshdr-zzprevisto.
          ENDIF.
          CLEAR <fs_csdoc>-x-cshdr-zzprev_errad.
*-- Recálculo Data Final de acordo com /AGRI/GLCMPRS
          READ TABLE lt_cpros INTO ls_cpros
            WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
                     varia = <fs_csdoc>-x-cshdr-varia
                     cpros = c_crop_process-manutencao BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR lwa_duration.
            CASE ls_cpros-miunt.
              WHEN 'TAG'. "DIA
                lwa_duration-durdd = ls_cpros-midur.
              WHEN 'JHR'. "ANS
                lwa_duration-duryy = ls_cpros-midur.
            ENDCASE.

            CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
              EXPORTING
                im_date     = <fs_csdoc>-x-cshdr-datab
                im_operator = '+'
                im_duration = lwa_duration
              IMPORTING
                ex_date     = <fs_csdoc>-x-cshdr-datbi.

*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
            LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
              <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
              <fs_csprs>-updkz = 'U'.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND <fs_csdoc> TO lt_csdoc.
  ENDLOOP.

  IF lt_csdoc[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLCS_SAVE'
      EXPORTING
        i_set_update_task  = abap_true
        i_commit_work      = 'X'
      CHANGING
        ct_csdoc           = lt_csdoc
        ct_messages        = lt_messages
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

        INSERT INITIAL LINE INTO TABLE et_messages
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
    ELSE.
*-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
      READ TABLE lt_csdoc INTO ls_csdoc INDEX 1.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE et_messages
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '358'.
          <ls_message>-msgty = 'S'.
          <ls_message>-msgv1 = lv_tplnr_fl.
          <ls_message>-msgv2 = ls_csdoc-x-cshdr-cmnum.
          <ls_message>-msgv3 = ls_csdoc-x-cshdr-varia.
          <ls_message>-msgv4 = ls_csdoc-x-cshdr-astat.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_flcma_create[] IS NOT INITIAL.
    WAIT UP TO 1 SECONDS.
    REFRESH lt_csdoc.
    CALL FUNCTION '/AGRI/GLCS_CREATE'
      EXPORTING
        it_flcma                = lt_flcma_create
      IMPORTING
        et_csdoc                = lt_csdoc
        et_messages             = lt_messages
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

        INSERT INITIAL LINE INTO TABLE et_messages
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
    ELSE.
      LOOP AT lt_messages INTO DATA(ls_bapi_ret) WHERE msgty <> 'S'.
        IF sy-subrc EQ 0.
          INSERT INITIAL LINE INTO TABLE et_messages
            ASSIGNING <ls_message>.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING ls_bapi_ret TO <ls_message>.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF sy-subrc NE 0.
        lt_cskey = CORRESPONDING #( lt_csdoc ).

        IF lt_cskey[] IS NOT INITIAL.
          CALL FUNCTION '/AGRI/GLCS_VIEW'
            EXPORTING
              it_cskey       = lt_cskey
            IMPORTING
              et_csdoc       = lt_csdoc
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.
        ENDIF.

        LOOP AT lt_csdoc ASSIGNING <fs_csdoc>.
          <fs_csdoc>-x-cshdr-datbi = <fs_csdoc>-x-cshdr-zzprev_errad.
          <fs_csdoc>-x-cshdr-updkz = 'U'.
          <fs_csdoc>-x-cshdr-astat = 'I'.
*-- Data errad
          <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.
*-- Data Final [Manutenção] -> Data Final (Processos de épocas de cultura)
          LOOP AT <fs_csdoc>-x-csprs ASSIGNING <fs_csprs>.
            <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datbi.
            <fs_csprs>-updkz = 'U'.
          ENDLOOP.
        ENDLOOP.


        LOOP AT lt_csdoc_del ASSIGNING <fs_csdoc_del>.
          <fs_csdoc_del>-x-cshdr-updkz = 'U'.
          <fs_csdoc_del>-updkz = abap_true.
          <fs_csdoc_del>-x-cshdr-loevm = abap_false.
          APPEND <fs_csdoc_del> TO lt_csdoc.
        ENDLOOP.


        IF lt_csdoc[] IS NOT INITIAL.
          WAIT UP TO 1 SECONDS.
          CALL FUNCTION '/AGRI/GLCS_SAVE'
            EXPORTING
              i_set_update_task  = abap_true
              i_commit_work      = 'X'
            CHANGING
              ct_csdoc           = lt_csdoc
              ct_messages        = lt_messages
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

              INSERT INITIAL LINE INTO TABLE et_messages
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
          ELSE.
            LOOP AT lt_messages INTO ls_bapi_ret WHERE msgty <> 'S'.
              IF sy-subrc EQ 0.
                INSERT INITIAL LINE INTO TABLE et_messages
                  ASSIGNING <ls_message>.
                IF sy-subrc EQ 0.
                  MOVE-CORRESPONDING ls_bapi_ret TO <ls_message>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
**-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
*      READ TABLE lt_csdoc INTO DATA(ls_csdoc) INDEX 1.
*      IF sy-subrc EQ 0.
*        INSERT INITIAL LINE INTO TABLE et_messages
*          ASSIGNING <ls_message>.
*        IF sy-subrc EQ 0.
*          <ls_message>-msgid = 'ZFMFP'.
*          <ls_message>-msgno = '358'.
*          <ls_message>-msgty = 'S'.
*          <ls_message>-msgv1 = lv_tplnr_fl.
*          <ls_message>-msgv2 = ls_csdoc-x-cshdr-cmnum.
*          <ls_message>-msgv3 = ls_csdoc-x-cshdr-varia.
*          <ls_message>-msgv4 = ls_csdoc-x-cshdr-astat.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
