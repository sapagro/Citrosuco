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
* Description       :  Tipo'ZPTA' e Grupo 'FAZ-PLANTIO'                *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
FUNCTION zabs_fm_cs_fields_update.
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
         END OF ly_atributos.

*--Internal table declarations
  DATA: lt_tplnr      TYPE /agri/t_gltplnr,
        lt_fldoc      TYPE /agri/t_glfl_doc,
        lt_clint      TYPE /agri/t_gclint,
        lt_atnam      TYPE /agri/t_gatnam,
        lt_mdclass    TYPE /agri/t_glatgrp,
        lt_messages   TYPE /agri/t_gprolog,
        lt_cskey      TYPE /agri/t_glcs_key,
        lt_csdoc      TYPE /agri/t_glcs_doc,
        lt_csdoc_temp TYPE /agri/t_glcs_doc,
        lt_klah       TYPE tt_klah,
        lt_cs_afn     TYPE /plmb/t_fieldname,

*--Workarea declarations
        lwa_atnam     TYPE /agri/s_gatnam,
        lwa_atributo  TYPE ly_atributos,
        lwa_clint     TYPE /agri/s_gclint,
        lwa_attr_val  TYPE auspdata,
        lwa_flatv     TYPE /agri/s_glflatv,
        lwa_glflcmaca TYPE zabs_str_glflcmaca,
        lwa_cs_afn    TYPE /plmb/s_fieldname,
        lwa_comp      TYPE abap_compdescr,

*--Local Reference declarations
        lref_descr    TYPE REF TO cl_abap_structdescr,
        lref_date     TYPE REF TO data,

*--Local Variables
        lv_tplnr_fl   TYPE /agri/gltplnr_fl,
        lv_data_char  TYPE atwrt,
        lv_datum8     TYPE char8,
        lv_datum      TYPE sy-datum,
        lv_null_date  TYPE sydatum,
        lv_begda      TYPE begda,
        lv_endda      TYPE endda,
        lv_i          TYPE i.

  FIELD-SYMBOLS: <fs_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

  DO 8 TIMES.
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

  DATA(lv_etapa) = 0.

*-- Primeira etapa
  SELECT *
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
   WHERE tplnr_fl   EQ @is_mdhdr-tplnr_fl
     AND contr      EQ @is_mdhdr-contr
     AND cmnum      EQ @is_mdhdr-cmnum
     AND zzprevisto EQ @abap_true.

*-- Data de Previsão de Plantio está preenchida.
  DELETE lt_glflcma WHERE zzfazplantio NE lv_null_date
                       OR zzprev_plantio IS INITIAL.
*===================================================================*
*-- É segunda etapa?
  IF lt_glflcma[] IS INITIAL.
    SELECT *
      FROM /agri/glflcma
      INTO TABLE @lt_glflcma
     WHERE tplnr_fl   EQ @is_mdhdr-tplnr_fl
       AND contr      EQ @is_mdhdr-contr
       AND cmnum      EQ @is_mdhdr-cmnum
       AND zzprevisto EQ @abap_false.

*-- Data de Plantio está preenchida.
    DELETE lt_glflcma WHERE zzfazplantio IS INITIAL
                         OR zzprev_plantio NE lv_null_date.

    IF lt_glflcma[] IS NOT INITIAL.
      lv_etapa = 2.

      SELECT *
        FROM /agri/glflcma
        INTO TABLE @DATA(lt_glflcma_aux)
        FOR ALL ENTRIES   IN @lt_glflcma
       WHERE tplnr_fl     EQ @lt_glflcma-tplnr_fl
         AND cmnum        EQ @is_mdhdr-cmnum
         AND zzfazplantio EQ @lt_glflcma-zzfazplantio.

      lt_glflcma[] = lt_glflcma_aux[].
    ENDIF.
  ELSE.
    lv_etapa = 1.
  ENDIF.
*===================================================================*

  lt_cskey = CORRESPONDING #( lt_glflcma ).

  IF lt_cskey[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
*     I_MODE         = 'V'
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

  LOOP AT lt_csdoc_temp INTO DATA(lwa_csdoc_temp).
    IF lwa_csdoc_temp-x-cshdr-datab LE is_mdhdr-mdate
    AND lwa_csdoc_temp-x-cshdr-datbi GE is_mdhdr-mdate.
      DATA(lv_contr) = lwa_csdoc_temp-contr.

      UNASSIGN <fs_csdoc>.
      ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
      IF <fs_csdoc> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      LOOP AT lt_md_attr_map INTO DATA(lwa_md_attr_map).
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

        ASSIGN COMPONENT lwa_md_attr_map-fieldname OF STRUCTURE <fs_csdoc>-x-cshdr TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = lwa_mdatv-atwrt.
          <fs_csdoc>-x-cshdr-updkz = 'U'.
          <fs_csdoc>-updkz = abap_true.

          IF lwa_md_attr_map-fieldname EQ 'ZZAREA_TALHAO'
          AND lwa_md_attr_map-mdatnam EQ 'FAZ_AREA_TALHAO'
          AND <fs_value> IS NOT INITIAL.
            <fs_csdoc>-x-cshdr-aarea = <fs_value>.
            <fs_csdoc>-x-cshdr-garea = <fs_value>.
          ENDIF.

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
      APPEND <fs_csdoc> TO lt_csdoc.
    ENDIF.
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

ENDFUNCTION.
