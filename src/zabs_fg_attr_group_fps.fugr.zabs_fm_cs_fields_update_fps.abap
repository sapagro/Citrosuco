FUNCTION zabs_fm_cs_fields_update_fps.
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
           atividade            TYPE atinn,
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
        lt_fmfphdr      TYPE STANDARD TABLE OF ty_fmfphdr INITIAL SIZE 0,
        lt_cpros        TYPE STANDARD TABLE OF ty_cpros INITIAL SIZE 0,
        lt_md_attr_map  TYPE STANDARD TABLE OF zabst_md_atr_map INITIAL SIZE 0,
        lt_message      TYPE /agri/t_gprolog,
        lt_fpdoc_timp   TYPE /agri/t_fmfp_doc,
        lt_datas        TYPE STANDARD TABLE OF ly_datas,
        lt_fldoc        TYPE /agri/t_glfl_doc,
        lt_clint        TYPE /agri/t_gclint,
        lt_atnam        TYPE /agri/t_gatnam,
        lt_mdclass      TYPE /agri/t_glatgrp,
        lt_messages     TYPE /agri/t_gprolog,
        lt_cskey        TYPE /agri/t_glcs_key,
        lt_csdoc_change TYPE /agri/t_glcs_doc,
        lt_csdoc_temp   TYPE /agri/t_glcs_doc,
        lt_klah         TYPE tt_klah,
        lt_cs_afn       TYPE /plmb/t_fieldname,

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
        lv_subrc        TYPE sysubrc,
        lv_datum8       TYPE char8,
        lv_datum        TYPE sy-datum,
        lv_begda        TYPE begda,
        lv_endda        TYPE endda,
        lv_gyear        TYPE char4,
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
            input  = 'INV-QTDADE-ARV'
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
*-- Safra
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-SAFRA'
          IMPORTING
            output = lwa_atributo-safra.
*-- Atividade Inventário
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'INV-ATIVIDADE'
          IMPORTING
            output = lwa_atributo-atividade.
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

  DO 2 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lt_md_attr_map
      ASSIGNING FIELD-SYMBOL(<ls_md_attr_map>).
    IF sy-subrc EQ 0.
      CASE lv_index.
        WHEN 1.
          <ls_md_attr_map>-mdclass = 'FAZ-PREPARO-SOLO'.
          <ls_md_attr_map>-mdatnam = 'FAZ_FIM_PREP_SOLO'.
          <ls_md_attr_map>-fieldname = 'DATAB'.
        WHEN 2.
          <ls_md_attr_map>-mdclass = 'FAZ-PREPARO-SOLO'.
          <ls_md_attr_map>-mdatnam = 'FAZ_IMOBILIZADO'.
          <ls_md_attr_map>-fieldname = 'ANLNR'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SORT lt_md_attr_map BY mdclass mdatnam.

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

  SELECT *
    FROM /agri/glseason
    INTO TABLE @DATA(lt_season).

  SELECT *
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
   WHERE tplnr_fl   EQ @is_mdhdr-tplnr_fl
     AND cmnum      EQ 'CITROS'
     AND varia      EQ @c_crop_season-formacao
     AND season     EQ 'SAFRA PLAN'
     AND astat      EQ 'I'.

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
      INTO TABLE @lt_cpros
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

  LOOP AT lt_csdoc_temp INTO DATA(lwa_csdoc_temp).
    DATA(lv_contr) = lwa_csdoc_temp-contr.

    UNASSIGN <fs_csdoc>.
    ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
    IF <fs_csdoc> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    READ TABLE lt_glflcma INTO DATA(ls_glflcma)
      WITH KEY tplnr_fl = lwa_csdoc_temp-tplnr_fl
               contr    = lwa_csdoc_temp-contr.
    IF sy-subrc NE 0.
      CLEAR ls_glflcma.
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

      ASSIGN COMPONENT lwa_md_attr_map-fieldname
        OF STRUCTURE <fs_csdoc>-x-cshdr TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = lwa_mdatv-atwrt.
        <fs_csdoc>-x-cshdr-updkz = 'U'.
        <fs_csdoc>-updkz = abap_true.
      ENDIF.

      IF lwa_md_attr_map-fieldname EQ 'DATAB'.
        <fs_csdoc>-x-cshdr-datab = <fs_csdoc>-x-cshdr-datab(6) && '01'.

*-- Recálculo Data Final de acordo com /AGRI/GLCMPRS
        READ TABLE lt_cpros INTO DATA(ls_season_form)
          WITH KEY cmnum = <fs_csdoc>-x-cshdr-cmnum
                   varia = <fs_csdoc>-x-cshdr-varia
                   cpros = c_crop_process-formacao BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR lwa_duration.
          CASE ls_season_form-miunt.
            WHEN 'TAG'. "DIA
              lwa_duration-durdd = ls_season_form-midur.
            WHEN 'JHR'. "ANS
              lwa_duration-duryy = ls_season_form-midur.
          ENDCASE.

          CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
            EXPORTING
              im_date     = <fs_csdoc>-x-cshdr-datab
              im_operator = '+'
              im_duration = lwa_duration
            IMPORTING
              ex_date     = <fs_csdoc>-x-cshdr-datbi.

          <fs_csdoc>-x-cshdr-exhad = <fs_csdoc>-x-cshdr-datab.

*-- Data Final [FORMAÇÃO] -> Data Final (Processos de épocas de cultura)
          LOOP AT <fs_csdoc>-x-csprs ASSIGNING FIELD-SYMBOL(<fs_csprs>).
            <fs_csprs>-strdt = <fs_csdoc>-x-cshdr-datab.
            <fs_csprs>-enddt = <fs_csdoc>-x-cshdr-datab.
            READ TABLE lt_cpros INTO DATA(ls_cpros)
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
      ENDIF.
    ENDLOOP.
    <fs_csdoc>-x-cshdr-astat = 'A'.
    <fs_csdoc>-x-cshdr-updkz = 'U'.
    APPEND <fs_csdoc> TO lt_csdoc_change.
*-------------------------------------------------------------------*
* Save crop season changes
*-------------------------------------------------------------------*
    DATA(lv_start_date) = <fs_csdoc>-x-cshdr-datab.
    PERFORM save_csdoc_changes USING lv_tplnr_fl
                            CHANGING lt_csdoc_change
                                     lt_message
                                     lv_subrc.
*-------------------------------------------------------------------*
* Create new Process Order for FORMAÇÃO
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: IMPLANT
*-------------------------------------------------------------------*
    PERFORM create_process_order USING lt_fmfphdr
                                       lt_cpros
                                       ls_glflcma
                                       c_crop_process-implantacao
                                       lv_tplnr_fl
                                       lv_start_date
                              CHANGING lt_fpdoc_timp
                                       lt_message.
*-------------------------------------------------------------------*
* Create new Task Order for TFORGEN
* CMNUM: CITROS / VARIA: FORMAÇÃO / CPROS: IMPLANT
*-------------------------------------------------------------------*
    READ TABLE lt_fpdoc_timp INTO DATA(ls_fpdoc_timp) INDEX 1.
    IF sy-subrc EQ 0.
      PERFORM document_infocus_lock USING ls_fpdoc_timp-aufnr
                                 CHANGING lt_message
                                          lv_subrc.
      IF lv_subrc EQ 0.
        PERFORM create_task_order USING lv_tplnr_fl
                                        'TFORGEN'
                                        lv_start_date
                               CHANGING lt_fpdoc_timp
                                        lt_message.

        PERFORM document_infocus_unlock USING ls_fpdoc_timp-aufnr
                                              ls_fpdoc_timp-x-fphdr-rsnum.
      ENDIF.
    ENDIF.
  ENDLOOP.

  et_messages[] = lt_message[].

ENDFUNCTION.
