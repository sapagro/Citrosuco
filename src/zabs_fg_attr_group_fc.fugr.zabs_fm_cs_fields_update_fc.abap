FUNCTION zabs_fm_cs_fields_update_fc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_MDHDR) TYPE  /AGRI/S_GLMDHDR
*"     REFERENCE(IT_MDITM) TYPE  /AGRI/T_GLMDITM
*"     REFERENCE(IT_MDATV) TYPE  /AGRI/T_GLMDATV
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_CS_ADD_FIELDS_UPD                       *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  The attribute values from measurement document  *
*                      workbench gets updated in the crop season       *
*                      based on custom table.                          *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
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
        lwa_atributo  TYPE ly_atributos,
        lwa_atnam     TYPE /agri/s_gatnam,
        lwa_clint     TYPE /agri/s_gclint,
        lwa_attr_val  TYPE auspdata,
        lwa_flatv     TYPE /agri/s_glflatv,
        lwa_glflcmaca TYPE zabs_str_glflcmaca,
        lwa_cs_afn    TYPE /plmb/s_fieldname,
        lwa_comp      TYPE abap_compdescr,

*--Local Variables
        lv_tplnr_fl   TYPE /agri/gltplnr_fl,

*--Local Reference declarations
        lref_descr    TYPE REF TO cl_abap_structdescr,
        lref_date     TYPE REF TO data.

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

  SELECT *
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
   WHERE tplnr_fl EQ @is_mdhdr-tplnr_fl
*     AND contr    EQ @is_mdhdr-contr
     AND cmnum    EQ 'CITROS'
     AND varia    EQ 'FORMAÇÃO'
     AND class    EQ '1'
     AND astat    EQ 'A'
     AND loevm    EQ @space.

  IF lt_glflcma[] IS INITIAL.
    RETURN.
  ENDIF.

  lt_cskey = CORRESPONDING #( lt_glflcma ).

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
        ENDIF.
      ENDLOOP.
      APPEND <fs_csdoc> TO lt_csdoc.

    ENDIF.

    READ TABLE lt_csdoc_temp INTO DATA(lwa_csdoc)
      WITH KEY x-cshdr-contr_ref = lv_contr.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    UNASSIGN <fs_csdoc>.
    ASSIGN lwa_csdoc TO <fs_csdoc>.

    lv_contr = lwa_csdoc-x-cshdr-contr.
    CLEAR lwa_md_attr_map.
    LOOP AT lt_md_attr_map INTO lwa_md_attr_map.
      READ TABLE lt_athdr INTO ls_athdr
        WITH KEY atnam = lwa_md_attr_map-mdatnam BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_mdatv INTO lwa_mdatv
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
      ENDIF.
    ENDLOOP.

    APPEND <fs_csdoc> TO lt_csdoc.

  ENDLOOP.

  IF lt_csdoc IS NOT INITIAL.

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
