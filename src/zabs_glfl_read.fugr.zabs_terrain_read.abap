FUNCTION zabs_terrain_read.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_TERRAIN) TYPE  /AGRI/GLTPLNR_FL
*"  EXPORTING
*"     VALUE(ET_TERRAIN) TYPE  /AGRI/T_GLFL_DOC
*"     VALUE(ES_FLHDR) TYPE  /AGRI/S_GLFLOT
*"     VALUE(ES_ADRC) TYPE  /AGRI/S_GLADRC
*"     VALUE(ET_IFLOTX) TYPE  /AGRI/T_GLIFLOTX
*"     VALUE(ET_IHPA) TYPE  /AGRI/T_GLIHPA
*"     VALUE(ET_FLPPL) TYPE  /AGRI/T_GLFLPPL
*"     VALUE(ET_FLATG) TYPE  /AGRI/T_GLFLATG
*"     VALUE(ET_FLATV) TYPE  /AGRI/T_GLFLATV
*"     VALUE(ET_FLCMA) TYPE  /AGRI/T_GLFLCMA
*"     VALUE(ET_FLOWN) TYPE  /AGRI/T_GLFLOWN
*"     VALUE(ET_FLOS) TYPE  /AGRI/T_GLFLOS
*"     VALUE(ET_ADDITIONAL_DATA) TYPE  ZABS_TTY_ADDITIONAL_DATA
*"     VALUE(ET_DOC_MEDICAO) TYPE  ZABS_TTY_DOC_MEDICAO
*"     VALUE(EV_QTDE_PLANTAS) TYPE  MENGE_D
*"----------------------------------------------------------------------

  DATA: lt_tplnr           TYPE /agri/t_gltplnr,
        lt_fl_doc          TYPE /agri/t_glfl_doc,
        lt_cskey           TYPE /agri/t_glcs_key,
        lt_amhdr           TYPE /agri/t_glamhdr,
        lt_ativos          TYPE /agri/t_glamhdr,
        lt_cshdr           TYPE STANDARD TABLE OF /agri/glflcma,
        lt_additional_data TYPE TABLE OF /agri/s_abgl_user_scrfields,
        lt_asset_summary   TYPE /agri/t_glam_summary_fcat,
        lwa_summary_list   TYPE /agri/s_glam_summary_fcat,
        lwa_table_field    TYPE tabfield,
        lr_atinn           TYPE RANGE OF atinn,
        lr_mdtyp           TYPE RANGE OF /agri/glmdtyp,
        lr_mpgrp           TYPE RANGE OF /agri/glmpgrp,
        lv_user_structure  TYPE ddobjname VALUE '/AGRI/S_GLFLOTCA',
        lv_terrain         TYPE /agri/gltplnr_fl,
        lv_fieldname       TYPE fnam_____4,
        lv_cultura         TYPE /agri/glcmnum VALUE 'CITROS',
        lv_tabix           TYPE sytabix,
        lv_msgid           TYPE symsgid,
        lv_msgty           TYPE symsgty,
        lv_msgno           TYPE symsgno,
        lv_msgv1           TYPE symsgv,
        lv_msgv2           TYPE symsgv,
        lv_msgv3           TYPE symsgv,
        lv_msgv4           TYPE symsgv,
        lv_int_value(128)  TYPE c.

*--Additional data
  FIELD-SYMBOLS: <lt_additional_data> TYPE table,
                 <lv_field_value>     TYPE any.

  CONSTANTS: BEGIN OF c_action,
               add    TYPE /agri/glivact VALUE 'A',
               remove TYPE /agri/glivact VALUE 'D',
               reset  TYPE /agri/glivact VALUE 'R',
             END OF c_action.

  lv_terrain = iv_terrain.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = lv_terrain
    IMPORTING
      output     = lv_terrain
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.

  APPEND lv_terrain TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc EQ 0.
    et_terrain[] = lt_fl_doc[].
    READ TABLE lt_fl_doc INTO DATA(lwa_terrain) INDEX 1.
    IF sy-subrc EQ 0.
      DO 4 TIMES.
        DATA(lv_index) = sy-index.
        IF lv_index LE 2.
*-- Tipo de documento de medição
          INSERT INITIAL LINE INTO TABLE lr_mdtyp
            ASSIGNING FIELD-SYMBOL(<lwa_mdtyp>).
          IF sy-subrc EQ 0.
            <lwa_mdtyp> = 'IEQ'.
            CASE lv_index.
              WHEN 1.
                <lwa_mdtyp>-low = 'ZARV'.
              WHEN 2.
                <lwa_mdtyp>-low = 'ZPTA'.
            ENDCASE.
          ENDIF.
        ENDIF.
*-- Grupo de medições
        INSERT INITIAL LINE INTO TABLE lr_mpgrp
          ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
        IF sy-subrc EQ 0.
          <lwa_mpgrp> = 'IEQ'.
          CASE lv_index.
            WHEN 1.
              <lwa_mpgrp>-low = 'TEST_CS_UPDATE'.
            WHEN 2.
              <lwa_mpgrp>-low = 'FAZ-PLANTIO'.
            WHEN 3.
              <lwa_mpgrp>-low = 'CIT-INV-PLANTAS'.
            WHEN 4.
              <lwa_mpgrp>-low = 'FAZ-INV-PLANTAS'.
          ENDCASE.
        ENDIF.
*-- Característica interna
        INSERT INITIAL LINE INTO TABLE lr_atinn
          ASSIGNING FIELD-SYMBOL(<lwa_atinn>).
        IF sy-subrc EQ 0.
          <lwa_atinn> = 'IEQ'.
          CASE lv_index.
            WHEN 1.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'FAZ_VAR_MP_TECNICA'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN 2.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'FAZ_FIM_MP_PLANTIO'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN 3.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-TP-CONTAGEM'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN 4.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'INV-QTDADE-ARV'
                IMPORTING
                  output = <lwa_atinn>-low.
          ENDCASE.
        ENDIF.
      ENDDO.

      SELECT m~mdocm, m~mdtyp, m~aslvl, m~tplnr_fl,
             m~contr, m~cmnum, m~equnr, m~mpgrp,
             m~datab, m~datbi, m~mdate, a~atinn,
             a~atzhl, a~atwrt, a~atflv, c~adzhl,
             t~spras, t~atwtb, t~lkenz
        FROM /agri/glmdhdr AS m
        INNER JOIN /agri/glmdatv AS a
          ON m~mdocm EQ a~mdocm
        LEFT OUTER JOIN cawn AS c
          ON  c~atinn = a~atinn
          AND c~atwrt = a~atwrt
        LEFT OUTER JOIN cawnt AS t
          ON  c~atinn = t~atinn
          AND c~atzhl = t~atzhl
          AND c~adzhl = t~adzhl
        INTO TABLE @DATA(lt_glmdhdr)
       WHERE m~mdtyp  IN @lr_mdtyp[]
         AND tplnr_fl EQ @lv_terrain
         AND m~mpgrp  IN @lr_mpgrp[]
         AND a~atinn  IN @lr_atinn[].
*         AND t~spras  EQ @sy-langu

      SORT lt_glmdhdr BY mdtyp atinn mdate DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_glmdhdr COMPARING mdtyp atinn.
      DELETE lt_glmdhdr WHERE lkenz EQ abap_true.
      SORT lt_glmdhdr BY atinn.

      LOOP AT lt_glmdhdr INTO DATA(lwa_glmdhdr).
        INSERT INITIAL LINE INTO TABLE et_doc_medicao
          ASSIGNING FIELD-SYMBOL(<lwa_doc_medicao>).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_glmdhdr TO <lwa_doc_medicao>.
        ENDIF.
      ENDLOOP.

      SELECT tplnr_fl contr FROM /agri/glflcma
        INTO CORRESPONDING FIELDS OF TABLE lt_cskey
       WHERE tplnr_fl EQ lv_terrain
         AND cmnum    EQ lv_cultura
         AND loevm    EQ space.

      IF sy-subrc EQ 0.
        SELECT ivnum FROM /agri/glamhdr
          INTO CORRESPONDING FIELDS OF TABLE lt_amhdr
           FOR ALL ENTRIES IN lt_cskey             "#EC CI_NO_TRANSFORM
         WHERE tplnr_fl EQ lt_cskey-tplnr_fl
           AND contr    EQ lt_cskey-contr.

        IF sy-subrc EQ 0.
          SELECT * FROM /agri/glamhdr
            INTO CORRESPONDING FIELDS OF TABLE lt_ativos
             FOR ALL ENTRIES IN lt_amhdr
           WHERE ivnum EQ lt_amhdr-ivnum.

          IF sy-subrc EQ 0.
            SELECT * FROM /agri/glflcma
              INTO TABLE lt_cshdr
               FOR ALL ENTRIES IN lt_ativos        "#EC CI_NO_TRANSFORM
             WHERE tplnr_fl EQ lt_ativos-tplnr_fl
               AND contr EQ lt_ativos-contr.

            SORT: lt_ativos[] BY tplnr_fl contr,
                  lt_amhdr[] BY ivdat charg_in.

            LOOP AT lt_ativos INTO DATA(lwa_ativo).
              READ TABLE lt_cshdr INTO DATA(lwa_cshdr)
                WITH KEY tplnr_fl = lwa_ativo-tplnr_fl
                         contr    = lwa_ativo-contr BINARY SEARCH.
              CHECK sy-subrc EQ 0.
              MOVE-CORRESPONDING lwa_ativo TO lwa_summary_list.
              IF lwa_ativo-acode EQ c_action-remove.
                lwa_summary_list-ivdat = lwa_ativo-ivdat_ref.
                lwa_summary_list-menge = -1 * lwa_summary_list-menge.
              ENDIF.
              COLLECT lwa_summary_list INTO lt_asset_summary.
              CLEAR lwa_summary_list.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      DELETE lt_asset_summary WHERE menge IS INITIAL.
      READ TABLE lt_asset_summary INTO lwa_summary_list INDEX 1.
      IF sy-subrc EQ 0.
        ev_qtde_plantas = lwa_summary_list-menge.
      ENDIF.

      es_flhdr    = lwa_terrain-x-flhdr.
      es_adrc     = lwa_terrain-x-adrc.
      et_iflotx[] = lwa_terrain-x-iflotx[].
      et_ihpa[]   = lwa_terrain-x-ihpa[].
      et_flppl[]  = lwa_terrain-x-flppl[].
      et_flatg[]  = lwa_terrain-x-flatg[].
      et_flatv[]  = lwa_terrain-x-flatv[].
      et_flcma[]  = lwa_terrain-x-flcma[].
      et_flown[]  = lwa_terrain-x-flown[].
      et_flos[]   = lwa_terrain-x-flos[].

      REFRESH lr_atinn.
*-- Características
      DO 5 TIMES.
        lv_index = sy-index.
        INSERT INITIAL LINE INTO TABLE lr_atinn ASSIGNING <lwa_atinn>.
        IF sy-subrc EQ 0.
          <lwa_atinn> = 'IEQ'.
          CASE lv_index.
            WHEN '1'.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-TIPO-IM'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN '2'.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-DATA-CAD'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN '3'.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-BIN-PROP'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN '4'.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-TP-CAPBIN'
                IMPORTING
                  output = <lwa_atinn>-low.
            WHEN '5'.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-TP-DIRFOR'
                IMPORTING
                  output = <lwa_atinn>-low.
          ENDCASE.
        ENDIF.
      ENDDO.

      SELECT *
        FROM /agri/glflatv
        INTO TABLE @DATA(lt_flatv)
       WHERE tplnr_fl EQ @es_flhdr-tplnr_fl
         AND atinn IN @lr_atinn[].

      IF sy-subrc EQ 0.
        SELECT c~atinn, c~atzhl, c~adzhl,
               c~atwrt, t~spras, t~atwtb,
               t~datuv, t~lkenz, t~datub
          FROM cawn AS c
          INNER JOIN cawnt AS t
          ON  c~atinn = t~atinn
          AND c~atzhl = t~atzhl
          AND c~adzhl = t~adzhl
          INTO TABLE @DATA(lt_cawn)
          FOR ALL ENTRIES IN @lt_flatv
         WHERE c~atinn IN @lr_atinn[]
           AND c~atwrt EQ @lt_flatv-atwrt
           AND t~spras EQ @sy-langu
           AND t~lkenz EQ @abap_false.

        SORT lt_cawn BY atinn.

        LOOP AT lt_flatv INTO DATA(lwa_flatv).
          READ TABLE lt_cawn INTO DATA(lwa_cawn)
            WITH KEY atinn = lwa_flatv-atinn BINARY SEARCH.
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE et_flatv
              ASSIGNING FIELD-SYMBOL(<lwa_flatv>).
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING: lwa_flatv TO <lwa_flatv>,
                                  lwa_cawn  TO <lwa_flatv>.
            ENDIF.
          ENDIF.
        ENDLOOP.

        SORT et_flatv BY tplnr_fl clint atinn atzhl.
        DELETE ADJACENT DUPLICATES FROM et_flatv COMPARING tplnr_fl clint atinn atzhl.
      ENDIF.

*-- Fornecedores
      IF et_ihpa[] IS NOT INITIAL.
        SELECT lifnr, name1, name2, name3, name4
          FROM lfa1
          INTO TABLE @DATA(lt_lfa1)
          FOR ALL ENTRIES IN @et_ihpa
         WHERE lifnr EQ @et_ihpa-lifnr.

        SORT lt_lfa1 BY lifnr.

        LOOP AT et_ihpa ASSIGNING FIELD-SYMBOL(<lwa_ihpa>).
          READ TABLE lt_lfa1 INTO DATA(lwa_lfa1)
            WITH KEY lifnr = <lwa_ihpa>-lifnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_ihpa>-name   = lwa_lfa1-name1.
            <lwa_ihpa>-name_2 = lwa_lfa1-name2.
            <lwa_ihpa>-name_3 = lwa_lfa1-name3.
            <lwa_ihpa>-name_4 = lwa_lfa1-name4.
          ENDIF.
        ENDLOOP.
      ENDIF.

*-- Dados Adicionais
      PERFORM additional_data_fields_get IN PROGRAM /agri/saplglflm
        USING lv_user_structure IF FOUND.

      ASSIGN ('(/AGRI/SAPLGLFLM)GT_ADDITIONAL_DATA') TO <lt_additional_data>.
      IF sy-subrc EQ 0.
        lt_additional_data[] = <lt_additional_data>[].

        LOOP AT lt_additional_data ASSIGNING FIELD-SYMBOL(<lwa_additional_data>).
          ASSIGN COMPONENT <lwa_additional_data>-fieldname
             OF STRUCTURE lwa_terrain-x-flhdr TO <lv_field_value>.
          IF sy-subrc EQ 0
          AND <lv_field_value> IS NOT INITIAL.
            lwa_table_field-fieldname = <lwa_additional_data>-fieldname.
***Single Field
            lwa_table_field-tabname = <lwa_additional_data>-tabname.

            IF <lwa_additional_data>-fieldtyp EQ 'D'.
              IF <lv_field_value> EQ '00000000'.
                CLEAR <lwa_additional_data>-fieldval.
              ELSE.
                CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                  EXPORTING
                    date_internal            = <lv_field_value>
                  IMPORTING
                    date_external            = <lwa_additional_data>-fieldval
                  EXCEPTIONS
                    date_internal_is_invalid = 1
                    OTHERS                   = 2.
              ENDIF.
            ELSE.
              lv_int_value = <lv_field_value>.
****DS_CONV fm delimts data which has lenght more than 45
              IF <lwa_additional_data>-outputlen LT 46.
                CALL FUNCTION 'RS_DS_CONV_IN_2_EX'
                  EXPORTING
                    input       = lv_int_value
*                   DESCR       =
                    table_field = lwa_table_field
                  IMPORTING
                    output      = <lwa_additional_data>-fieldval.
              ELSE.
                CALL FUNCTION '/AGRI/G_CONVRT_FIELD_TO_EXTERN'
                  EXPORTING
                    i_conve = <lwa_additional_data>-convexit
                    i_feld  = lv_int_value
                  IMPORTING
                    e_feld  = <lwa_additional_data>-fieldval.
****
              ENDIF.
            ENDIF.
            CALL FUNCTION '/AGRI/G_KEYTEXT_GET'
              EXPORTING
                i_tabname             = lwa_table_field-tabname
                i_fieldname           = <lwa_additional_data>-fieldname
                i_fieldvalue          = <lv_field_value>
              IMPORTING
                e_text                = <lwa_additional_data>-fielddscr
              EXCEPTIONS
                ##fm_subrc_ok "#EC *
                inadequate_parameters = 1
                text_not_found        = 2
                OTHERS                = 3 ##fm_subrc_ok.
          ENDIF.
        ENDLOOP.

        et_additional_data[] = lt_additional_data[].
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
