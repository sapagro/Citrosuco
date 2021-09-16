FUNCTION zabs_fm_encerrar_colheita.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_MDHDR) TYPE  /AGRI/S_GLMDHDR
*"     REFERENCE(CT_MDITM) TYPE  /AGRI/T_GLMDITM
*"     REFERENCE(CT_MDATV) TYPE  /AGRI/T_GLMDATV
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ly_atributos,
           safra          TYPE atinn,
           data_encerra   TYPE atinn,
           estimativa     TYPE atinn,
           cxpl_estimada  TYPE atinn,
           prod_real      TYPE atinn,
           cxpl_real      TYPE atinn,
           cod_imovel     TYPE atinn,
           talhao         TYPE atinn,
           florada        TYPE atinn,
           estimativa_ini TYPE atinn,
           estimativa_fim TYPE atinn,
           variacao_perc  TYPE atinn,
           cit_quimicos   TYPE atinn,
           dias_carencia  TYPE atinn,
           data_aplicacao TYPE atinn,
         END OF ly_atributos,

         BEGIN OF ly_ausp,
           objek TYPE cuobn,
           atinn TYPE atinn,
           atzhl TYPE wzaehl,
           mafid TYPE klmaf,
           klart TYPE klassenart,
           adzhl TYPE adzhl,
           atwrt TYPE atwrt,
           atflv TYPE atflv,
         END OF ly_ausp,

         BEGIN OF ly_matdoc,
*--BOC-T_T.KONNO-07.22.21
           key1  TYPE matdoc-key1,
           key2  TYPE matdoc-key2,
           key3  TYPE matdoc-key3,
           key4  TYPE matdoc-key4,
           key5  TYPE matdoc-key5,
           key6  TYPE matdoc-key6,
*--EOC-T_T.KONNO-07.22.21
           werks TYPE werks_d,
           menge TYPE menge_d,
           budat TYPE budat,
           bwart TYPE bwart,
           matnr TYPE matnr,
           charg TYPE charg_d,
           lifnr TYPE elifn,
           atwrt TYPE atwrt,
         END OF ly_matdoc,

         BEGIN OF ly_mch1,
           matnr    TYPE matnr,
           charg    TYPE charg_d,
           cuobj_bm TYPE cuobj_bm,
           objek    TYPE cuobn,
         END OF ly_mch1.

  DATA: lt_cskey           TYPE /agri/t_glcs_key,
        lt_fl_doc          TYPE /agri/t_glfl_doc,
        lt_tplnr           TYPE /agri/t_gltplnr,
        lt_amhdr           TYPE /agri/t_glamhdr,
        lt_ativos          TYPE /agri/t_glamhdr,
        lt_ausp            TYPE STANDARD TABLE OF ly_ausp INITIAL SIZE 0,
        lrt_objek          TYPE RANGE OF cuobn,
        lrs_objek          LIKE LINE OF lrt_objek,
        lt_cshdr           TYPE STANDARD TABLE OF /agri/glflcma,
        lt_mch1            TYPE STANDARD TABLE OF ly_mch1 INITIAL SIZE 0,
        lt_matdoc          TYPE STANDARD TABLE OF ly_matdoc INITIAL SIZE 0,
        lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        lwa_atributo       TYPE ly_atributos,
        lv_menge_x         TYPE menge_d,
        lv_cod_imovel      TYPE /agri/gltplnr_fl,
        lv_imovel_atwrt    TYPE atwrt,
        lv_talhao_atwrt    TYPE atwrt,
        lv_cod_imovel_in   TYPE /agri/gltplnr_fl,
        lv_talhao          TYPE /agri/gltplnr_fl,
        lv_talhao_in       TYPE /agri/gltplnr_fl,
        lwa_attributes     TYPE zabs_str_attr_forml,
        lwa_summary_list   TYPE /agri/s_glam_summary_fcat,
        lt_asset_summary   TYPE /agri/t_glam_summary_fcat,
        lt_attributes      TYPE zabs_tty_attr_forml,
        lr_mpgrp           TYPE RANGE OF /agri/glmpgrp,
        lr_cuobj           TYPE RANGE OF cuobj_bm,
        lr_werks           TYPE RANGE OF werks_d,
        lwa_werks          LIKE LINE OF lr_werks,
        lr_bwart           TYPE RANGE OF bwart,
*--BOC-T_T.KONNO-07.15.21
        lr_yaufnr          TYPE RANGE OF /agri/glyaufnr,
*--EOC-T_T.KONNO-07.15.21
        lwa_bwart          LIKE LINE OF lr_bwart,
        lr_budat           TYPE RANGE OF budat,
        lwa_budat          LIKE LINE OF lr_budat,
        lr_matnr           TYPE RANGE OF matnr,
        lr_batch           TYPE RANGE OF atnam,
        lr_cx_liq          TYPE RANGE OF atnam,
        lr_imovel          TYPE RANGE OF atnam,
        lr_atnam           TYPE RANGE OF atnam,
        lr_atinn           TYPE RANGE OF atinn,
        lr_atinn_imovel    TYPE RANGE OF atinn,
        lr_atinn_cx_liq    TYPE RANGE OF atinn,
        lr_atinn_batch     TYPE RANGE OF atinn,
        lwa_atinn          LIKE LINE OF lr_atinn,
        lwa_matnr          LIKE LINE OF lr_matnr,
        lv_terreno         TYPE /agri/gltplnr_fl,
        lv_data_temp       TYPE i,
        lv_data_encerra    TYPE sydatum,
        lv_cod_imov_f      TYPE atflv,
        lv_talhao_f        TYPE atflv,
        lv_sum_menge       TYPE menge_d,
        lv_estimativa_x    TYPE menge_d,
        lv_var_perc        TYPE menge_d,
        lv_var_qtd         TYPE menge_d,
        lv_libera_colheita TYPE atinn,
        lv_menge           TYPE menge_d,
        lv_menge_char      TYPE char16,
        lv_data_apl_char   TYPE char8,
        lv_data_char       TYPE char8,
        lv_safra_char      TYPE char4,
        lv_caixa_char      TYPE char20,
        lv_data_apl_datum  TYPE sydatum,
        lv_data_datum      TYPE sydatum,
        lv_data_fim        TYPE sydatum,
        lv_data_inicio     TYPE sydatum,
        lv_i               TYPE i,
        lv_dias_carencia   TYPE i,
        lv_gyear           TYPE /agri/gyear,
        lv_gyear_tela      TYPE /agri/gyear,
        lv_matnr_in        TYPE matnr,
        lv_matnr           TYPE cuobn,
        lv_subrc           TYPE sysubrc,
        lv_message         TYPE char80,
        lv_safra_float     TYPE atflv,
        lv_safra           TYPE atinn,
        lv_atinn_imovel    TYPE atinn,
        lv_atinn_talhao    TYPE atinn,
        lv_atinn_data      TYPE atinn,
        lv_cod_imovel_flt  TYPE atflv,
        lv_cod_talhao_flt  TYPE atflv,
        lv_cod_talhao_char TYPE atwrt,
        lv_florada_1       TYPE atwrt VALUE '1',
        lv_florada_2       TYPE atwrt VALUE '2',
        lv_cod_imovel_out  TYPE /agri/gltplnr_fl,
        lv_value           TYPE atflv.

  TYPES: BEGIN OF ty_notes_buffer,
           objtyp    TYPE tojtb-name,
           objkey    TYPE /agri/gnotes-object,
           subobject TYPE /agri/gnotes-subobject,
           posnr     TYPE numc06,
           notes     TYPE /agri/t_gnote,
           subscreen TYPE sy-dynnr,
           changed   TYPE abap_bool,
         END OF ty_notes_buffer,

         tt_notes_buffer TYPE TABLE OF ty_notes_buffer.

  FIELD-SYMBOLS: <lwa_mddoc_infocus>   TYPE /agri/s_glmd_doc,
                 <lt_std_notes_buffer> TYPE tt_notes_buffer,
                 <lt_cst_notes_buffer> TYPE tt_notes_buffer.

  CONSTANTS: BEGIN OF lc_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF lc_tipo_safra,

             BEGIN OF lc_propriedade,
               proprio   TYPE /agri/glownshp VALUE 'OW',
               terceiros TYPE /agri/glownshp VALUE 'TP',
             END OF lc_propriedade,

             BEGIN OF c_action,
               add    TYPE /agri/glivact VALUE 'A',
               remove TYPE /agri/glivact VALUE 'D',
               reset  TYPE /agri/glivact VALUE 'R',
             END OF c_action,

             lc_cultura     TYPE /agri/glcmnum VALUE 'CITROS',
             lc_cl_material TYPE klassenart VALUE '001'.

  IF ct_mdatv[] IS INITIAL.
    RETURN.
  ENDIF.

  DO 15 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-SAFRA'
          IMPORTING
            output = lwa_atributo-safra.
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-ENCERRA'
          IMPORTING
            output = lwa_atributo-data_encerra.
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESTIMATIVA'
          IMPORTING
            output = lwa_atributo-estimativa.
      WHEN 4.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-EST'
          IMPORTING
            output = lwa_atributo-cxpl_estimada.
      WHEN 5.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-PROD-REAL'
          IMPORTING
            output = lwa_atributo-prod_real.
      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-REAL'
          IMPORTING
            output = lwa_atributo-cxpl_real.
      WHEN 7.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_COD_IM'
          IMPORTING
            output = lwa_atributo-cod_imovel.
      WHEN 8.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_TALH'
          IMPORTING
            output = lwa_atributo-talhao.
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-FLORADA'
          IMPORTING
            output = lwa_atributo-florada.
      WHEN 10.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-INICIAL'
          IMPORTING
            output = lwa_atributo-estimativa_ini.
      WHEN 11.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-FINAL'
          IMPORTING
            output = lwa_atributo-estimativa_fim.
      WHEN 12.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-VAR-PERC'
          IMPORTING
            output = lwa_atributo-variacao_perc.
      WHEN 13.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-QUIMICOS'
          IMPORTING
            output = lwa_atributo-cit_quimicos.
      WHEN 14.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_DIAS_CARENCIA'
          IMPORTING
            output = lwa_atributo-dias_carencia.
      WHEN 15.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-APLIC'
          IMPORTING
            output = lwa_atributo-data_aplicacao.
    ENDCASE.
  ENDDO.

  APPEND INITIAL LINE TO lt_tplnr
   ASSIGNING FIELD-SYMBOL(<lwa_tplnr>).
  IF sy-subrc EQ 0.
    <lwa_tplnr> = cs_mdhdr-tplnr_fl.
  ENDIF.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  READ TABLE lt_fl_doc INTO DATA(lwa_terrain) INDEX 1.
  IF sy-subrc EQ 0.
    IF lwa_terrain-x-flhdr-ownshp EQ lc_propriedade-proprio.
      DATA(lv_proprio) = abap_true.
    ELSEIF lc_propriedade-terceiros EQ lc_propriedade-terceiros.
      lv_proprio = abap_false.
    ENDIF.
  ENDIF.

  DO 4 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_mpgrp
      ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
    IF sy-subrc EQ 0.
      <lwa_mpgrp> = 'IEQ'.
      CASE lv_index.
        WHEN '1'.
          <lwa_mpgrp>-low = 'CIT-ESTIMATIVA'.
        WHEN '2'.
          <lwa_mpgrp>-low = 'FAZ-ESTIMATIVA'.
        WHEN '3'.
          <lwa_mpgrp>-low = 'ENCERRAR_FLORADA'.
        WHEN '4'.
          <lwa_mpgrp>-low = 'ESTIMATIVA_SAFRA'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
         h~contr, h~cmnum, h~equnr, h~mpgrp,
         h~mdate, h~mtime, v~atzhl, v~atwrt, v~atflv,
         c~atinn, q~value AS atinn_out, c~atnam
    INTO TABLE @DATA(lt_glmdhdr_join)
    FROM /agri/glmdhdr AS h
    INNER JOIN /agri/glmdatv AS v
    ON v~mdocm EQ h~mdocm
    INNER JOIN cabn AS c
    ON c~atinn EQ v~atinn
    LEFT OUTER JOIN /agri/fm_qmsmpl AS q
    ON q~datum EQ c~adatu "creates field atinn_out char40
   WHERE h~mdtyp    EQ 'ZPTA'
     AND h~tplnr_fl EQ @cs_mdhdr-tplnr_fl
     AND h~mpgrp    IN @lr_mpgrp
     AND h~kfrst    EQ @/agri/cl_fmpr_process_ticket=>mc_kfrst
     AND h~canceled EQ @abap_false.

  LOOP AT lt_glmdhdr_join ASSIGNING FIELD-SYMBOL(<lwa_glmdhdr>).
    IF <lwa_glmdhdr>-atinn IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          input  = <lwa_glmdhdr>-atinn
        IMPORTING
          output = <lwa_glmdhdr>-atinn_out.
    ENDIF.
  ENDLOOP.

*-- Data de Liberação de Carência
  READ TABLE ct_mdatv INTO DATA(lwa_safra_tela)
      WITH KEY atinn = lwa_atributo-safra.
  IF sy-subrc EQ 0.
    IF lwa_safra_tela-atwrt IS INITIAL.
*-- Preencher o campo Safra!
      MESSAGE e051(zfmfp) DISPLAY LIKE 'I'.
    ELSE.
      LOOP AT lt_glmdhdr_join INTO DATA(lwa_cit_safra)
        WHERE atinn_out = 'CIT-SAFRA'.
        IF lwa_cit_safra-atwrt NE lwa_safra_tela-atwrt.
          DELETE lt_glmdhdr_join WHERE mdocm EQ lwa_cit_safra-mdocm.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
*-- Preencher o campo Safra!
    MESSAGE e051(zfmfp) DISPLAY LIKE 'I'.
  ENDIF.

  DATA(lt_florada) = lt_glmdhdr_join[].
  DELETE lt_florada WHERE mpgrp NE 'FAZ-ESTIMATIVA'.
  DELETE lt_florada WHERE atinn_out NE 'CIT-FLORADA'.
  SORT lt_florada BY atwrt ASCENDING
                     mdocm DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_florada
    COMPARING atwrt.

  DATA(lt_estimativa) = lt_glmdhdr_join[].
  IF lv_proprio EQ abap_true.
    DELETE lt_estimativa WHERE mpgrp NE 'FAZ-ESTIMATIVA'.
  ELSE.
    DELETE lt_estimativa WHERE mpgrp NE 'CIT-ESTIMATIVA'.
  ENDIF.

  DELETE lt_estimativa WHERE atinn_out NE 'CIT-ESTIMATIVA'.

  LOOP AT lt_estimativa INTO DATA(ls_estimativa).
    READ TABLE lt_florada INTO DATA(ls_florada)
      WITH KEY mdocm = ls_estimativa-mdocm.
    IF sy-subrc NE 0.
      DELETE lt_estimativa WHERE mdocm = ls_estimativa-mdocm.
    ELSE.
      lv_estimativa_x = lv_estimativa_x + ls_estimativa-atflv.
    ENDIF.
  ENDLOOP.
*--EOC- T_T.KONNO-05.27.21

  SORT lt_glmdhdr_join BY mdocm DESCENDING mpgrp atinn_out atflv atwrt.

  REFRESH: lr_mpgrp, lt_glmdhdr_join.
  CLEAR: lv_index, lwa_safra_tela, lwa_cit_safra,
         lv_sum_menge, lv_menge.
  UNASSIGN: <lwa_mpgrp>, <lwa_glmdhdr>.

  READ TABLE ct_mdatv INTO lwa_safra_tela
      WITH KEY atinn = lwa_atributo-safra.
  IF sy-subrc EQ 0.
    SELECT *
      FROM zfmfpsafras
      INTO TABLE @DATA(lt_zfmfpsafras)
     WHERE ano_safra = @lwa_safra_tela-atwrt(4).

    READ TABLE lt_zfmfpsafras INTO DATA(ls_safra_tecnica)
      WITH KEY tipo_safra = 'T'.
    IF sy-subrc EQ 0.
      DATA(ls_zfmfpsafra) = ls_safra_tecnica.
    ENDIF.
  ENDIF.

  SELECT *
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma_x)
   WHERE tplnr_fl EQ @cs_mdhdr-tplnr_fl
     AND cmnum    EQ @lc_cultura
*--BOC-T_T.KONNO-07.15.21
*     AND astat    EQ 'A'
     AND astat    IN ('A','C')
*--EOC-T_T.KONNO-07.15.21
     AND loevm    EQ @space.

  DELETE lt_glflcma_x WHERE yaufnr IS INITIAL.
  IF lt_glflcma_x[] IS NOT INITIAL.
    READ TABLE lt_fl_doc INTO lwa_terrain INDEX 1.
    IF sy-subrc EQ 0.
      IF lv_proprio EQ abap_true.
*--BOC-T_T.KONNO-07.15.21
*        DELETE lt_glflcma_x WHERE contr NE cs_mdhdr-contr.
        lr_yaufnr = CORRESPONDING #( lt_glflcma_x MAPPING low = aufnr ).
*--EOC-T_T.KONNO-07.15.21
      ELSE.
        READ TABLE ct_mdatv INTO lwa_safra_tela
            WITH KEY atinn = lwa_atributo-safra.
        IF sy-subrc EQ 0.
          IF lwa_safra_tela-atwrt IS NOT INITIAL.
            LOOP AT lt_glflcma_x INTO DATA(lwa_glflcma_x).
              DATA(lv_tabix_x) = sy-tabix.
              lv_gyear = lwa_glflcma_x-datab(4).
              lv_gyear_tela = lwa_safra_tela-atwrt(4).
              IF lv_gyear NE lwa_safra_tela-atwrt.
                DELETE lt_glflcma_x INDEX lv_tabix_x.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      SORT lt_glflcma_x BY datab DESCENDING
                           datbi DESCENDING.

*-- Data de Lançamento
      READ TABLE lt_glflcma_x INTO lwa_glflcma_x INDEX 1.
      IF sy-subrc EQ 0.
*        READ TABLE ct_mdatv INTO DATA(lwa_florada_tela)
*          WITH KEY atinn = lwa_atributo-florada.
*        IF sy-subrc EQ 0.
*          IF lwa_florada_tela-atwrt EQ '1'.
        CLEAR lv_data_char.
        READ TABLE ct_mdatv INTO DATA(lwa_data_tela)
          WITH KEY atinn = lwa_atributo-data_encerra.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
            EXPORTING
              float_imp  = lwa_data_tela-atflv
              format_imp = lv_i
            IMPORTING
              char_exp   = lv_data_char.

          lv_data_temp = lwa_data_tela-atflv.
          WRITE lv_data_temp TO lv_data_encerra.
          INSERT INITIAL LINE INTO TABLE lr_budat ASSIGNING FIELD-SYMBOL(<lwa_budat>).
          IF sy-subrc EQ 0.
            <lwa_budat> = 'IBT'.
            <lwa_budat>-low = ls_safra_tecnica-inicio_safra.
            <lwa_budat>-high = lv_data_encerra.
          ENDIF.
        ENDIF.

*-- Tipo de Movimento
        SELECT bwart AS low
          INTO CORRESPONDING FIELDS OF TABLE @lr_bwart
          FROM t156
         WHERE bwart IN ('101','102').

        IF sy-subrc EQ 0.
          lwa_bwart = 'IEQ'.
          MODIFY lr_bwart FROM lwa_bwart
            TRANSPORTING sign option WHERE low <> ''.

*--BOC-T_T.KONNO-07.15.21
*          SELECT werks, menge, budat, bwart,
*                 matnr, charg, lifnr
*            FROM matdoc
*            INTO TABLE @lt_matdoc
*           WHERE werks EQ @lwa_terrain-x-flhdr-swerk
*             AND budat IN @lr_budat[]
*             AND aufnr EQ @lwa_glflcma_x-yaufnr
*             AND bwart IN @lr_bwart[]
*             AND matnr EQ @lwa_glflcma_x-ymatnr.
*
*          LOOP AT lt_matdoc INTO DATA(lwa_matdoc_x).
*            ADD lwa_matdoc_x-menge TO lv_menge_x.
*          ENDLOOP.
          IF lv_proprio EQ abap_true.
            IF lt_glflcma_x[] IS NOT INITIAL.
*--BOC-T_T.KONNO-07.22.21
*             SELECT werks, menge, budat, bwart,
*                    matnr, charg, lifnr
              SELECT key1, key2, key3, key4, key5, key6,
                     werks, menge, budat, bwart, matnr,
                     charg, lifnr
*--EOC-T_T.KONNO-07.22.21
                FROM matdoc
                INTO TABLE @lt_matdoc
                FOR ALL ENTRIES IN @lt_glflcma_x
               WHERE werks EQ @lwa_terrain-x-flhdr-swerk
                 AND budat IN @lr_budat[]
                 AND aufnr EQ @lt_glflcma_x-yaufnr
                 AND bwart IN @lr_bwart[]
                 AND matnr EQ @lt_glflcma_x-ymatnr.
            ENDIF.
          ELSE.
*--BOC-T_T.KONNO-07.22.21
*             SELECT werks, menge, budat, bwart,
*                    matnr, charg, lifnr
             SELECT key1, key2, key3, key4, key5, key6,
                    werks, menge, budat, bwart, matnr,
                    charg, lifnr
*--EOC-T_T.KONNO-07.22.21
              FROM matdoc
              INTO TABLE @lt_matdoc
             WHERE werks EQ @lwa_terrain-x-flhdr-swerk
               AND budat IN @lr_budat[]
               AND aufnr EQ @lwa_glflcma_x-yaufnr
               AND bwart IN @lr_bwart[]
               AND matnr EQ @lwa_glflcma_x-ymatnr.
          ENDIF.

          LOOP AT lt_matdoc INTO DATA(lwa_matdoc_x).
            IF lwa_matdoc_x-bwart EQ '102'
            OR lwa_matdoc_x-bwart EQ 'Z94'.
              lv_menge = lwa_matdoc_x-menge * -1.
            ELSE.
              lv_menge = lwa_matdoc_x-menge.
            ENDIF.
            lv_menge_x = lv_menge_x + lv_menge.
          ENDLOOP.
          CLEAR lv_menge.
*--EOC-T_T.KONNO-07.15.21
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT tplnr_fl contr FROM /agri/glflcma
    INTO CORRESPONDING FIELDS OF TABLE lt_cskey
   WHERE tplnr_fl EQ cs_mdhdr-tplnr_fl
     AND cmnum    EQ lc_cultura
     AND loevm    EQ space.

  IF sy-subrc EQ 0.
    SELECT ivnum FROM /agri/glamhdr
      INTO CORRESPONDING FIELDS OF TABLE lt_amhdr
       FOR ALL ENTRIES IN lt_cskey                 "#EC CI_NO_TRANSFORM
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
           FOR ALL ENTRIES IN lt_ativos            "#EC CI_NO_TRANSFORM
         WHERE tplnr_fl EQ lt_ativos-tplnr_fl
           AND contr    EQ lt_ativos-contr.

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

  SELECT *
    FROM zfmfpsafras
    INTO TABLE @DATA(lt_safras)
   WHERE tipo_safra EQ @lc_tipo_safra-tecnica.

  SORT lt_safras BY ano_safra.

  CLEAR lv_safra_char.
  READ TABLE ct_mdatv INTO lwa_safra_tela
    WITH KEY atinn = lwa_atributo-safra.
  IF sy-subrc EQ 0.
    lv_safra_char = lwa_safra_tela-atwrt.
  ENDIF.

  CLEAR lv_data_char.
  READ TABLE ct_mdatv INTO lwa_data_tela
    WITH KEY atinn = lwa_atributo-data_encerra.
  IF sy-subrc EQ 0.
    READ TABLE lt_safras ASSIGNING FIELD-SYMBOL(<lwa_safra>)
      WITH KEY ano_safra = lv_safra_char.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
        EXPORTING
          float_imp  = lwa_data_tela-atflv
          format_imp = lv_i
        IMPORTING
          char_exp   = lv_data_char.
      <lwa_safra>-fim_safra = lv_data_char.
    ENDIF.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lr_batch
    ASSIGNING FIELD-SYMBOL(<lwa_atnam>).
  IF sy-subrc EQ 0.
    <lwa_atnam> = 'IEQ'.
    <lwa_atnam>-low = 'ABS_BATCH_LOG'.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lr_cx_liq
    ASSIGNING <lwa_atnam>.
  IF sy-subrc EQ 0.
    <lwa_atnam> = 'IEQ'.
    <lwa_atnam>-low = 'RC_FR_CAIX_LIQ'.
  ENDIF.

  DO 2 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_imovel ASSIGNING <lwa_atnam>.
    IF sy-subrc EQ 0.
      <lwa_atnam> = 'IEQ'.
      CASE lv_index.
        WHEN '1'.
          <lwa_atnam>-low = 'RC_FR_COD_IM'.
        WHEN '2'.
          <lwa_atnam>-low = 'RC_FR_TALH'.
      ENDCASE.
    ENDIF.
  ENDDO.

*-- Próprio
  IF lv_proprio EQ abap_true.
    lr_atnam[] = lr_batch[].
*-- Terceiros
  ELSE.
    IF lt_matdoc[] IS NOT INITIAL.
      SORT lt_matdoc BY matnr charg.

      SELECT matnr,
             charg,
             cuobj_bm
        FROM mch1
        INTO CORRESPONDING FIELDS OF TABLE @lt_mch1
        FOR ALL ENTRIES IN @lt_matdoc
       WHERE matnr = @lt_matdoc-matnr
         AND charg = @lt_matdoc-charg.

      SORT lt_mch1 BY matnr charg.

      LOOP AT lt_mch1 ASSIGNING FIELD-SYMBOL(<lwa_mch1>).
        <lwa_mch1>-objek = <lwa_mch1>-cuobj_bm.
      ENDLOOP.
    ENDIF.

    lr_atnam[] = lr_imovel[].
  ENDIF.

  IF lt_matdoc[] IS NOT INITIAL.
    SELECT atinn, adzhl, atnam,
           atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
     WHERE atnam IN @lr_batch.

    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam atinn.
    ENDIF.

    lr_atinn_batch = CORRESPONDING #( lt_cabn MAPPING low = atinn ).
    lwa_atinn = 'IEQ'.
    MODIFY lr_atinn_batch FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

    REFRESH lt_cabn.
    SELECT atinn, adzhl, atnam,
           atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @lt_cabn
     WHERE atnam IN @lr_cx_liq.

    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam atinn.
    ENDIF.

    lr_atinn_cx_liq = CORRESPONDING #( lt_cabn MAPPING low = atinn ).
    lwa_atinn = 'IEQ'.
    MODIFY lr_atinn_cx_liq FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

    REFRESH lt_cabn.
    SELECT atinn, adzhl, atnam,
           atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @lt_cabn
     WHERE atnam IN @lr_imovel.

    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam atinn.
    ENDIF.

    lr_atinn_imovel = CORRESPONDING #( lt_cabn MAPPING low = atinn ).
    lwa_atinn = 'IEQ'.
    MODIFY lr_atinn_imovel FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

*-- Terceiros
    IF lv_proprio EQ abap_false.
      SELECT objek, atinn, atzhl, mafid,
             klart, adzhl, atwrt, atflv
        FROM ausp
        INTO TABLE @lt_ausp
        FOR ALL ENTRIES IN @lt_mch1
       WHERE objek EQ @lt_mch1-objek
         AND atinn IN @lr_atinn_imovel.
    ELSE.
*-- Próprio
      LOOP AT lt_matdoc ASSIGNING FIELD-SYMBOL(<lwa_matdoc>).
        <lwa_matdoc>-atwrt = <lwa_matdoc>-charg.
      ENDLOOP.

      IF lt_matdoc[] IS NOT INITIAL.
        SELECT objek, atinn, atzhl, mafid,
               klart, adzhl, atwrt, atflv
          FROM ausp
          INTO TABLE @DATA(lt_ausp_batch)
          FOR ALL ENTRIES IN @lt_matdoc
         WHERE atinn IN @lr_atinn_batch
           AND atwrt EQ @lt_matdoc-atwrt.

        IF sy-subrc EQ 0.
          SELECT objek, atinn, atzhl, mafid,
                 klart, adzhl, atwrt, atflv
            FROM ausp
            INTO TABLE @lt_ausp
            FOR ALL ENTRIES IN @lt_ausp_batch
           WHERE objek EQ @lt_ausp_batch-objek
             AND atinn IN @lr_atinn_imovel[].
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = cs_mdhdr-tplnr_fl
      IMPORTING
        output = lv_terreno.

    lv_cod_imov_f = lv_cod_imovel = lv_terreno+0(6).
    lv_talhao_f = lv_talhao = lv_terreno+7(5).

    READ TABLE lt_cabn INTO DATA(lwa_imovel)
      WITH KEY atnam = 'RC_FR_COD_IM'.
    IF sy-subrc NE 0.
      CLEAR lwa_imovel.
    ENDIF.

    READ TABLE lt_cabn INTO DATA(lwa_talhao)
      WITH KEY atnam = 'RC_FR_TALH'.
    IF sy-subrc NE 0.
      CLEAR lwa_talhao.
    ENDIF.

    SORT: lt_cabn BY atinn,
          lt_mch1 BY objek,
          lt_matdoc BY matnr charg.

    lv_imovel_atwrt = lv_cod_imovel.
    lv_talhao_atwrt = lv_talhao.

*--BOC- T_T.KONNO-05.27.21
    DATA(lt_ausp_copy) = lt_ausp[].
    DELETE lt_ausp_copy WHERE atwrt NE lv_imovel_atwrt
                          AND atwrt NE lv_talhao_atwrt.
    REFRESH lrt_objek.
    lrt_objek = CORRESPONDING #( lt_ausp_copy MAPPING low = objek ).
    lrs_objek = 'IEQ'.
    MODIFY lrt_objek FROM lrs_objek TRANSPORTING sign option WHERE low <> ''.
    DELETE lt_ausp WHERE objek NOT IN lrt_objek[].
    REFRESH lt_ausp_copy.
*--EOC- T_T.KONNO-05.27.21

    LOOP AT lt_ausp INTO DATA(lwa_caracteristica).
      DATA(lv_tabix) = sy-tabix.
      DATA(lv_delete) = abap_false.
      IF lwa_caracteristica-atinn EQ lwa_imovel-atinn.
        IF lwa_caracteristica-atwrt NE lv_cod_imovel. "lv_cod_imov_f.
          lv_delete = abap_true.
        ENDIF.
      ELSEIF lwa_caracteristica-atinn NE lwa_talhao-atinn.
        IF lwa_caracteristica-atwrt EQ lv_talhao. "lv_talhao_f.
          lv_delete = abap_true.
        ENDIF.
      ENDIF.
      IF lv_delete EQ abap_true.
        READ TABLE lt_ausp INTO DATA(lwa_delete) INDEX lv_tabix.
        IF sy-subrc EQ 0.
          DELETE lt_ausp WHERE objek = lwa_delete-objek.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT: lt_mch1 BY matnr charg,
          lt_ausp BY objek atinn.
    DELETE lt_matdoc WHERE charg IS INITIAL.
    LOOP AT lt_matdoc INTO DATA(lwa_matdoc).
      lv_tabix = sy-tabix.
      READ TABLE lt_mch1 INTO DATA(lwa_mch1)
        WITH KEY matnr = lwa_matdoc-matnr
                 charg = lwa_matdoc-charg BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_ausp INTO lwa_caracteristica
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_imovel-atinn BINARY SEARCH.
        IF ( ( sy-subrc NE 0 ) OR
             ( lwa_caracteristica-atwrt NE lv_terreno(6) AND
               lwa_caracteristica-atwrt IS NOT INITIAL ) ).
          DELETE lt_matdoc INDEX lv_tabix.
        ELSE.
          READ TABLE lt_ausp INTO lwa_caracteristica
            WITH KEY objek = lwa_mch1-objek
                     atinn = lwa_talhao-atinn BINARY SEARCH.
          IF ( ( sy-subrc NE 0 ) OR
               ( lwa_caracteristica-atwrt NE lv_terreno+7(5) AND
                 lwa_caracteristica-atwrt IS NOT INITIAL ) ).
            DELETE lt_matdoc INDEX lv_tabix.
          ENDIF.
        ENDIF.
      ELSE.
        DELETE lt_matdoc INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

*-- Próprio
*    CLEAR lv_sum_menge.
*    IF lv_proprio EQ abap_true.
    lv_sum_menge = lv_menge_x.
*    ELSEIF lv_proprio EQ abap_false.
*      LOOP AT lt_matdoc INTO lwa_matdoc.
*        IF lwa_matdoc-bwart EQ '102'
*        OR lwa_matdoc-bwart EQ 'Z94'.
*          lv_menge = lwa_matdoc-menge * -1.
*        ELSE.
*          lv_menge = lwa_matdoc-menge.
*        ENDIF.
*        lv_sum_menge = lv_sum_menge + lv_menge.
*      ENDLOOP.
*    ENDIF.

    READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<lwa_producao_real>)
      WITH KEY atinn = lwa_atributo-prod_real.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE ct_mdatv
        ASSIGNING <lwa_producao_real>.
      <lwa_producao_real>-updkz = 'I'.
    ELSE.
      IF <lwa_producao_real>-updkz IS INITIAL.
        <lwa_producao_real>-updkz = 'U'.
      ENDIF.
    ENDIF.
    IF <lwa_producao_real> IS ASSIGNED.
      READ TABLE ct_mdatv INTO DATA(lwa_mdatv_ref) INDEX 1.
      IF sy-subrc EQ 0.
        <lwa_producao_real>-atcod = lwa_mdatv_ref-atcod.
      ENDIF.
      <lwa_producao_real>-mdocm = cs_mdhdr-mdocm.
      <lwa_producao_real>-atinn = lwa_atributo-prod_real.
      lv_menge_char = lv_sum_menge.
      <lwa_producao_real>-atflv = lv_menge_char.
    ENDIF.

    READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<lwa_estimativa>)
      WITH KEY atinn = lwa_atributo-estimativa.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE ct_mdatv
        ASSIGNING <lwa_estimativa>.
      <lwa_estimativa>-updkz = 'I'.
    ELSE.
      IF <lwa_estimativa>-updkz IS INITIAL.
        <lwa_estimativa>-updkz = 'U'.
      ENDIF.
    ENDIF.

    IF <lwa_estimativa> IS ASSIGNED
    AND <lwa_estimativa>-atflv IS INITIAL.
      READ TABLE ct_mdatv INTO lwa_mdatv_ref INDEX 1.
      IF sy-subrc EQ 0.
        <lwa_estimativa>-atcod = lwa_mdatv_ref-atcod.
      ENDIF.
      <lwa_estimativa>-mdocm = cs_mdhdr-mdocm.
      <lwa_estimativa>-atinn = lwa_atributo-estimativa.
      <lwa_estimativa>-atflv = lv_estimativa_x.
    ENDIF.

    READ TABLE lt_asset_summary INTO DATA(lwa_asset_summary) INDEX 1.
    IF sy-subrc EQ 0
    AND lwa_asset_summary-menge IS NOT INITIAL.
      READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<lwa_cxpl_real>)
        WITH KEY atinn = lwa_atributo-cxpl_real.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE ct_mdatv
          ASSIGNING <lwa_cxpl_real>.
        <lwa_cxpl_real>-updkz = 'I'.
      ELSE.
        IF <lwa_cxpl_real>-updkz IS INITIAL.
          <lwa_cxpl_real>-updkz = 'U'.
        ENDIF.
      ENDIF.
      IF <lwa_cxpl_real> IS ASSIGNED.
        READ TABLE ct_mdatv INTO lwa_mdatv_ref INDEX 1.
        IF sy-subrc EQ 0.
          <lwa_cxpl_real>-atcod = lwa_mdatv_ref-atcod.
        ENDIF.
        <lwa_cxpl_real>-mdocm = cs_mdhdr-mdocm.
        <lwa_cxpl_real>-atinn = lwa_atributo-cxpl_real.
        lv_menge_char = lwa_asset_summary-menge.
        IF lv_menge_char IS NOT INITIAL.
          <lwa_cxpl_real>-atflv = ( lv_sum_menge / lv_menge_char ).
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_asset_summary INTO lwa_asset_summary INDEX 1.
    IF sy-subrc EQ 0
    AND lwa_asset_summary-menge IS NOT INITIAL.
      READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<lwa_cxpl_estimada>)
        WITH KEY atinn = lwa_atributo-cxpl_estimada.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE ct_mdatv
          ASSIGNING <lwa_cxpl_estimada>.
        <lwa_cxpl_estimada>-updkz = 'I'.
      ELSE.
        IF <lwa_cxpl_estimada>-updkz IS INITIAL.
          <lwa_cxpl_estimada>-updkz = 'U'.
        ENDIF.
      ENDIF.
      IF <lwa_cxpl_estimada> IS ASSIGNED.
        READ TABLE ct_mdatv INTO lwa_mdatv_ref INDEX 1.
        IF sy-subrc EQ 0.
          <lwa_cxpl_estimada>-atcod = lwa_mdatv_ref-atcod.
        ENDIF.
        <lwa_cxpl_estimada>-mdocm = cs_mdhdr-mdocm.
        <lwa_cxpl_estimada>-atinn = lwa_atributo-cxpl_estimada.
        lv_menge_char = lwa_asset_summary-menge.
        READ TABLE ct_mdatv INTO DATA(lwa_estimativa)
          WITH KEY atinn = lwa_atributo-estimativa.
        IF sy-subrc EQ 0
        AND lv_menge_char IS NOT INITIAL.
          <lwa_cxpl_estimada>-atflv = ( lwa_estimativa-atflv / lv_menge_char ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE clint
    FROM klah
    INTO @DATA(lv_clint)
    WHERE class = @cs_mdhdr-mpgrp.

  IF sy-subrc = 0.
    SELECT *
      FROM zabst_ksml
      INTO TABLE @DATA(lt_ksml)
      WHERE clint = @lv_clint.
    IF sy-subrc = 0.
      SELECT atinn, adzhl, atnam,
             atfor, anzst, anzdz
        FROM cabn
        INTO TABLE @lt_cabn
        FOR ALL ENTRIES IN @lt_ksml
       WHERE atnam = @lt_ksml-atnam.
    ENDIF.
  ENDIF.

  LOOP AT ct_mdatv INTO DATA(lwa_mdatv).
    READ TABLE lt_cabn INTO DATA(lwa_cabn)
      WITH KEY atinn = lwa_mdatv-atinn.
    IF sy-subrc EQ 0.
      READ TABLE lt_ksml INTO DATA(lwa_ksml)
        WITH KEY atnam = lwa_cabn-atnam.
      IF sy-subrc = 0.
        DATA(lv_data) = lwa_ksml-ident(1).
        lwa_attributes-atflv = lwa_mdatv-atflv.
        lwa_attributes-atnam = lwa_ksml-atnam.
        lwa_attributes-ident = lwa_ksml-ident.
        APPEND lwa_attributes TO lt_attributes.
      ENDIF.
    ENDIF.
    CLEAR: lwa_attributes, lwa_ksml, lwa_cabn, lwa_mdatv.
  ENDLOOP.

  IF lt_attributes[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT lt_ksml INTO lwa_ksml WHERE formula IS NOT INITIAL.
    CLEAR: lwa_cabn.
    READ TABLE lt_cabn INTO lwa_cabn
      WITH KEY atnam = lwa_ksml-atnam.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<fs_mdatv>) INDEX 1.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    lwa_mdatv = <fs_mdatv>.
    lwa_mdatv-atinn = lwa_cabn-atinn.

    CALL FUNCTION 'ZABS_FM_EVAL_FORMULA'
      EXPORTING
        iv_formula    = lwa_ksml-formula
        it_attributes = lt_attributes
      IMPORTING
        ev_subrc      = lv_subrc
        ev_message    = lv_message
        ev_value      = lv_value.
    IF lv_subrc IS INITIAL.
      lwa_mdatv-atflv = lv_value.
      APPEND lwa_mdatv TO ct_mdatv.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
