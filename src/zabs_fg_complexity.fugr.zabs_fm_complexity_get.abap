FUNCTION zabs_fm_complexity_get.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_TPLNR) TYPE  /AGRI/T_GLTPLNR
*"     REFERENCE(IT_SAFRA) TYPE  ZABS_T_SAFRA_COMPLEX OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_COMPLEX) TYPE  ZABS_T_TERRAIN_COMPLEX
*"----------------------------------------------------------------------

*-- Local Declarations
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
         END OF ly_atributos,

         BEGIN OF ly_docs,
           mdocm      TYPE /agri/glmdocm,
           safra_doc  TYPE zfmanosafra,
           safra_cont TYPE zfmanosafra,
         END OF ly_docs.

  DATA: lt_docs     TYPE STANDARD TABLE OF ly_docs INITIAL SIZE 0,
        lrt_mdocm   TYPE RANGE OF /agri/glmdocm,
        lrs_mdocm   LIKE LINE OF lrt_mdocm,
        lrt_mpgrp   TYPE RANGE OF /agri/glmpgrp,
        ls_doc      LIKE LINE OF lt_docs,
        ls_athdr    TYPE /agri/s_gathdr,
        ls_complex  TYPE zabs_s_terrain_complex,
        ls_atributo TYPE ly_atributos,
        lv_atwrt    TYPE atwrt,
        lv_ztyp     TYPE /agri/glmdtyp VALUE 'ZTYP',
        lv_zpta     TYPE /agri/glmdtyp VALUE 'ZPTA',
        lv_safra    TYPE zfmanosafra,
        lv_atgrp    TYPE /agri/glmpgrp,
        lv_atnam    TYPE atnam.

  CONSTANTS: BEGIN OF lc_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF lc_measurement_level.

  CONSTANTS: BEGIN OF lc_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF lc_tipo_safra.

  DO 12 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-SAFRA'
          IMPORTING
            output = ls_atributo-safra.
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-ENCERRA'
          IMPORTING
            output = ls_atributo-data_encerra.
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESTIMATIVA'
          IMPORTING
            output = ls_atributo-estimativa.
      WHEN 4.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-EST'
          IMPORTING
            output = ls_atributo-cxpl_estimada.
      WHEN 5.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-PROD-REAL'
          IMPORTING
            output = ls_atributo-prod_real.
      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-REAL'
          IMPORTING
            output = ls_atributo-cxpl_real.
      WHEN 7.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_COD_IM'
          IMPORTING
            output = ls_atributo-cod_imovel.
      WHEN 8.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_TALH'
          IMPORTING
            output = ls_atributo-talhao.
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-FLORADA'
          IMPORTING
            output = ls_atributo-florada.
      WHEN 10.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-INICIAL'
          IMPORTING
            output = ls_atributo-estimativa_ini.
      WHEN 11.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-FINAL'
          IMPORTING
            output = ls_atributo-estimativa_fim.
      WHEN 12.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-VAR-PERC'
          IMPORTING
            output = ls_atributo-variacao_perc.
    ENDCASE.
  ENDDO.

  DO 3 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lrt_mpgrp
      ASSIGNING FIELD-SYMBOL(<lrs_mpgrp>).
    IF sy-subrc EQ 0.
      <lrs_mpgrp> = 'IEQ'.
      CASE lv_index.
        WHEN '1'.
          <lrs_mpgrp>-low = 'CIT-ESTIMATIVA'.
        WHEN '2'.
          <lrs_mpgrp>-low = 'FAZ-ESTIMATIVA'.
        WHEN '3'.
          <lrs_mpgrp>-low = 'ENCERRAR_FLORADA'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SELECT *
    FROM zfmfpsafras
    INTO TABLE @DATA(lt_safras)
   WHERE tipo_safra EQ @lc_tipo_safra-contabil.

*--BOC-T_T.KONNO-07.15.21
  LOOP AT lt_safras INTO DATA(ls_safra).
    DATA(lv_tabix) = sy-tabix.
    IF sy-datum NOT BETWEEN ls_safra-inicio_safra
                        AND ls_safra-fim_safra.
      DELETE lt_safras INDEX lv_tabix.
    ENDIF.
  ENDLOOP.
*--EOC-T_T.KONNO-07.15.21

  SORT lt_safras BY ano_safra.

*-- Fetch the measurement document type, attribute group, attribute name
*-- to get the terrain complexity
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish "'ACCM'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_complex "'COMP'
    IMPORTING
      ev_cnval1 = lv_ztyp
      ev_cnval2 = lv_atgrp
      ev_cnval3 = lv_atnam.

*-- Fetch the Internal Characteristic value
  SELECT SINGLE atinn
    FROM cabn
    INTO @DATA(lv_atinn)
   WHERE atnam EQ @lv_atnam. "'FAZ_COMPLEXIDADE'.

  IF sy-subrc EQ 0.
*-- Fetch the attribute header details
    SELECT SINGLE *
      FROM /agri/gatha AS a
      INNER JOIN cabn AS b
      ON b~atinn EQ a~atinn
      INTO CORRESPONDING FIELDS OF ls_athdr
     WHERE a~atinn EQ lv_atinn.

    IF sy-subrc EQ 0.
      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~equnr, h~mpgrp,
             h~mdate, h~mtime, v~atzhl, v~atwrt, v~atflv,
             c~atinn, q~value AS atinn_out, c~atnam
        INTO TABLE @DATA(lt_glmd_florada)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        LEFT OUTER JOIN /agri/fm_qmsmpl AS q
        ON q~datum EQ c~adatu "creates field atinn_out char40
        FOR ALL ENTRIES IN @it_tplnr
       WHERE h~mdtyp    EQ @lv_zpta "'ZPTA'
         AND h~aslvl    EQ @lc_measurement_level-crop_seasons
         AND h~tplnr_fl EQ @it_tplnr-tplnr_fl
         AND h~mpgrp    IN @lrt_mpgrp
         AND h~kfrst    EQ @/agri/cl_fmpr_process_ticket=>mc_kfrst
         AND h~canceled EQ @abap_false.

      DATA(lv_found) = abap_false.

      LOOP AT lt_glmd_florada INTO DATA(ls_glmd_florada).
        IF ls_atributo-safra NE ls_glmd_florada-atinn.
          CONTINUE.
        ELSE.
          IF ls_glmd_florada-atwrt CO ' 1234567890'.
            lv_found = abap_false.
            LOOP AT lt_safras INTO ls_safra
              WHERE ano_safra = ls_glmd_florada-atwrt.
              lv_found = abap_true.
              EXIT.
            ENDLOOP.

            "IF ls_safra-ano_safra NE ls_glmd_florada-atwrt.
            IF lv_found = abap_false.
              DELETE lt_glmd_florada WHERE mdocm EQ ls_glmd_florada-mdocm.
              CLEAR ls_safra.
            ENDIF.
          ELSE.
            DELETE lt_glmd_florada WHERE mdocm EQ ls_glmd_florada-mdocm.
            CLEAR ls_safra.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA(lt_estimativa) = lt_glmd_florada[].

      SORT lt_glmd_florada BY tplnr_fl ASCENDING
                              mpgrp    ASCENDING
                              mdate    DESCENDING
                              mtime    DESCENDING.

      DELETE ADJACENT DUPLICATES FROM lt_glmd_florada COMPARING tplnr_fl mpgrp.
      DELETE lt_estimativa WHERE ( mpgrp NE 'CIT-ESTIMATIVA' AND
                                   mpgrp NE 'FAZ-ESTIMATIVA' )
                              OR ( atnam NE 'CIT-FLORADA' ).
      SORT lt_estimativa BY tplnr_fl atnam.

      DATA(lt_encerramento) = lt_glmd_florada[].
      DELETE lt_encerramento WHERE mpgrp NE 'ENCERRAR_FLORADA'.
      SORT lt_encerramento BY tplnr_fl atnam.

      LOOP AT lt_estimativa INTO DATA(ls_estimativa).
        READ TABLE lt_encerramento INTO DATA(ls_encerramento)
          WITH KEY tplnr_fl = ls_estimativa-tplnr_fl
                   atnam    = 'CIT-FLORADA' BINARY SEARCH.
        WHILE sy-subrc EQ 0.
          lv_tabix = sy-tabix + 1.
          IF ls_encerramento-atwrt EQ ls_estimativa-atwrt.
            DELETE lt_estimativa WHERE mdocm = ls_estimativa-mdocm.
          ENDIF.
          READ TABLE lt_encerramento INTO ls_encerramento INDEX lv_tabix
            COMPARING tplnr_fl atnam.
        ENDWHILE.
      ENDLOOP.

*-- Fetch the measurement documents with complexity attribute values
      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~equnr, h~mpgrp,
             h~mdate, h~mtime, v~atzhl, v~atwrt, v~atflv,
             c~atinn, q~value AS atinn_out, c~atnam
        INTO TABLE @DATA(lt_glmd_complex)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON h~mdocm EQ v~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        LEFT OUTER JOIN /agri/fm_qmsmpl AS q
        ON q~datum EQ c~adatu "creates field atinn_out char40
        FOR ALL ENTRIES IN @it_tplnr
       WHERE h~mdtyp    EQ @lv_ztyp
         AND h~aslvl    EQ @lc_measurement_level-terrain
         AND h~tplnr_fl EQ @it_tplnr-tplnr_fl
         AND h~mpgrp    EQ @lv_atgrp "'FAZ_COMPLEXIDADE'
         AND h~kfrst    EQ @/agri/cl_fmpr_process_ticket=>mc_kfrst
         AND h~canceled EQ @abap_false.

      LOOP AT lt_glmd_complex INTO DATA(ls_glmd_complex).
        IF ls_atributo-safra NE ls_glmd_complex-atinn.
          CONTINUE.
        ELSE.
          IF ls_glmd_complex-atwrt CO ' 1234567890'.
            lv_found = abap_false.
            LOOP AT lt_safras INTO ls_safra
              WHERE ano_safra = ls_glmd_complex-atwrt.

              lv_found = abap_true.
              EXIT.
            ENDLOOP.

            IF lv_found = abap_true.
              ls_doc-mdocm = ls_glmd_complex-mdocm.
              ls_doc-safra_doc = ls_glmd_complex-atwrt.
              ls_doc-safra_cont = ls_safra-ano_safra.
              APPEND ls_doc TO lt_docs.
            ELSE.
              DELETE lt_glmd_complex WHERE mdocm EQ ls_glmd_complex-mdocm.
            ENDIF.
          ELSE.
            DELETE lt_glmd_complex WHERE mdocm EQ ls_glmd_complex-mdocm.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA(lt_check_florada) = lt_glmd_complex[].
      DELETE: lt_check_florada WHERE atnam NE 'CIT-FLORADA',
              lt_glmd_complex  WHERE atnam NE 'FAZ_COMPLEXIDADE'.

      SORT: lt_check_florada BY tplnr_fl ASCENDING
                                atwrt    ASCENDING
                                mdocm    DESCENDING,
            lt_glmd_complex  BY mdocm,
            lt_estimativa    BY tplnr_fl ASCENDING
                                atwrt ASCENDING.

      IF lt_glmd_complex[] IS NOT INITIAL.
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
          FOR ALL ENTRIES IN @lt_glmd_complex
         WHERE h~mdocm EQ @lt_glmd_complex-mdocm.

        LOOP AT lt_glmdhdr_join ASSIGNING FIELD-SYMBOL(<lwa_glmdhdr>).
          IF <lwa_glmdhdr>-atinn IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
              EXPORTING
                input  = <lwa_glmdhdr>-atinn
              IMPORTING
                output = <lwa_glmdhdr>-atinn_out.
          ENDIF.
        ENDLOOP.

        DELETE lt_glmdhdr_join WHERE atinn_out NE 'CIT-SAFRA'.
        SORT lt_glmdhdr_join BY mdocm.

        SORT lt_check_florada BY tplnr_fl atwrt.
        DELETE ADJACENT DUPLICATES FROM lt_check_florada COMPARING tplnr_fl atwrt.

        LOOP AT it_tplnr ASSIGNING FIELD-SYMBOL(<fs_tplnr>).
          CLEAR ls_complex.
          ls_complex-tplnr = <fs_tplnr>-tplnr_fl.

          READ TABLE lt_estimativa INTO ls_estimativa
            WITH KEY tplnr_fl = <fs_tplnr>-tplnr_fl BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE lt_check_florada INTO DATA(ls_check_florada)
                  WITH KEY tplnr_fl = <fs_tplnr>-tplnr_fl
                           atwrt    = ls_estimativa-atwrt BINARY SEARCH.
            IF sy-subrc EQ 0.
              READ TABLE lt_glmd_complex INTO ls_glmd_complex
                WITH KEY mdocm = ls_check_florada-mdocm BINARY SEARCH.
              IF sy-subrc EQ 0.
                READ TABLE lt_glmdhdr_join INTO DATA(ls_glmdhdr_join)
                  WITH KEY mdocm = ls_glmd_complex-mdocm BINARY SEARCH.
                IF sy-subrc EQ 0.
                  CONDENSE ls_glmdhdr_join-atwrt NO-GAPS.
                  IF ls_glmdhdr_join-atwrt CO ' 0123456789'.
                    lv_found = abap_false.
                    LOOP AT lt_safras INTO ls_safra
                    WHERE ano_safra = ls_glmdhdr_join-atwrt.
                      lv_found = abap_true.
                      EXIT.
                    ENDLOOP.

                    IF lv_found = abap_true.
                      CLEAR lv_atwrt.
                      CALL METHOD /agri/cl_gattr_utils=>attr_value_for_display_prepare
                        EXPORTING
                          i_agtyp  = 'X90'
                          is_athdr = ls_athdr
                        CHANGING
                          c_atwrt  = lv_atwrt
                          c_atflv  = ls_glmd_complex-atflv.

*-- Populate the latest complex attribute value
                      ls_complex-complex = lv_atwrt.
                      ls_complex-inicio_safra = ls_safra-inicio_safra.
                      ls_complex-fim_safra    = ls_safra-fim_safra.
                      APPEND ls_complex TO et_complex.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        SORT et_complex BY tplnr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
