FUNCTION zabs_fetch_measurement_doc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_DOC_MEDICAO) TYPE  /AGRI/GLMDOCM OPTIONAL
*"     VALUE(IV_TIPO_DOC) TYPE  /AGRI/GLMDTYP OPTIONAL
*"     VALUE(IV_TERRENO) TYPE  /AGRI/GLTPLNR_FL OPTIONAL
*"     VALUE(IV_GRUPO_MEDICAO) TYPE  /AGRI/GLMPGRP OPTIONAL
*"     VALUE(IV_DATA_MED_DE) TYPE  /AGRI/GLMDATE OPTIONAL
*"     VALUE(IV_DATA_MED_ATE) TYPE  /AGRI/GLMDATE OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DOC_MEDICAO) TYPE  ZABS_TTY_GLMDHDR
*"     VALUE(ET_ITENS_DOC_MEDICAO) TYPE  ZABS_TTY_GLMDITM
*"     VALUE(ET_ATRIBUTOS_MEDICAO) TYPE  ZABS_TTY_GLMDATV
*"----------------------------------------------------------------------

  DATA: lwa_makt       TYPE makt,
        lr_mdocm       TYPE RANGE OF /agri/glmdocm,
        lr_mdtyp       TYPE RANGE OF /agri/glmdtyp,
        lr_tplnr_fl    TYPE RANGE OF /agri/gltplnr_fl,
        lr_mpgrp       TYPE RANGE OF /agri/glmpgrp,
        lr_mdate       TYPE RANGE OF /agri/glmdate,
        lv_matnr       TYPE matnr,
        lv_spras       TYPE spras VALUE 'P',
        lv_doc_medicao TYPE /agri/glmdocm,
        lv_terreno     TYPE /agri/gltplnr_fl.

  STATICS: lv_cit_quimicos TYPE atinn.

  IF iv_doc_medicao IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_mdocm
      ASSIGNING FIELD-SYMBOL(<lwa_mdocm>).
    IF sy-subrc EQ 0.
      <lwa_mdocm> = 'IEQ'.
      lv_doc_medicao = iv_doc_medicao.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_doc_medicao
        IMPORTING
          output = lv_doc_medicao.

      <lwa_mdocm>-low = lv_doc_medicao.
    ENDIF.
  ENDIF.

  IF iv_tipo_doc IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_mdtyp
      ASSIGNING FIELD-SYMBOL(<lwa_mdtyp>).
    IF sy-subrc EQ 0.
      <lwa_mdtyp> = 'IEQ'.
      <lwa_mdtyp>-low = iv_tipo_doc.
    ENDIF.
  ENDIF.

  IF iv_terreno IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_tplnr_fl
      ASSIGNING FIELD-SYMBOL(<lwa_tplnr_fl>).
    IF sy-subrc EQ 0.
      <lwa_tplnr_fl> = 'IEQ'.

      lv_terreno = iv_terreno.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = lv_terreno
        IMPORTING
          output     = lv_terreno
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      <lwa_tplnr_fl>-low = lv_terreno.
    ENDIF.
  ENDIF.

  IF iv_grupo_medicao IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_mpgrp
      ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
    IF sy-subrc EQ 0.
      <lwa_mpgrp> = 'IEQ'.
      <lwa_mpgrp>-low = iv_grupo_medicao.
    ENDIF.
  ENDIF.

  IF iv_data_med_de IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_mdate
      ASSIGNING FIELD-SYMBOL(<lwa_mdate>).
    IF sy-subrc EQ 0.
      <lwa_mdate> = 'IEQ'.
      <lwa_mdate>-low = iv_data_med_de.
    ENDIF.
  ENDIF.

  IF iv_data_med_ate IS NOT INITIAL.
    READ TABLE lr_mdate ASSIGNING <lwa_mdate> INDEX 1.
    IF sy-subrc EQ 0.
      <lwa_mdate> = 'IEQ'.
      <lwa_mdate>-high = iv_data_med_ate.
    ENDIF.
  ENDIF.

  SELECT *
    FROM /agri/glmdhdr
    INTO TABLE @DATA(lt_glmdhdr)
   WHERE mdocm    IN @lr_mdocm[]
     AND mdtyp    IN @lr_mdtyp[]
     AND tplnr_fl IN @lr_tplnr_fl[]
     AND mpgrp    IN @lr_mpgrp[]
     AND mdate    IN @lr_mdate[].

  IF sy-subrc EQ 0.
    SELECT *
      FROM /agri/glmditm
      INTO TABLE @DATA(lt_glmditm)
      FOR ALL ENTRIES IN @lt_glmdhdr
     WHERE mdocm = @lt_glmdhdr-mdocm.

    SELECT *
      FROM /agri/glmdatv
      INTO TABLE @DATA(lt_glmdatv)
      FOR ALL ENTRIES IN @lt_glmdhdr
     WHERE mdocm = @lt_glmdhdr-mdocm.

    IF sy-uname EQ 'T_T.KONNO'.
      BREAK-POINT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'CIT-QUIMICOS'
      IMPORTING
        output = lv_cit_quimicos.

    LOOP AT lt_glmdatv INTO DATA(lwa_glmdatv).
      INSERT INITIAL LINE INTO TABLE et_atributos_medicao
        ASSIGNING FIELD-SYMBOL(<lwa_atributo_medicao>).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lwa_glmdatv TO <lwa_atributo_medicao>.

        IF lwa_glmdatv-atinn EQ lv_cit_quimicos..
          CONDENSE lwa_glmdatv-atwrt.
          lv_matnr = lwa_glmdatv-atwrt(18).
          CALL FUNCTION 'MAKT_SINGLE_READ'
            EXPORTING
              matnr      = lv_matnr
              spras      = lv_spras
            IMPORTING
              wmakt      = lwa_makt
            EXCEPTIONS
              wrong_call = 1
              not_found  = 2
              OTHERS     = 3.

          IF sy-subrc EQ 0.
            <lwa_atributo_medicao>-matnr = lwa_makt-matnr.
            <lwa_atributo_medicao>-maktx = lwa_makt-maktx.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT: lt_glmdhdr BY mdocm,
        lt_glmditm BY mdocm.

  et_doc_medicao[] = lt_glmdhdr[].
  et_itens_doc_medicao[] = lt_glmditm[].
*  et_atributos_medicao[] = lt_glmdatv[].

ENDFUNCTION.
