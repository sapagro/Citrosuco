FUNCTION zabs_fetch_measurement_doc_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_TIPO_DOC) TYPE  /AGRI/GLMDTYP OPTIONAL
*"     VALUE(IT_TERRENOS) TYPE  ZABS_TTY_GLFLOT OPTIONAL
*"     VALUE(IV_GRUPO_MEDICAO) TYPE  /AGRI/GLMPGRP OPTIONAL
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
        lr_matnr       TYPE RANGE OF mara-matnr,
        lv_matnr       TYPE matnr,
        lv_spras       TYPE spras VALUE 'P',
        lv_doc_medicao TYPE /agri/glmdocm,
        lv_terreno     TYPE /agri/gltplnr_fl.

  STATICS: lv_cit_quimicos TYPE atinn.

  IF iv_tipo_doc IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lr_mdtyp
      ASSIGNING FIELD-SYMBOL(<lwa_mdtyp>).
    IF sy-subrc EQ 0.
      <lwa_mdtyp> = 'IEQ'.
      <lwa_mdtyp>-low = iv_tipo_doc.
    ENDIF.
  ENDIF.

  IF it_terrenos IS NOT INITIAL.

    LOOP AT it_terrenos INTO DATA(ls_terreno).
      ls_terreno-strno = ls_terreno-tplnr_fl.
    ENDLOOP.

    SELECT tplnr_fl, strno FROM /agri/glflot
      INTO TABLE @DATA(lt_tplnr_fl)
      FOR ALL ENTRIES IN @it_terrenos
      WHERE strno EQ @it_terrenos-strno.

    IF sy-subrc = 0.

      lr_tplnr_fl = VALUE #( FOR struc IN lt_tplnr_fl ( sign = 'I'
                                                        option = 'EQ'
                                                        low    = struc-tplnr_fl ) ).

*      LOOP AT it_terrenos INTO ls_terreno.
*
*        READ TABLE lt_tplnr_fl INTO DATA(ls_tplnr) with key strno = ls_terreno-strno.
*        IF sy-subrc = 0.
*
*          APPEND INITIAL LINE TO lr_tplnr_fl
*            ASSIGNING FIELD-SYMBOL(<lwa_tplnr_fl>).
*          IF sy-subrc EQ 0.
*            <lwa_tplnr_fl> = 'IEQ'.
*
*            <lwa_tplnr_fl>-low = ls_tplnr-tplnr_fl.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
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


  SELECT *
    FROM /agri/glmdhdr
    INTO TABLE @DATA(lt_glmdhdr)
   WHERE mdtyp    IN @lr_mdtyp[]
     AND tplnr_fl IN @lr_tplnr_fl[]
     AND mpgrp    IN @lr_mpgrp[].


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


    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'CIT-QUIMICOS'
      IMPORTING
        output = lv_cit_quimicos.


    LOOP AT lt_glmdatv INTO DATA(lwa_glmdatv).

      IF lwa_glmdatv-atinn EQ lv_cit_quimicos.

        APPEND INITIAL LINE TO lr_matnr ASSIGNING FIELD-SYMBOL(<fs_m>).
        <fs_m>-sign = 'I'.
        <fs_m>-option = 'EQ'.
        UNPACK lwa_glmdatv-atwrt TO <fs_m>-low.
        <fs_m>-low = <fs_m>-low(18).

      ENDIF.

    ENDLOOP.

    IF lr_matnr IS NOT INITIAL.

      SELECT matnr, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt)
       WHERE matnr IN @lr_matnr.

    ENDIF.

    LOOP AT lt_glmdatv INTO lwa_glmdatv.
      APPEND INITIAL LINE TO et_atributos_medicao
        ASSIGNING FIELD-SYMBOL(<lwa_atributo_medicao>).


        MOVE-CORRESPONDING lwa_glmdatv TO <lwa_atributo_medicao>.

        IF lwa_glmdatv-atinn EQ lv_cit_quimicos.
          CONDENSE lwa_glmdatv-atwrt.
          lv_matnr = lwa_glmdatv-atwrt.
          UNPACK lv_matnr TO lv_matnr.
          READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = lv_matnr.
          IF sy-subrc EQ 0.
            <lwa_atributo_medicao>-matnr = ls_makt-matnr.
            <lwa_atributo_medicao>-maktx = ls_makt-maktx.
          ENDIF.
        ENDIF.

        READ TABLE lt_glmdhdr INTO DATA(ls_hdr) WITH KEY mdocm = lwa_glmdatv-mdocm.
        IF sy-subrc = 0.
          <lwa_atributo_medicao>-tplnr_fl = ls_hdr-tplnr_fl.
          READ TABLE lt_tplnr_fl INTO DATA(ls_tplnr) WITH KEY tplnr_fl = ls_hdr-tplnr_fl.
          IF sy-subrc = 0.
            <lwa_atributo_medicao>-strno = ls_tplnr-strno.
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
