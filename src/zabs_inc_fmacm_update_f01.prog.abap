*&---------------------------------------------------------------------*
*& Include ZABS_INC_FMACM_UPDATE_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form SET_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_default_values .



ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

  IF so_acnum[] IS INITIAL.
*-- Informar Área de Cultivo!
    MESSAGE i290(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_ajahr IS INITIAL.
*-- Informar Ano!
    MESSAGE i291(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_screen .



ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_CULTIVATION_AREA_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_cultivation_area_data .

  REFRESH: gt_achdr_old, gt_acitm_old, gt_acvlcl_old.

  IF so_acnum[] IS NOT INITIAL
  AND p_ajahr IS NOT INITIAL.
    SELECT *
      FROM zfmachdr
      INTO TABLE @gt_achdr_old
     WHERE acnum IN @so_acnum[]
       AND ajahr EQ @p_ajahr.

    IF sy-subrc EQ 0.
      SELECT *
        FROM zfmaitm
        INTO TABLE @gt_acitm_old
        FOR ALL ENTRIES IN @gt_achdr_old
       WHERE acnum = @gt_achdr_old-acnum.

      SELECT *
        FROM zfmacvlcl
        INTO TABLE @gt_acvlcl_old
        FOR ALL ENTRIES IN @gt_achdr_old
       WHERE acnum = @gt_achdr_old-acnum.
    ENDIF.

    SORT gt_achdr_old BY acnum ajahr.
    SORT gt_acitm_old BY acnum acpos.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_CULTIVATION_AREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_cultivation_area .

  TYPES: BEGIN OF type_safras,
           tplnr_fl TYPE /agri/gltplnr_fl,
           contr    TYPE /agri/gcontr,
           datab    TYPE /agri/gldatab,
           datbi    TYPE /agri/gldatbi,
         END OF type_safras,

         type_safras_tab TYPE STANDARD TABLE OF type_safras INITIAL SIZE 0.

  TYPES: BEGIN OF type_terrain,
           input  TYPE /agri/gltplnr_fl,
           output TYPE /agri/gltplnr_fl,
           season TYPE /agri/gl_season,
         END OF type_terrain,

         type_terrain_tab TYPE STANDARD TABLE OF type_terrain INITIAL SIZE 0.

  DATA: lt_achdr_new    TYPE STANDARD TABLE OF zfmachdr INITIAL SIZE 0,
        lt_acitm_new    TYPE zt_fmacitm,
        lt_acitm_aux    TYPE zt_fmacitm,
        lt_zfmaitm_new  TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
        lt_acitm_old    TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
        lt_acvlcl_new   TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_acvlcl_old   TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_glmdhdr      TYPE /agri/t_glmdhdr,
        lt_glmdatv      TYPE /agri/t_glmdatv,
        lt_terrain      TYPE type_terrain_tab,
        lt_sorted       TYPE zt_fmacitm,
        lt_safras_new   TYPE type_safras_tab,
        lrt_iwerk       TYPE RANGE OF iwerk,
        ls_vcl_layout   TYPE zsc_fmacvlcl_fcat,
        ls_achdr_new    TYPE zfmachdr,
        lv_sumqtyplants TYPE zfmacqpl,
        lv_sumabsolarea TYPE /agri/glaarea,
        lv_sumavolumen  TYPE zfmacvlctl,
        lv_msgv1        TYPE sy-msgv1,
        lv_msgli        TYPE sy-msgli,
        lv_float        TYPE f,
        lv_valor        TYPE zfmacqtb,
        lv_subrc        TYPE sysubrc.

  CONSTANTS: BEGIN OF c_unit_of_measurement,
               unit    TYPE meins VALUE 'UN',
               hectare TYPE meins VALUE 'HAR',
             END OF c_unit_of_measurement.

  FIELD-SYMBOLS: <ls_acdoc_infocus> TYPE zsc_fmac_doc.

  REFRESH gt_message.

  LOOP AT gt_achdr_old INTO DATA(ls_achdr_old).
    REFRESH: lt_achdr_new, lt_acitm_new, lt_zfmaitm_new,
             lt_acitm_old, lt_acvlcl_new, lt_acvlcl_old,
             lt_glmdhdr, lt_glmdatv, lt_terrain,
             lt_sorted, lt_safras_new, lrt_iwerk.

    CLEAR: gs_acdoc_infocus, ls_achdr_new, lv_sumqtyplants,
           lv_sumabsolarea, lv_sumavolumen, lv_subrc, lv_msgli.

    PERFORM document_data_initialize IN PROGRAM saplzfmacm
                                          USING abap_true IF FOUND.

    CALL FUNCTION 'ENQUEUE_EZ_FMAC'
      EXPORTING
        mode_zfmachdr  = 'X'
        mandt          = sy-mandt
        acnum          = ls_achdr_old-acnum
        ajahr          = ls_achdr_old-ajahr
        x_acnum        = ' '
        x_ajahr        = ' '
        _scope         = '2'
        _wait          = ' '
        _collect       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      lv_subrc = sy-subrc.
      lv_msgv1 = sy-msgv1.
*-- Área de Cultivo &1 bloqueada pelo usuário &2!
      MESSAGE e292(zfmfp) WITH ls_achdr_old-acnum lv_msgv1 INTO lv_msgli.
      sy-msgli = lv_msgli.
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '292'.
        <ls_message>-msgty = 'E'.
        <ls_message>-msgv1 = ls_achdr_old-acnum.
        <ls_message>-msgv2 = lv_msgv1.
      ENDIF.
      CONTINUE.
    ENDIF.

    gs_acdoc_infocus-acnum = ls_achdr_old-acnum.

    PERFORM set_document_mode IN PROGRAM saplzfmacm
                                   USING c_mode_change IF FOUND.

    lt_acvlcl_old[] = gt_acvlcl_old[].
    DELETE lt_acvlcl_old WHERE acnum NE ls_achdr_old-acnum.

    lt_acitm_old[] = gt_acitm_old[].
    DELETE lt_acitm_old WHERE acnum NE ls_achdr_old-acnum.

    DATA(lt_werks) = lt_acitm_old[].
    SORT lt_werks BY iwerk.
    DELETE ADJACENT DUPLICATES FROM lt_werks COMPARING iwerk.
    LOOP AT lt_werks INTO DATA(ls_werks).
      INSERT INITIAL LINE INTO TABLE lrt_iwerk
        ASSIGNING FIELD-SYMBOL(<lrs_iwerk>).
      IF sy-subrc EQ 0.
        <lrs_iwerk> = 'IEQ'.
        <lrs_iwerk>-low = ls_werks-iwerk.
      ENDIF.
    ENDLOOP.

    INSERT INITIAL LINE INTO TABLE lrt_iwerk ASSIGNING <lrs_iwerk>.
    IF sy-subrc EQ 0.
      <lrs_iwerk> = 'IEQ'.
      <lrs_iwerk>-low = ls_achdr_old-werks.
    ENDIF.

    SORT lrt_iwerk BY low.
    DELETE ADJACENT DUPLICATES FROM lrt_iwerk COMPARING low.

*-- BOC T_T.KONNO 04.07.21
*    SELECT *
*      FROM /agri/glflcma AS a
*      INNER JOIN /agri/glflot AS t
*      ON a~tplnr_fl EQ t~tplnr_fl
*      INTO CORRESPONDING FIELDS OF TABLE @lt_acitm_new
*     WHERE t~iwerk IN @lrt_iwerk[]
*       AND a~cmnum EQ 'CITROS'
*       AND a~loevm NE @abap_true
*       AND a~astat EQ @c_crop_season_status-active.
*
*    SELECT *
*      FROM /agri/glflcma AS a
*      INNER JOIN /agri/glflot AS t
*      ON a~tplnr_fl EQ t~tplnr_fl
*      APPENDING CORRESPONDING FIELDS OF TABLE @lt_acitm_new
*     WHERE t~iwerk      IN @lrt_iwerk[]
*       AND a~cmnum      EQ 'CITROS'
*       AND a~loevm      NE @abap_true
*       AND a~zzprevisto EQ @abap_true.

    SELECT *
      FROM /agri/glflcma AS a
      INNER JOIN /agri/glflot AS t
      ON a~tplnr_fl EQ t~tplnr_fl
      INTO CORRESPONDING FIELDS OF TABLE @gt_glflcma
     WHERE t~iwerk      IN @lrt_iwerk[]
       AND a~cmnum      EQ 'CITROS'
       AND a~loevm      NE @abap_true
       AND a~astat      EQ @c_crop_season_status-active
       AND a~zzprevisto EQ @abap_false.

    SELECT *
      FROM /agri/glflcma AS a
      INNER JOIN /agri/glflot AS t
      ON a~tplnr_fl EQ t~tplnr_fl
      APPENDING CORRESPONDING FIELDS OF TABLE @gt_glflcma
     WHERE t~iwerk      IN @lrt_iwerk[]
       AND a~cmnum      EQ 'CITROS'
       AND a~loevm      NE @abap_true
       AND a~zzprevisto EQ @abap_true.
*-- EOC T_T.KONNO 04.07.21

*-- BOC T_T.KONNO 04.07.21
*    SORT lt_acitm_new BY tplnr_fl ASCENDING contr DESCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_acitm_new COMPARING tplnr_fl contr.
*
*    DELETE lt_acitm_new WHERE NOT ( datab BETWEEN ls_achdr_old-datab
*                                              AND ls_achdr_old-datbi )
*                          AND NOT ( datbi BETWEEN ls_achdr_old-datab
*                                              AND ls_achdr_old-datbi )
*                          AND NOT ( datab LE ls_achdr_old-datab AND
*                                    datbi GE ls_achdr_old-datbi ).
*    SORT gt_glflcma BY tplnr_fl ASCENDING contr DESCENDING.
    SORT gt_glflcma BY tplnr_fl contr.
    DELETE ADJACENT DUPLICATES FROM gt_glflcma COMPARING tplnr_fl contr.

    DELETE gt_glflcma WHERE NOT ( datab BETWEEN ls_achdr_old-datab
                                            AND ls_achdr_old-datbi )
                        AND NOT ( datbi BETWEEN ls_achdr_old-datab
                                            AND ls_achdr_old-datbi )
                        AND NOT ( datab LE ls_achdr_old-datab AND
                                  datbi GE ls_achdr_old-datbi ).
*-------------------------------------------------------------------*
*-- BOC T_T.KONNO 04.10.21
*   lt_acitm_new = CORRESPONDING #( gt_glflcma ).
    DATA(lt_glflcma_1) = gt_glflcma[].
    DATA(lt_glflcma_2) = gt_glflcma[].
*-- 1)NÃO PREVISTO: [ZZPREVISTO = ABAP_FALSE] ou
*-- [ZZPREVISTO = ABAP_TRUE e ZZPREV_PLANTIO = VAZIO]
    DELETE lt_glflcma_1 WHERE zzprevisto EQ abap_true
                          AND zzprev_plantio IS NOT INITIAL.
    lt_acitm_new = CORRESPONDING #( lt_glflcma_1 ).
*-- 2) PREVISTO: [ZZPREVISTO = ABAP_TRUE e ZZPREV_PLANTIO <> VAZIO]
    DELETE lt_glflcma_2 WHERE zzprevisto EQ abap_false
                           OR zzprev_plantio IS INITIAL.
    lt_acitm_aux = CORRESPONDING #( lt_glflcma_2 MAPPING aarea = zzarea_talhao ).
    APPEND LINES OF lt_acitm_aux TO lt_acitm_new.
*-- EOC T_T.KONNO 04.10.21
*-------------------------------------------------------------------*
*-- EOC T_T.KONNO 04.07.21

    PERFORM terrain_attribute_get IN PROGRAM saplzfmacm
                                    CHANGING lt_acitm_new
                                             lt_safras_new IF FOUND.

    LOOP AT lt_acitm_new ASSIGNING FIELD-SYMBOL(<ls_acitm_new>).
      <ls_acitm_new>-acnum = ls_achdr_old-acnum.

      ADD <ls_acitm_new>-aarea TO lv_sumabsolarea.
      ADD <ls_acitm_new>-adqpl TO lv_sumqtyplants.
      ADD <ls_acitm_new>-advlc TO lv_sumavolumen.

      INSERT INITIAL LINE INTO TABLE lt_terrain
        ASSIGNING FIELD-SYMBOL(<ls_terrain>).
      IF sy-subrc EQ 0.
        <ls_terrain>-input = <ls_acitm_new>-tplnr_fl.

        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = <ls_acitm_new>-tplnr_fl
          IMPORTING
            output = <ls_terrain>-output.

        <ls_terrain>-season = <ls_acitm_new>-season.
      ENDIF.
    ENDLOOP.

    SORT lt_terrain BY output season.

    LOOP AT lt_terrain INTO DATA(ls_terrain).
      DATA(lv_tabix) = sy-tabix.
      READ TABLE lt_acitm_new INTO DATA(ls_acitm_new)
        WITH KEY tplnr_fl = ls_terrain-input
                 season   = ls_terrain-season.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE lt_sorted
          ASSIGNING FIELD-SYMBOL(<ls_sorted>).
        IF sy-subrc EQ 0.
          <ls_sorted> = ls_acitm_new.
          <ls_sorted>-acpos = lv_tabix.
          APPEND <ls_sorted> TO gs_acdoc_infocus-x-acitm.
        ENDIF.
      ENDIF.
    ENDLOOP.

    lt_acitm_new[] = lt_sorted[].
    ls_achdr_new = ls_achdr_old.

    MOVE lv_sumqtyplants TO ls_achdr_new-qtplt.
    MOVE lv_sumabsolarea TO ls_achdr_new-arabs.
    MOVE lv_sumavolumen  TO ls_achdr_new-vlctl.

    ASSIGN ('(SAPLZFMACM)GS_ACDOC_INFOCUS') TO <ls_acdoc_infocus>.
    IF sy-subrc EQ 0.
      <ls_acdoc_infocus> = gs_acdoc_infocus.
      PERFORM volumen_calda_create IN PROGRAM saplzfmacm IF FOUND.
      REFRESH: lt_glmdatv, lt_glmdhdr.
*--- Fetch Measurement Document
      PERFORM measurement_document_get IN PROGRAM saplzfmacm
                                         CHANGING lt_glmdatv
                                                  lt_glmdhdr IF FOUND.

      lt_acvlcl_new[] = <ls_acdoc_infocus>-x-acvlc[].

      IF lt_acvlcl_old[] IS NOT INITIAL
      AND lt_acvlcl_new[] IS NOT INITIAL.
        SORT: lt_acvlcl_old BY acnum vornr matnr tplnr_fl,
              lt_acvlcl_new BY acnum vornr matnr tplnr_fl.

        LOOP AT lt_acvlcl_new ASSIGNING FIELD-SYMBOL(<ls_acvlcl_new>).
          CLEAR ls_vcl_layout.
          READ TABLE lt_acvlcl_old INTO DATA(ls_acvlcl_old)
            WITH KEY acnum    = <ls_acvlcl_new>-acnum
                     vornr    = <ls_acvlcl_new>-vornr
                     matnr    = <ls_acvlcl_new>-matnr
                     tplnr_fl = <ls_acvlcl_new>-tplnr_fl BINARY SEARCH.
          IF sy-subrc EQ 0.
*-- % Execução
            <ls_acvlcl_new>-acpex = ls_acvlcl_old-acpex.

*            CLEAR ls_acitm_new.
*            READ TABLE lt_acitm_new INTO ls_acitm_new
*              WITH KEY tplnr_fl = <ls_acvlcl_new>-tplnr_fl.
*-- SAFRA PLAN -> FORMAÇÃO
*-- SAFRA PROD -> MANUT&COLHEITA
            CLEAR ls_acitm_new.
            CASE <ls_acvlcl_new>-matnr(4).
              WHEN 'TFOR'
                OR 'TIMP'.
                READ TABLE lt_acitm_new INTO ls_acitm_new
                  WITH KEY tplnr_fl = <ls_acvlcl_new>-tplnr_fl
                           season   = 'SAFRA PLAN'.
              WHEN 'TMAN'
                OR 'TCOL'.
                READ TABLE lt_acitm_new INTO ls_acitm_new
                  WITH KEY tplnr_fl = <ls_acvlcl_new>-tplnr_fl
                           season   = 'SAFRA PROD'.
              WHEN OTHERS.
                READ TABLE lt_acitm_new INTO ls_acitm_new
                  WITH KEY tplnr_fl = <ls_acvlcl_new>-tplnr_fl.
            ENDCASE.
            IF sy-subrc EQ 0.
*-- Área a executar
              <ls_acvlcl_new>-acarx = ( ( <ls_acvlcl_new>-acpex * ls_acitm_new-aarea ) / 100 ).
              IF <ls_acvlcl_new>-acarx IS NOT INITIAL.
                <ls_acvlcl_new>-unarx = c_unit_of_measurement-hectare.
              ENDIF.
            ENDIF.

            <ls_acvlcl_new>-acidt = ls_acvlcl_old-acidt.
            <ls_acvlcl_new>-acdis = ls_acvlcl_old-acdis.

*-- Volume de Calda
            MOVE-CORRESPONDING <ls_acvlcl_new> TO ls_vcl_layout.
            PERFORM volumen_calda_calculate IN PROGRAM saplzfmacm
                                                 USING ls_vcl_layout
                                                       lt_glmdhdr[]
                                                       lt_glmdatv[]
                                              CHANGING <ls_acvlcl_new>-acvcl IF FOUND.

            IF <ls_acvlcl_new>-matnr CS '0150'.
              IF <ls_acvlcl_new>-acdis IS NOT INITIAL.
                lv_float = <ls_acvlcl_new>-acidt * 1000 * <ls_acvlcl_new>-acarx.
                <ls_acvlcl_new>-acqtb = lv_float / <ls_acvlcl_new>-acdis.
                lv_valor = <ls_acvlcl_new>-acidt * 1000 * <ls_acvlcl_new>-acarx.
                lv_valor = lv_valor / <ls_acvlcl_new>-acdis.
              ENDIF.
            ELSE.
              IF <ls_acvlcl_new>-acdis IS NOT INITIAL.
                <ls_acvlcl_new>-acqtb = <ls_acvlcl_new>-acvcl / <ls_acvlcl_new>-acdis.
                MOVE c_unit_of_measurement-unit TO <ls_acvlcl_new>-acuqb.
              ENDIF.
            ENDIF.

            <ls_acvlcl_new>-acfcb = ls_acvlcl_old-acfcb.
            <ls_acvlcl_new>-acren = ls_acvlcl_old-acren.
            <ls_acvlcl_new>-unren = ls_acvlcl_old-unren.
*            <ls_acvlcl_new>-acarx = ls_acvlcl_old-acarx.
            <ls_acvlcl_new>-unarx = ls_acvlcl_old-unarx.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF lt_acitm_old[] IS NOT INITIAL.
      DELETE zfmaitm FROM TABLE lt_acitm_old.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF lt_acvlcl_old[] IS NOT INITIAL.
      DELETE zfmacvlcl FROM TABLE lt_acvlcl_old.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF lt_acitm_new[] IS NOT INITIAL.
      UPDATE zfmachdr FROM ls_achdr_new.
      lt_zfmaitm_new = CORRESPONDING #( lt_acitm_new ).
      INSERT zfmaitm FROM TABLE lt_zfmaitm_new.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF lt_acvlcl_new[] IS NOT INITIAL.
      INSERT zfmacvlcl FROM TABLE lt_acvlcl_new.
      COMMIT WORK AND WAIT.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EZ_FMAC'
      EXPORTING
        mandt = sy-mandt
        acnum = ls_achdr_old-acnum
        ajahr = ls_achdr_old-ajahr.

*-- Área de Cultivo &1 atualizada com sucesso!
    MESSAGE s293(zfmfp) WITH ls_achdr_old-acnum INTO lv_msgli.
    sy-msgli = lv_msgli.
    INSERT INITIAL LINE INTO TABLE gt_message ASSIGNING <ls_message>.
    IF sy-subrc EQ 0.
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = '293'.
      <ls_message>-msgty = 'S'.
      <ls_message>-msgv1 = ls_achdr_old-acnum.
    ENDIF.
  ENDLOOP.

ENDFORM.
