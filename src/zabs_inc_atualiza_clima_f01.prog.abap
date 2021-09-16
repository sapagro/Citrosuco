*----------------------------------------------------------------------*
***INCLUDE ZABS_INC_ATUALIZA_CLIMA_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  IF s_tplma[] IS INITIAL.
*-- O terreno superior deve ser informado!
    MESSAGE i377(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_tipo IS INITIAL.
*-- O tipo de dado climático deve ser informado!
    MESSAGE i378(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_data IS INITIAL.
*-- A data de referência deve ser informada!
    MESSAGE i379(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_glflot, gt_glmdhdr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIABLES_INITIATOR
*&---------------------------------------------------------------------*
FORM display_messages  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-cprog.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR lv_initiator.

ENDFORM.                    " MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*& Form FETCH_MEASUREMENT_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_measurement_document .

  DATA: lrt_mpgrp    TYPE RANGE OF /agri/glmdhdr-mpgrp,
        lrt_mdate    TYPE RANGE OF /agri/glmdhdr-mdate,
        lv_mpgrp     TYPE /agri/glmpgrp,
        lv_first_day TYPE sydatum,
        lv_last_day  TYPE sydatum.

  SELECT tplnr_fl, pltxt, tplkz, fltyp, tplvl, tplma, loevm
    FROM /agri/glflot
    INTO TABLE @gt_glflot
   WHERE tplma IN @s_tplma[]
    ORDER BY tplma, tplnr_fl.

  IF sy-subrc NE 0.
*-- Não existem terrenos válidos para os parâmetros informados!
    MESSAGE i380(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    PERFORM check_basic_data CHANGING lv_mpgrp
                                      lv_first_day
                                      lv_last_day.

    INSERT INITIAL LINE INTO TABLE lrt_mdate
      ASSIGNING FIELD-SYMBOL(<lrs_mdate>).
    IF sy-subrc EQ 0.
      <lrs_mdate> = 'IBT'.
      <lrs_mdate>-low  = lv_first_day.
      <lrs_mdate>-high = lv_last_day.
    ENDIF.

    SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
           h~contr, h~cmnum, h~equnr, h~mpgrp,
           h~mdate, h~mtime, v~atzhl, v~atwrt,
           v~atflv, c~atinn, c~atnam, f~tplma
      INTO TABLE @gt_glmdhdr
      FROM /agri/glmdhdr AS h
      INNER JOIN /agri/glmdatv AS v
      ON v~mdocm EQ h~mdocm
      INNER JOIN cabn AS c
      ON c~atinn EQ v~atinn
      INNER JOIN /agri/glflot AS f
      ON f~tplnr_fl EQ h~tplnr_fl
     WHERE h~mdtyp    EQ 'ZIRG'
       AND h~aslvl    EQ 'I'
       AND h~mpgrp    EQ @lv_mpgrp
       AND h~mdate    IN @lrt_mdate[]
       AND h~canceled EQ @abap_false
       AND f~tplma    IN @s_tplma[].

    IF gt_glmdhdr[] IS INITIAL.
*-- Não existem documentos de medição para os parâmetros informados!
      MESSAGE i381(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT gt_glmdhdr BY tplnr_fl mdocm.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_WEATHER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_weather_data .

  TYPES: BEGIN OF ly_total,
           tplma   TYPE /agri/gltplma,
           tipo    TYPE zabs_del_tipo,
           mpgrp   TYPE /agri/glmpgrp,
           year    TYPE /agri/gyear,
           atflv   TYPE atflv,
           atwrt   TYPE atwrt,
           counter TYPE i,
         END OF ly_total.

  DATA: lt_clima_hist_old TYPE STANDARD TABLE OF zabst_clima_hist INITIAL SIZE 0,
        lt_clima_hist_new TYPE STANDARD TABLE OF zabst_clima_hist INITIAL SIZE 0,
        ls_clima_hist_new LIKE LINE OF lt_clima_hist_new,
        lt_total          TYPE STANDARD TABLE OF ly_total INITIAL SIZE 0,
        ls_total          LIKE LINE OF lt_total,
        lv_first_day      TYPE sydatum,
        lv_last_day       TYPE sydatum,
        lv_month          TYPE char2,
        lv_packed         TYPE p DECIMALS 3,
        lv_year           TYPE /agri/gyear,
        lv_mpgrp          TYPE /agri/glmpgrp,
        lv_fieldname      TYPE fieldname.

  PERFORM check_basic_data CHANGING lv_mpgrp
                                    lv_first_day
                                    lv_last_day.

  lv_year = lv_first_day(4).
  lv_month = lv_first_day+4(2).
  lv_fieldname = 'MES' && lv_month.

  IF gt_glmdhdr[] IS NOT INITIAL.
    SELECT *
      FROM zabst_clima_hist
      INTO TABLE @lt_clima_hist_old
      FOR ALL ENTRIES IN @gt_glmdhdr
     WHERE tplnr_fl EQ @gt_glmdhdr-tplma
       AND tipo     EQ @p_tipo
       AND ano      EQ @lv_year.
  ENDIF.

  SORT gt_glmdhdr BY tplnr_fl mdocm.

  LOOP AT gt_glmdhdr INTO DATA(ls_glmdhdr).
    ls_total-tplma = ls_glmdhdr-tplma.
*-- mpgrp = PRECIPITACION / atinn = PRECIPITACION
*-- mpgrp = EVAPORATION / atinn = EVAPORATION
    ls_total-tipo  = p_tipo.
    ls_total-mpgrp = ls_glmdhdr-mpgrp.
    ls_total-year = lv_year.
    ls_total-atflv = ls_glmdhdr-atflv.
    ls_total-counter = 1.
    COLLECT ls_total INTO lt_total.
  ENDLOOP.

  LOOP AT lt_total ASSIGNING FIELD-SYMBOL(<ls_total>).
    lv_packed = <ls_total>-atflv.
    <ls_total>-atwrt = lv_packed.
    CONDENSE <ls_total>-atwrt NO-GAPS.
  ENDLOOP.

  SORT: lt_total BY year tipo tplma,
        lt_clima_hist_old BY ano tipo tplnr_fl.

  LOOP AT lt_total INTO ls_total.
    CLEAR ls_clima_hist_new.
    READ TABLE lt_clima_hist_old ASSIGNING FIELD-SYMBOL(<ls_clima_hist_old>)
      WITH KEY ano      = ls_total-year
               tipo     = ls_total-tipo
               tplnr_fl = ls_total-tplma BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_clima_hist_new = <ls_clima_hist_old>.
    ELSE.
      ls_clima_hist_new-ano      = ls_total-year.
      ls_clima_hist_new-tipo     = ls_total-tipo.
      ls_clima_hist_new-tplnr_fl = ls_total-tplma.
    ENDIF.

    ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_clima_hist_new
      TO FIELD-SYMBOL(<lv_new_value>).
    IF sy-subrc NE 0.
      CONTINUE.
    ELSE.
      IF ls_total-counter IS NOT INITIAL.
        <lv_new_value> = ls_total-atflv / ls_total-counter.
      ENDIF.
    ENDIF.

    APPEND ls_clima_hist_new TO lt_clima_hist_new.
  ENDLOOP.

  IF lt_clima_hist_new[] IS NOT INITIAL.
    MODIFY zabst_clima_hist FROM TABLE lt_clima_hist_new.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

*-- Os dados climáticos foram atualizados!
  MESSAGE i382(zfmfp).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_BASIC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_MPGRP
*&      <-- LV_FIRST_DAY
*&      <-- LV_LAST_DAY
*&---------------------------------------------------------------------*
FORM check_basic_data  CHANGING lv_mpgrp     TYPE /agri/glmpgrp
                                lv_first_day TYPE sydatum
                                lv_last_day  TYPE sydatum.

  DATA: ls_duration  TYPE psen_duration.

  IF p_tipo EQ 'EVAPO'.
    lv_mpgrp  = 'EVAPORATION'.
  ELSEIF p_tipo EQ 'CHUVA'.
    lv_mpgrp  = 'PRECIPITACION'.
  ENDIF.

*-- First day of previous month
  ls_duration-durmm = 1.
  lv_first_day = p_data(6) && '01'.
  CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
    EXPORTING
      im_date     = lv_first_day
      im_operator = '-'
      im_duration = ls_duration
    IMPORTING
      ex_date     = lv_first_day.

*-- Last day of previous month
  CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
    EXPORTING
      i_day_in            = lv_first_day
    IMPORTING
      e_last_day_of_month = lv_last_day.

ENDFORM.
