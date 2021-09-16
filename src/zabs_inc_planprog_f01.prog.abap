*&---------------------------------------------------------------------*
*& Include          ZABS_INC_PLANPROG_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form CHANGE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_screen .



ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_SHIFT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ABAP_TRUE
*&---------------------------------------------------------------------*
FORM f4_for_shift USING lv_low TYPE abap_bool.

*--Local declaration
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  IF gt_shift[] IS INITIAL.
    PERFORM shift_f4 CHANGING gt_shift.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VORNR'
      dynprofield     = 'SO_SHIFT'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      multiple_choice = ' ' "'X'
      value_org       = zcl_abs_abap_maintain=>c_f4_valorg_s
    TABLES
      value_tab       = gt_shift
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    IF lines( lt_return ) GT 1.
      REFRESH so_shift.
      CLEAR so_shift.
      so_shift = 'IEQ'.

      LOOP AT lt_return INTO DATA(lwa_return).
        so_shift-low = lwa_return-fieldval.
        APPEND so_shift.
      ENDLOOP.

      READ TABLE lt_return INTO lwa_return INDEX 1.
      IF sy-subrc EQ 0.
        so_shift-low  = lwa_return-fieldval.
      ENDIF.
    ELSE.
      READ TABLE lt_return INTO lwa_return INDEX 1.
      IF sy-subrc EQ 0.
        READ TABLE so_shift ASSIGNING FIELD-SYMBOL(<ls_shift>) INDEX 1.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE so_shift ASSIGNING <ls_shift>.
        ENDIF.
        IF <ls_shift> IS ASSIGNED.
          <ls_shift> = 'IBT'.
          IF lv_low EQ abap_true.
            so_shift-low = <ls_shift>-low = lwa_return-fieldval.
            IF so_shift-high IS NOT INITIAL.
              <ls_shift>-high = so_shift-high.
            ENDIF.
            IF <ls_shift>-high IS INITIAL.
              <ls_shift>-high = <ls_shift>-low.
            ENDIF.
          ELSE.
            so_shift-high = <ls_shift>-high = lwa_return-fieldval.
            IF so_shift-low IS NOT INITIAL.
              <ls_shift>-low = so_shift-low.
            ENDIF.
            IF <ls_shift>-low IS INITIAL.
              <ls_shift>-low = <ls_shift>-high.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHIFT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_SHIFT
*&---------------------------------------------------------------------*
FORM shift_f4 CHANGING ct_shift TYPE ty_shift_tab.

*--Local declarations
  DATA: lwa_shift TYPE ty_shift,
        lv_datab  TYPE datum,
        lv_datbi  TYPE datum,
        lv_sdate  TYPE datum.

  IF so_date-low IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-low CHANGING lv_datab lv_datbi.
  ENDIF.

  IF so_date-high IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-high CHANGING lv_sdate lv_datbi.
  ENDIF.

*--Fetching Order Numbers
  SELECT SINGLE ordno
    FROM zabst_ordhdr
    INTO @DATA(lv_ordno)
   WHERE werks EQ @p_werks
     AND equnr IN @so_proj[]
     AND datab LE @lv_datab
     AND datbi GE @lv_datbi.

*--Fetching WO Numbers
  SELECT ordno, wonum
    FROM zabst_orditm
    INTO TABLE @DATA(lt_wonum)
   WHERE ordno EQ @lv_ordno.
  IF sy-subrc = 0.
    SORT lt_wonum BY wonum.
  ENDIF.

*--Fetching Operations(Shift numbers)
  SELECT wonum,
         vornr,
         ltxa1
    FROM /agri/fmwoopr
    INTO TABLE @DATA(lt_opr_temp)
    FOR ALL ENTRIES IN @lt_wonum
   WHERE wonum EQ @lt_wonum-wonum.
  IF sy-subrc = 0.
    SORT lt_opr_temp BY wonum.
  ENDIF.

  DATA(lt_opr) = lt_opr_temp.
  SORT lt_opr BY vornr.
  DELETE ADJACENT DUPLICATES FROM lt_opr COMPARING vornr.

  LOOP AT so_shift INTO DATA(ls_shift).
    READ TABLE lt_opr TRANSPORTING NO FIELDS
      WITH KEY vornr = ls_shift-low.
    IF sy-subrc <> 0.
*-- Shift &1 inválido!
      MESSAGE i112(zfmfp) WITH ls_shift-low.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDLOOP.

  IF so_shift[] IS NOT INITIAL.
    DELETE lt_opr WHERE vornr NOT IN so_shift[].
  ENDIF.

  LOOP AT lt_opr INTO DATA(lwa_opr).
    lwa_shift-vornr = lwa_opr-vornr.
    lwa_shift-ltxa1 = lwa_opr-ltxa1.
    APPEND lwa_shift TO ct_shift.
    CLEAR lwa_shift.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_WEEK_DATES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SO_DATE_LOW
*&      <-- LV_DATAB
*&      <-- LV_DATBI
*&---------------------------------------------------------------------*
FORM get_week_dates USING iv_date  TYPE datum
                 CHANGING cv_datab TYPE datum
                          cv_datbi TYPE datum.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
    EXPORTING
      date   = iv_date
    IMPORTING
      monday = cv_datab
      sunday = cv_datbi.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters.

  DATA: lv_first_day TYPE sydatum,
        lv_last_day  TYPE sydatum.

  IF p_werks IS INITIAL.
*-- Informar Centro!
    MESSAGE i385(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_proj[] IS INITIAL.
*-- Informar Projeto!
    MESSAGE i386(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_shift[] IS INITIAL.
*-- Informar Shift!
    MESSAGE i387(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_date[] IS INITIAL.
*-- Informar Período!
    MESSAGE i388(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
*-- Planejamento de Irrigação
    IF p_plan EQ abap_true.
      READ TABLE so_date INTO DATA(ls_date) INDEX 1.
      IF sy-subrc EQ 0.
        IF ls_date-low IS INITIAL.
*-- Informar Data Inicial!
          MESSAGE i389(zfmfp).
          LEAVE LIST-PROCESSING.
        ELSE.
          lv_first_day = ls_date-low(6) && '01'.

          CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
            EXPORTING
              i_day_in            = lv_first_day
            IMPORTING
              e_last_day_of_month = lv_last_day.

          IF ls_date-low NE lv_first_day.
*-- &1 não é o primeiro dia do mês [&2]!
            MESSAGE i391(zfmfp) WITH ls_date-low lv_first_day.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.

        IF ls_date-high IS INITIAL.
*-- Informar Data Final!
          MESSAGE i390(zfmfp).
          LEAVE LIST-PROCESSING.
        ELSE.
          IF ls_date-high NE lv_last_day.
*-- &1 não é o último dia do mês [&2]!
            MESSAGE i392(zfmfp) WITH ls_date-high lv_last_day.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
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
FORM initialize_global_data.

  REFRESH: gt_fmirhdr, gt_fmirhdrt, gt_fmirflo, gt_glflot,
           gt_glflatv, gt_glmdhdr, gt_ordhdr, gt_ordcnf,
           gt_fcat, gt_output, gt_index_rows, gt_glmdhdr_sum,
           gt_clima_hist, gt_output_old, gt_output_new.

  CLEAR: gv_irrtyp, gv_dias, gv_tx_repo.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_IRRIGATION_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_irrigation_data.

  DATA: lt_klah        TYPE tt_klah,
        ls_glmdhdr_sum LIKE LINE OF gt_glmdhdr_sum,
        lref_date      TYPE REF TO data,
        lrt_gyear      TYPE RANGE OF /agri/gyear.

  FIELD-SYMBOLS: <lv_value> TYPE any.

*-- Fetching Equipment header and Equipment master plants data
  SELECT *
    FROM /agri/fmirhdr
    INTO TABLE @gt_fmirhdr
   WHERE equnr IN @so_proj[]
     AND irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp_001
     AND kfrst EQ @abap_false.
  IF sy-subrc NE 0.
*-- Não existem projetos para os parâmetros informados!
    MESSAGE i397(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_fmirhdr BY equnr.

    LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
      INSERT INITIAL LINE INTO TABLE gt_fazendas
        ASSIGNING FIELD-SYMBOL(<ls_fazenda>).
      IF sy-subrc EQ 0.
        <ls_fazenda>-equnr = ls_fmirhdr-equnr.
        <ls_fazenda>-tplma_in = ls_fmirhdr-equnr(6).
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <ls_fazenda>-tplma_in
          IMPORTING
            output     = <ls_fazenda>-tplma_in
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        <ls_fazenda>-tplma_out = ls_fmirhdr-equnr(6).
      ENDIF.
    ENDLOOP.

    SORT gt_fazendas BY equnr tplma_in.
    DELETE ADJACENT DUPLICATES FROM gt_fazendas COMPARING equnr tplma_in.

    SELECT *
      FROM /agri/fmirhdrt
      INTO TABLE @gt_fmirhdrt
      FOR ALL ENTRIES IN @gt_fmirhdr
     WHERE equnr EQ @gt_fmirhdr-equnr
       AND spras EQ @sy-langu.

    SORT gt_fmirhdrt BY equnr.

*-- Fetching Terrain Header data to get terrains
    SELECT *
      FROM /agri/fmirflo
      INTO TABLE @gt_fmirflo
      FOR ALL ENTRIES IN @gt_fmirhdr
     WHERE equnr EQ @gt_fmirhdr-equnr.

    IF sy-subrc NE 0.
*-- Não existem talhões atrelados aos projetos informados!
      MESSAGE i396(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT gt_fmirflo BY equnr tplnr_fl.

*-- Fetching Terrain Header data to get gross terrain arae
      READ TABLE gt_fazendas INTO DATA(ls_fazenda) INDEX 1.
      IF sy-subrc EQ 0.
        SELECT tplnr_fl, pltxt, tplkz, fltyp, tplvl,
               tplma, iwerk, swerk, kokrs, anlnr,
               kostl, garea, kfrst, loevm
          FROM /agri/glflot
          INTO TABLE @gt_glflot
         WHERE tplma    EQ @ls_fazenda-tplma_in
           AND iwerk    EQ @p_werks
           AND kfrst    EQ @abap_false
           AND loevm    EQ @abap_false.
      ENDIF.

      IF gt_glflot[] IS NOT INITIAL.
        SORT gt_glflot BY tplnr_fl.

*-- Fecth all crop season
        SELECT *
          FROM /agri/glflcma
          INTO TABLE @gt_glflcma
          FOR ALL ENTRIES IN @gt_glflot
         WHERE tplnr_fl EQ @gt_glflot-tplnr_fl
           AND astat    EQ 'A'
           AND loevm    EQ @abap_false.

        SORT gt_glflcma BY tplnr_fl contr.

*--Fetch class header data
        SELECT *
          FROM klah
          INTO TABLE @lt_klah
          WHERE class IN ( 'EVAPORATION', 'PRECIPITACION' ).
        IF sy-subrc EQ 0.
*--Calling meathod to get attribute header data
          CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
            EXPORTING
              it_klah  = lt_klah
              i_agtyp  = 'X90'
            IMPORTING
              et_athdr = DATA(lt_athdr).
        ENDIF.

        SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
               h~contr, h~cmnum, h~equnr, h~mpgrp,
               h~mdate, h~mtime, v~atzhl, v~atwrt,
               v~atflv, c~atinn, c~atnam
          INTO TABLE @gt_glmdhdr
          FROM /agri/glmdhdr AS h
          INNER JOIN /agri/glmdatv AS v
          ON v~mdocm EQ h~mdocm
          INNER JOIN cabn AS c
          ON c~atinn EQ v~atinn
          FOR ALL ENTRIES IN @gt_glflot
         WHERE h~mdtyp    EQ 'ZIRG'
           AND h~aslvl    EQ 'I'
           AND h~tplnr_fl EQ @gt_glflot-tplnr_fl
           AND h~mpgrp    IN ( 'EVAPORATION', 'PRECIPITACION' )
           AND h~mdate    IN @so_date[]
           AND h~kfrst    EQ @abap_false
           AND h~canceled EQ @abap_false.

        IF gt_glmdhdr[] IS NOT INITIAL
        AND lt_athdr[] IS NOT INITIAL.
          SORT lt_athdr BY atinn.

          DATA(lt_precip) = gt_glmdhdr[].
          DELETE lt_precip WHERE mpgrp NE 'PRECIPITACION'.
          SORT lt_precip BY atflv DESCENDING.
          DELETE lt_precip WHERE atflv LT 10.
          SORT lt_precip BY mdate.
          DELETE ADJACENT DUPLICATES FROM lt_precip COMPARING mdate.
          LOOP AT lt_precip INTO DATA(ls_precip).
            DELETE gt_glmdhdr WHERE mpgrp EQ 'EVAPORATION'
                                AND mdate EQ ls_precip-mdate.
          ENDLOOP.

          LOOP AT gt_glmdhdr ASSIGNING FIELD-SYMBOL(<ls_glmdhdr>).
            IF <ls_glmdhdr>-atflv IS NOT INITIAL.
              READ TABLE lt_athdr INTO DATA(ls_athdr)
                WITH KEY atinn = <ls_glmdhdr>-atinn BINARY SEARCH.
              IF sy-subrc EQ 0.
                CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
                ASSIGN lref_date->* TO <lv_value>.
                <lv_value> = <ls_glmdhdr>-atflv.
                <ls_glmdhdr>-atwrt = <lv_value>.
                CONDENSE <ls_glmdhdr>-atwrt NO-GAPS.
              ENDIF.
            ELSE.
              CONDENSE <ls_glmdhdr>-atwrt NO-GAPS.
            ENDIF.

            READ TABLE gt_glflot INTO DATA(ls_glflot)
              WITH KEY tplnr_fl = <ls_glmdhdr>-tplnr_fl BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_glmdhdr_sum-tplma_in = ls_glflot-tplma.

              CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                EXPORTING
                  input      = ls_glmdhdr_sum-tplma_in
                IMPORTING
                  output     = ls_glmdhdr_sum-tplma_out
                EXCEPTIONS
                  not_found  = 1
                  not_active = 2
                  OTHERS     = 3.
            ENDIF.

            ls_glmdhdr_sum-qtd_doctos = 1.
            ls_glmdhdr_sum-mpgrp = <ls_glmdhdr>-mpgrp.
            ls_glmdhdr_sum-atflv = <ls_glmdhdr>-atflv.
            ls_glmdhdr_sum-atinn = <ls_glmdhdr>-atinn.
            ls_glmdhdr_sum-atnam = <ls_glmdhdr>-atnam.
            COLLECT ls_glmdhdr_sum INTO gt_glmdhdr_sum.
          ENDLOOP.

          SORT gt_glmdhdr_sum BY tplma_in mpgrp atnam.
        ENDIF.

*--Fetching Terrain Attribute Values to get Crop Coefficient value
        SELECT g~tplnr_fl, g~atwrt,
               g~atflv, c~atinn
          FROM /agri/glflatv AS g
          JOIN cabn AS c
          ON c~atinn EQ g~atinn
          INTO TABLE @gt_glflatv
          FOR ALL ENTRIES IN @gt_glflot
         WHERE g~tplnr_fl  EQ @gt_glflot-tplnr_fl
           AND g~class     EQ @zcl_abs_abap_maintain=>c_class_info "'INFO_PROJETO'
           AND g~deleted   EQ @space
           AND c~atnam     EQ @zcl_abs_abap_maintain=>c_charact_coef. "'COEF_CULTURA'
        IF sy-subrc EQ 0.
          SORT gt_glflatv BY tplnr_fl.

          REFRESH: lt_klah, lt_athdr.
*--Fetch class header data
          SELECT *
            FROM klah
            INTO TABLE @lt_klah
           WHERE class EQ 'INFO_PROJETO'.
          IF sy-subrc EQ 0.
*--Calling meathod to get attribute header data
            CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
              EXPORTING
                it_klah  = lt_klah
                i_agtyp  = 'X91'
              IMPORTING
                et_athdr = lt_athdr.
          ENDIF.

          IF lt_athdr[] IS NOT INITIAL.
            LOOP AT gt_glflatv ASSIGNING FIELD-SYMBOL(<ls_glflatv>).
              IF <ls_glflatv>-atflv IS NOT INITIAL.
                READ TABLE lt_athdr INTO ls_athdr
                  WITH KEY atinn = <ls_glflatv>-atinn BINARY SEARCH.
                IF sy-subrc EQ 0.
                  CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
                  ASSIGN lref_date->* TO <lv_value>.
                  <lv_value> = <ls_glflatv>-atflv.
                  <ls_glflatv>-atwrt = <lv_value>.
                  CONDENSE <ls_glflatv>-atwrt NO-GAPS.
                ENDIF.
              ELSE.
                CONDENSE <ls_glflatv>-atwrt NO-GAPS.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gt_fazendas[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lrt_gyear
        ASSIGNING FIELD-SYMBOL(<lrs_gyear>).
      IF sy-subrc EQ 0.
        <lrs_gyear> = 'IBT'.
        <lrs_gyear>-low = so_date[ 1 ]-low(4).
        <lrs_gyear>-low = <lrs_gyear>-low - 4.
        <lrs_gyear>-high = so_date[ 1 ]-low(4).
        <lrs_gyear>-high = <lrs_gyear>-high - 1.

        SELECT *
          FROM zabst_clima_hist
          INTO TABLE @gt_clima_hist
          FOR ALL ENTRIES IN @gt_fazendas
         WHERE tplnr_fl EQ @gt_fazendas-tplma_in
           AND ano      IN @lrt_gyear[].

        SORT gt_clima_hist BY tplnr_fl tipo.
      ENDIF.
    ENDIF.
  ENDIF.

*-- Fetching Order Numbers
  SELECT *
    FROM zabst_ordhdr
    INTO TABLE @gt_ordhdr
   WHERE equnr IN @so_proj[]
     AND werks EQ @p_werks.

  IF sy-subrc EQ 0.
    SORT gt_ordhdr BY equnr ordno.

*-- Fetching Work Order Confirmation data
    SELECT *
      FROM zabst_ordcnf
      INTO TABLE @gt_ordcnf
       FOR ALL ENTRIES IN @gt_ordhdr
     WHERE ordno EQ @gt_ordhdr-ordno
       AND vornr IN @so_shift[]
       AND loevm EQ @abap_false.

    SORT gt_ordcnf BY ordno.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM controls_display.

*--Local table declaration
  DATA: lt_fcat    TYPE lvc_t_fcat,
*--Workarea declaration
        ls_variant TYPE disvariant,
        ls_stable  TYPE lvc_s_stbl,
        ls_layout  TYPE lvc_s_layo,
*--Variables
        lv_valid   TYPE char01,
        lv_input   TYPE i.

*--Set ALV attributes for layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
  ls_layout-smalltitle = abap_true.

  IF gobj_alv IS NOT BOUND.
*--Create Object for custom container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'C_MDMS_APR_0100_CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_alv
      EXPORTING
        i_parent = gobj_cont.
  ENDIF.

*--Displaying ALV Data
  IF gobj_alv IS NOT INITIAL.
    IF gt_output[] IS NOT INITIAL.
      ls_variant-report = sy-repid.
      CALL METHOD gobj_alv->set_table_for_first_display
        EXPORTING
          is_variant                    = ls_variant
          i_save                        = 'A'
          is_layout                     = ls_layout
        CHANGING
          it_outtab                     = gt_output
          it_fieldcatalog               = gt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE i035(zabs_msgcls).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
*-- Não existem dados para os parâmetros informados!
      MESSAGE i398(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DYNAMIC_FCAT_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM dynamic_fcat_prepare .

*--Field catalog prepare
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABST_PLANEJ'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      IF <ls_fcat>-fieldname = 'MANDT'
      OR <ls_fcat>-fieldname = 'CHUVA_IRRIG'
      OR <ls_fcat>-fieldname = 'ERNAM'
      OR <ls_fcat>-fieldname = 'ERDAT'
      OR <ls_fcat>-fieldname = 'ERZET'
      OR <ls_fcat>-fieldname = 'DAY_IN_WEEK'
      OR <ls_fcat>-fieldname = 'PROGRAMADO'
      OR <ls_fcat>-fieldname = 'AENAM'
      OR <ls_fcat>-fieldname = 'AEDAT'
      OR <ls_fcat>-fieldname = 'AEZET'.
        <ls_fcat>-no_out = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_IRRIGATION_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_irrigation_data .
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA: lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  IF ok_code IS NOT INITIAL.
    fcode = ok_code.
    CLEAR: ok_code.

    IF fcode(2) EQ 'T\'.
      PERFORM fcode_processing_tabstrip.
    ELSE.
      CONCATENATE lv_subroutine fcode INTO lv_subroutine.
      PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                              IF FOUND.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Back Button
*&---------------------------------------------------------------------*
FORM fcode_back.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_IRRTYP
*&---------------------------------------------------------------------*
*& Define irrigation type
*&---------------------------------------------------------------------*
FORM fcode_irrtyp.
  PERFORM define_irrtyp.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_TRANSF
*&---------------------------------------------------------------------*
*& Transfer irrigation type
*&---------------------------------------------------------------------*
FORM fcode_transf.

  DATA: lv_subrc TYPE sysubrc.

  PERFORM check_input_val CHANGING lv_subrc.
  IF lv_subrc EQ 0.
    PERFORM transfer_irrtyp.
    PERFORM fcode_back.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form FCODE_SAVE
*&---------------------------------------------------------------------*
*& Save irrigation planning
*&---------------------------------------------------------------------*
FORM fcode_save.

  PERFORM save_irrigation_planning.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form OUTPUT_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM output_prepare.

  TYPES: BEGIN OF ly_fmirflo_sum,
           equnr  TYPE /agri/glequnr,
           garea1 TYPE /agri/glgarea,
           garea2 TYPE /agri/glgarea,
           garea3 TYPE /agri/glgarea,
           garea4 TYPE /agri/glgarea,
           garea5 TYPE /agri/glgarea,
           garea6 TYPE /agri/glgarea,
         END OF ly_fmirflo_sum,

         BEGIN OF ly_ppsortkey,
           equnr          TYPE zabs_del_projeto,
           variedade1     TYPE zabs_del_key_variedade,
           variedade2     TYPE zabs_del_key_variedade,
           variedade3     TYPE zabs_del_key_variedade,
           variedade4     TYPE zabs_del_key_variedade,
           variedade5     TYPE zabs_del_key_variedade,
           variedade6     TYPE zabs_del_key_variedade,
           porta_enxerto1 TYPE zabs_del_key_enxerto,
           porta_enxerto2 TYPE zabs_del_key_enxerto,
           porta_enxerto3 TYPE zabs_del_key_enxerto,
           porta_enxerto4 TYPE zabs_del_key_enxerto,
           porta_enxerto5 TYPE zabs_del_key_enxerto,
           porta_enxerto6 TYPE zabs_del_key_enxerto,
           evaporation1   TYPE atflv,
           evaporation2   TYPE atflv,
           evaporation3   TYPE atflv,
           evaporation4   TYPE atflv,
           evaporation5   TYPE atflv,
           evaporation6   TYPE atflv,
           qtd_doc_evapo1 TYPE i,
           qtd_doc_evapo2 TYPE i,
           qtd_doc_evapo3 TYPE i,
           qtd_doc_evapo4 TYPE i,
           qtd_doc_evapo5 TYPE i,
           qtd_doc_evapo6 TYPE i,
           precipitacion1 TYPE atflv,
           precipitacion2 TYPE atflv,
           precipitacion3 TYPE atflv,
           precipitacion4 TYPE atflv,
           precipitacion5 TYPE atflv,
           precipitacion6 TYPE atflv,
           qtd_doc_preci1 TYPE i,
           qtd_doc_preci2 TYPE i,
           qtd_doc_preci3 TYPE i,
           qtd_doc_preci4 TYPE i,
           qtd_doc_preci5 TYPE i,
           qtd_doc_preci6 TYPE i,
         END OF ly_ppsortkey,

         BEGIN OF ly_media_hist,
           count     TYPE i,
           tplma_in  TYPE /agri/gltplma,
           tplma_out TYPE /agri/gltplma,
           equnr     TYPE /agri/glequnr,
           tipo      TYPE zabs_del_tipo,
           mes01     TYPE atflv, "zabs_del_mes1,
           mes02     TYPE atflv, "zabs_del_mes2,
           mes03     TYPE atflv, "zabs_del_mes3,
           mes04     TYPE atflv, "zabs_del_mes4,
           mes05     TYPE atflv, "zabs_del_mes5,
           mes06     TYPE atflv, "zabs_del_mes6,
           mes07     TYPE atflv, "zabs_del_mes7,
           mes08     TYPE atflv, "zabs_del_mes8,
           mes09     TYPE atflv, "zabs_del_mes9,
           mes10     TYPE atflv, "zabs_del_mes10,
           mes11     TYPE atflv, "zabs_del_mes11,
           mes12     TYPE atflv, "zabs_del_mes12,
         END OF ly_media_hist.

  DATA: lt_fmirflo_sum    TYPE STANDARD TABLE OF ly_fmirflo_sum INITIAL SIZE 0,
        lt_ppsortkey      TYPE STANDARD TABLE OF ly_ppsortkey INITIAL SIZE 0,
        lt_media_hist     TYPE STANDARD TABLE OF ly_media_hist,
        ls_media_hist     LIKE LINE OF lt_media_hist,
        lt_new_output     LIKE gt_output,
        ls_new_output     LIKE LINE OF lt_new_output,
        ls_ppsortkey      LIKE LINE OF lt_ppsortkey,
        lt_dates          TYPE /scwm/tt_lm_dates,
        ls_fmirflo_sum    LIKE LINE OF lt_fmirflo_sum,
        ls_glflot         LIKE LINE OF gt_glflot,
        lv_shift_area     TYPE fieldname,
        lv_shift_value    TYPE fieldname,
        lv_media_chuva    TYPE atflv,
        lv_media_evapo    TYPE atflv,
        lv_null_date      TYPE sydatum,
        lv_day_in_week    TYPE p,
        lv_number_of_days TYPE p,
        lv_aux1           TYPE f,
        lv_aux2           TYPE f,
        lv_fieldname      TYPE fieldname.

  FIELD-SYMBOLS: <lv_media_chuva> TYPE atflv,
                 <lv_media_evapo> TYPE atflv.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  READ TABLE so_date INTO DATA(ls_date_range) INDEX 1.
  IF sy-subrc EQ 0.
    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = ls_date_range-low
        iv_endda = ls_date_range-high
      IMPORTING
        et_dates = lt_dates.
  ENDIF.

  LOOP AT gt_fmirflo INTO DATA(ls_fmirflo).
    READ TABLE gt_glflot INTO ls_glflot
      WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_glflot.
    ENDIF.

    DATA(ls_fmirflo_aux) = ls_fmirflo.
    AT NEW equnr.
      CLEAR: ls_ppsortkey, ls_fmirflo_sum.
    ENDAT.

    ls_fmirflo_sum-equnr = ls_fmirflo_aux-equnr.
    DO 6 TIMES.
      DATA(lv_field_index) = sy-index.
      lv_shift_value = 'ZZCSHIFT' && lv_field_index.
      ASSIGN COMPONENT lv_shift_value OF STRUCTURE ls_fmirflo_aux
        TO FIELD-SYMBOL(<lv_shift_value>).
      IF sy-subrc EQ 0.
        lv_shift_area = 'GAREA' && lv_field_index.
        ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
          TO FIELD-SYMBOL(<lv_shift_area>).
        IF sy-subrc EQ 0.
          IF <lv_shift_value> IS NOT INITIAL.
            <lv_shift_area> = <lv_shift_area> + ls_glflot-garea.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift1
                             CHANGING ls_ppsortkey-variedade1
                                      ls_ppsortkey-porta_enxerto1
                                      ls_ppsortkey-evaporation1
                                      ls_ppsortkey-precipitacion1
                                      ls_ppsortkey-qtd_doc_evapo1
                                      ls_ppsortkey-qtd_doc_preci1.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift2
                             CHANGING ls_ppsortkey-variedade2
                                      ls_ppsortkey-porta_enxerto2
                                      ls_ppsortkey-evaporation2
                                      ls_ppsortkey-precipitacion2
                                      ls_ppsortkey-qtd_doc_evapo2
                                      ls_ppsortkey-qtd_doc_preci2.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift3
                             CHANGING ls_ppsortkey-variedade3
                                      ls_ppsortkey-porta_enxerto3
                                      ls_ppsortkey-evaporation3
                                      ls_ppsortkey-precipitacion3
                                      ls_ppsortkey-qtd_doc_evapo3
                                      ls_ppsortkey-qtd_doc_preci3.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift4
                             CHANGING ls_ppsortkey-variedade4
                                      ls_ppsortkey-porta_enxerto4
                                      ls_ppsortkey-evaporation4
                                      ls_ppsortkey-precipitacion4
                                      ls_ppsortkey-qtd_doc_evapo4
                                      ls_ppsortkey-qtd_doc_preci4.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift5
                             CHANGING ls_ppsortkey-variedade5
                                      ls_ppsortkey-porta_enxerto5
                                      ls_ppsortkey-evaporation5
                                      ls_ppsortkey-precipitacion5
                                      ls_ppsortkey-qtd_doc_evapo5
                                      ls_ppsortkey-qtd_doc_preci5.

    PERFORM check_shift_variety USING ls_fmirflo_aux
                                      ls_fmirflo_aux-zzcshift6
                             CHANGING ls_ppsortkey-variedade6
                                      ls_ppsortkey-porta_enxerto6
                                      ls_ppsortkey-evaporation6
                                      ls_ppsortkey-precipitacion6
                                      ls_ppsortkey-qtd_doc_evapo6
                                      ls_ppsortkey-qtd_doc_preci6.

    AT END OF equnr.
      ls_ppsortkey-equnr = ls_fmirflo_aux-equnr.
      APPEND ls_ppsortkey TO lt_ppsortkey.
      APPEND ls_fmirflo_sum TO lt_fmirflo_sum.
    ENDAT.
  ENDLOOP.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  SORT: lt_fmirflo_sum BY equnr,
        lt_ppsortkey BY equnr,
        gt_fazendas BY tplma_in.

  LOOP AT gt_clima_hist INTO DATA(ls_clima_hist).
    ls_media_hist-count = 1.
    ls_media_hist-tplma_in = ls_clima_hist-tplnr_fl.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input      = ls_media_hist-tplma_in
      IMPORTING
        output     = ls_media_hist-tplma_out
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.
    ls_media_hist-tipo  = ls_clima_hist-tipo.
    ls_media_hist-mes01 = ls_clima_hist-mes01.
    ls_media_hist-mes02 = ls_clima_hist-mes02.
    ls_media_hist-mes03 = ls_clima_hist-mes03.
    ls_media_hist-mes04 = ls_clima_hist-mes04.
    ls_media_hist-mes05 = ls_clima_hist-mes05.
    ls_media_hist-mes06 = ls_clima_hist-mes06.
    ls_media_hist-mes07 = ls_clima_hist-mes07.
    ls_media_hist-mes08 = ls_clima_hist-mes08.
    ls_media_hist-mes09 = ls_clima_hist-mes09.
    ls_media_hist-mes10 = ls_clima_hist-mes10.
    ls_media_hist-mes11 = ls_clima_hist-mes11.
    ls_media_hist-mes12 = ls_clima_hist-mes12.
    COLLECT ls_media_hist INTO lt_media_hist.
  ENDLOOP.

*  LOOP AT lt_media_hist ASSIGNING FIELD-SYMBOL(<ls_media_hist>).
*    IF <ls_media_hist>-count IS INITIAL.
*      <ls_media_hist>-count = 1.
*    ENDIF.
*    <ls_media_hist>-mes01 = <ls_media_hist>-mes01 / <ls_media_hist>-count.
*    <ls_media_hist>-mes02 = <ls_media_hist>-mes02 / <ls_media_hist>-count.
*    <ls_media_hist>-mes03 = <ls_media_hist>-mes03 / <ls_media_hist>-count.
*    <ls_media_hist>-mes04 = <ls_media_hist>-mes04 / <ls_media_hist>-count.
*    <ls_media_hist>-mes05 = <ls_media_hist>-mes05 / <ls_media_hist>-count.
*    <ls_media_hist>-mes06 = <ls_media_hist>-mes06 / <ls_media_hist>-count.
*    <ls_media_hist>-mes07 = <ls_media_hist>-mes07 / <ls_media_hist>-count.
*    <ls_media_hist>-mes08 = <ls_media_hist>-mes08 / <ls_media_hist>-count.
*    <ls_media_hist>-mes09 = <ls_media_hist>-mes09 / <ls_media_hist>-count.
*    <ls_media_hist>-mes10 = <ls_media_hist>-mes10 / <ls_media_hist>-count.
*    <ls_media_hist>-mes11 = <ls_media_hist>-mes11 / <ls_media_hist>-count.
*    <ls_media_hist>-mes12 = <ls_media_hist>-mes12 / <ls_media_hist>-count.
*  ENDLOOP.

  SORT lt_media_hist BY equnr tipo.

  LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
    CLEAR: ls_glflot, lv_media_chuva, lv_media_evapo, lv_aux1, lv_aux2.
    READ TABLE lt_fmirflo_sum INTO ls_fmirflo_sum
      WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_fmirhdrt INTO DATA(ls_fmirhdrt)
        WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_fmirhdrt.
      ENDIF.

      READ TABLE lt_ppsortkey INTO ls_ppsortkey
        WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_ppsortkey.
      ENDIF.
*-- Ler talhão aleatório pertencente ao projeto/equipamento
      READ TABLE gt_fmirflo INTO ls_fmirflo
        WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE gt_glflot INTO ls_glflot
          WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR ls_glflot.
        ENDIF.

        READ TABLE gt_glflatv INTO DATA(ls_glflatv)
          WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR ls_glflatv.
        ENDIF.
      ELSE.
        CLEAR ls_glflatv.
      ENDIF.

      lv_fieldname = 'MES' && so_date[ 1 ]-low+4(2).

      UNASSIGN <lv_media_evapo>.
      READ TABLE lt_media_hist INTO DATA(ls_media_evapo)
        WITH KEY tplma_in = ls_glflot-tplma
                 tipo     = 'EVAPO'.
      IF sy-subrc NE 0.
        CLEAR ls_media_evapo.
      ELSE.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_media_evapo
          TO <lv_media_evapo>.
      ENDIF.

      UNASSIGN <lv_media_chuva>.
      READ TABLE lt_media_hist INTO DATA(ls_media_chuva)
        WITH KEY tplma_in = ls_glflot-tplma
                 tipo     = 'CHUVA'.
      IF sy-subrc NE 0.
        CLEAR ls_media_chuva.
      ELSE.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_media_chuva
          TO <lv_media_chuva>.
      ENDIF.

      DO 6 TIMES.
        lv_field_index = sy-index.
        lv_shift_value = 'ZZCSHIFT' && lv_field_index.
        lv_shift_area = 'GAREA' && lv_field_index.
        ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
          TO FIELD-SYMBOL(<lv_fieldval>).
        IF sy-subrc EQ 0.
          IF <lv_fieldval> IS NOT INITIAL.
            INSERT INITIAL LINE INTO TABLE gt_output
              ASSIGNING FIELD-SYMBOL(<ls_output>).
            IF sy-subrc EQ 0.
*-- Centro/Fazenda
              <ls_output>-werks = p_werks.
*-- Projeto/Código Equipamento
              <ls_output>-equnr = ls_fmirhdr-equnr.
*-- Descrição Projeto/Equipamento
              <ls_output>-descr = ls_fmirhdrt-descr.
              CASE lv_field_index.
                WHEN 1. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-1.
                  <ls_output>-variedade = ls_ppsortkey-variedade1.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto1.
*-- Vazão (m3/h)
                  <ls_output>-vazao = ls_fmirhdr-zzvns01.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo1 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation1 /
                                            ls_ppsortkey-qtd_doc_evapo1.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci1 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion1 /
                                            ls_ppsortkey-qtd_doc_preci1.
                  ENDIF.
                WHEN 2. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-2.
                  <ls_output>-variedade = ls_ppsortkey-variedade2.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto2.
                  <ls_output>-vazao = ls_fmirhdr-zzvns02.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo2 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation2 /
                                            ls_ppsortkey-qtd_doc_evapo2.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci2 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion2 /
                                            ls_ppsortkey-qtd_doc_preci2.
                  ENDIF.
                WHEN 3. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-3.
                  <ls_output>-variedade = ls_ppsortkey-variedade3.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto3.
                  <ls_output>-vazao = ls_fmirhdr-zzvns03.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo3 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation3 /
                                            ls_ppsortkey-qtd_doc_evapo3.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci3 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion3 /
                                            ls_ppsortkey-qtd_doc_preci3.
                  ENDIF.
                WHEN 4. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-4.
                  <ls_output>-variedade = ls_ppsortkey-variedade4.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto4.
                  <ls_output>-vazao = ls_fmirhdr-zzvns04.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo4 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation4 /
                                            ls_ppsortkey-qtd_doc_evapo4.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci4 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion4 /
                                            ls_ppsortkey-qtd_doc_preci4.
                  ENDIF.
                WHEN 5. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-5.
                  <ls_output>-variedade = ls_ppsortkey-variedade5.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto5.
                  <ls_output>-vazao = ls_fmirhdr-zzvns05.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo5 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation5 /
                                            ls_ppsortkey-qtd_doc_evapo5.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci5 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion5 /
                                            ls_ppsortkey-qtd_doc_preci5.
                  ENDIF.
                WHEN 6. "Operação/Shift + Variedade + Porta-Enxerto
                  <ls_output>-shift = c_shift-6.
                  <ls_output>-variedade = ls_ppsortkey-variedade6.
                  <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto6.
                  <ls_output>-vazao = ls_fmirhdr-zzvns06.
*-- Evaporação Mês
                  IF ls_ppsortkey-qtd_doc_evapo6 IS NOT INITIAL.
                    <ls_output>-evapo_mes = ls_ppsortkey-evaporation6 /
                                            ls_ppsortkey-qtd_doc_evapo6.
                  ENDIF.
*-- Chuva Mês
                  IF ls_ppsortkey-qtd_doc_preci6 IS NOT INITIAL.
                    <ls_output>-chuva_mes = ls_ppsortkey-precipitacion6 /
                                            ls_ppsortkey-qtd_doc_preci6.
                  ENDIF.
              ENDCASE.
*-- Evaporação Mês
              IF <lv_media_evapo> IS ASSIGNED.
                ADD <lv_media_evapo> TO <ls_output>-evapo_mes.
*                ADD 1 TO ls_media_evapo-count.
*                <ls_output>-evapo_mes = <ls_output>-evapo_mes /
*                                        ls_media_evapo-count.
                DATA(lv_count_evapo) = ls_media_evapo-count + 1.
                <ls_output>-evapo_mes = <ls_output>-evapo_mes /
                                        lv_count_evapo.
              ENDIF.
*-- Chuva Mês
              IF <lv_media_chuva> IS ASSIGNED.
                ADD <lv_media_chuva> TO <ls_output>-chuva_mes.
*                ADD 1 TO ls_media_chuva-count.
*                <ls_output>-chuva_mes = <ls_output>-chuva_mes /
*                                        ls_media_chuva-count.
                DATA(lv_count_chuva) = ls_media_chuva-count + 1.
                <ls_output>-chuva_mes = <ls_output>-chuva_mes /
                                        lv_count_chuva.
              ENDIF.
**-- Total Dias Mês
*              CLEAR lv_number_of_days.
*              CALL FUNCTION 'HR_E_NUM_OF_DAYS_OF_MONTH'
*                EXPORTING
*                  p_fecha        = <ls_output>-irrdate
*                IMPORTING
*                  number_of_days = lv_number_of_days.
*              <ls_output>-total_d_mes = lv_number_of_days.

*              PERFORM calculate CHANGING <ls_output>.
*-- Área Shift
              ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
                TO <lv_shift_area>.
              IF sy-subrc EQ 0.
                <ls_output>-garea = <lv_shift_area>.
              ENDIF.
*-- Final de Semana
              <ls_output>-weekend = ls_fmirhdr-weekend.
*-- Chuva+irrig (mm) <- Factor_Hidrometro
              <ls_output>-chuva_irrig = ls_fmirhdr-zzhydfac.
*-- Lâmina (mm/h)
              <ls_output>-lamina = ls_fmirhdr-lamina.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDLOOP.
*----------------------*
* Qtde. dias           |
*----------------------*
*|1|2ter|    |    |    |
*|2|2ter|5sex|    |    |
*|3|1seg|3qua|5sex|    |
*|4|1seg|2ter|4qui|5sex|
*----------------------*
  LOOP AT lt_dates INTO DATA(ls_date).
    DATA(lt_output_aux) = gt_output[].
    ls_new_output-irrdate = ls_date.
*-- Semana.Ano
    CALL FUNCTION '/AGRI/G_GET_WEEK_BASED_ON_DATE'
      EXPORTING
        i_date = ls_new_output-irrdate
      IMPORTING
        e_week = ls_new_output-calweek.

    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = ls_new_output-irrdate
      IMPORTING
        wotnr = lv_day_in_week.

    ls_new_output-day_in_week = lv_day_in_week.

*-- Total Dias Mês
    CLEAR lv_number_of_days.
    CALL FUNCTION 'HR_E_NUM_OF_DAYS_OF_MONTH'
      EXPORTING
        p_fecha        = ls_new_output-irrdate
      IMPORTING
        number_of_days = lv_number_of_days.
    ls_new_output-total_d_mes = lv_number_of_days.

*    PERFORM check_days CHANGING <ls_output>.

    MODIFY lt_output_aux FROM ls_new_output
      TRANSPORTING irrdate calweek total_h_dia day_in_week total_d_mes
      WHERE irrdate = lv_null_date.
    APPEND LINES OF lt_output_aux TO lt_new_output.
  ENDLOOP.

  gt_output[] = lt_new_output[].

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_SHIFT_VARIETY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FMIRFLO_AUX_ZZCSHIFT1
*&      <-- LS_PPSORTKEY_VARIEDADE1
*&---------------------------------------------------------------------*
FORM check_shift_variety USING    ls_fmirflo       TYPE /agri/fmirflo
                                  lv_shift         TYPE zabs_del_cshift1
                         CHANGING lv_variedade     TYPE zabs_del_key_variedade
                                  lv_porta_enxerto TYPE zabs_del_key_enxerto
                                  lv_evaporation   TYPE atflv
                                  lv_precipitacion TYPE atflv
                                  lv_qtd_doc_evapo TYPE i
                                  lv_qtd_doc_preci TYPE i.

  READ TABLE gt_glflcma INTO DATA(ls_glflcma)
    WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = ls_glflcma-ymatnr
      IMPORTING
        output = ls_glflcma-ymatnr.

    IF lv_variedade IS NOT INITIAL.
      lv_variedade = lv_variedade && ';' && ls_glflcma-ymatnr.
    ELSE.
      lv_variedade = ls_glflcma-ymatnr.
    ENDIF.
  ENDIF.

  READ TABLE gt_glflot INTO DATA(ls_glflot)
    WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE gt_glmdhdr_sum INTO DATA(ls_glmdhdr_sum)
      WITH KEY tplma_in = ls_glflot-tplma
               mpgrp    = 'EVAPORATION'
               atnam    = 'EVAPORATION' BINARY SEARCH.
    IF sy-subrc EQ 0.
*      lv_evaporation = lv_evaporation + ls_glmdhdr_sum-atflv.
*      lv_qtd_doc_evapo = lv_qtd_doc_evapo + ls_glmdhdr_sum-qtd_doctos.
      lv_evaporation = ls_glmdhdr_sum-atflv.
      lv_qtd_doc_evapo = ls_glmdhdr_sum-qtd_doctos.
    ENDIF.

    READ TABLE gt_glmdhdr_sum INTO ls_glmdhdr_sum
      WITH KEY tplma_in = ls_glflot-tplma
               mpgrp    = 'PRECIPITACION'
               atnam    = 'PRECIPITACION' BINARY SEARCH.
    IF sy-subrc EQ 0.
*      lv_precipitacion = lv_precipitacion + ls_glmdhdr_sum-atflv.
*      lv_qtd_doc_preci = lv_qtd_doc_preci + ls_glmdhdr_sum-qtd_doctos.
      lv_precipitacion = ls_glmdhdr_sum-atflv.
      lv_qtd_doc_preci = ls_glmdhdr_sum-qtd_doctos.
    ENDIF.
  ENDIF.

  lv_porta_enxerto = lv_variedade.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_SHIFT_VARIETY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FMIRFLO_AUX_ZZCSHIFT1
*&      <-- LS_PPSORTKEY_VARIEDADE1
*&---------------------------------------------------------------------*
FORM check_shift_variety_for_prog USING ls_fmirflo       TYPE /agri/fmirflo
                                        lv_shift         TYPE zabs_del_cshift1
                                        lv_irrdate       TYPE sydatum
                               CHANGING lv_variedade     TYPE zabs_del_key_variedade
                                        lv_porta_enxerto TYPE zabs_del_key_enxerto
                                        lv_evaporation   TYPE atflv
                                        lv_precipitacion TYPE atflv
                                        lv_qtd_doc_evapo TYPE i
                                        lv_qtd_doc_preci TYPE i.

  READ TABLE gt_glflcma INTO DATA(ls_glflcma)
    WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = ls_glflcma-ymatnr
      IMPORTING
        output = ls_glflcma-ymatnr.

    IF lv_variedade IS NOT INITIAL.
      lv_variedade = lv_variedade && ';' && ls_glflcma-ymatnr.
    ELSE.
      lv_variedade = ls_glflcma-ymatnr.
    ENDIF.
  ENDIF.

  READ TABLE gt_glflot INTO DATA(ls_glflot)
    WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE gt_glmdhdr_sum INTO DATA(ls_glmdhdr_sum)
      WITH KEY irrdate  = lv_irrdate
               tplma_in = ls_glflot-tplma
               mpgrp    = 'EVAPORATION'
               atnam    = 'EVAPORATION' BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_evaporation = lv_evaporation + ls_glmdhdr_sum-atflv.
      lv_qtd_doc_evapo = lv_qtd_doc_evapo + ls_glmdhdr_sum-qtd_doctos.
    ENDIF.

    READ TABLE gt_glmdhdr_sum INTO ls_glmdhdr_sum
      WITH KEY irrdate  = lv_irrdate
               tplma_in = ls_glflot-tplma
               mpgrp    = 'PRECIPITACION'
               atnam    = 'PRECIPITACION' BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_precipitacion = lv_precipitacion + ls_glmdhdr_sum-atflv.
      lv_qtd_doc_preci = lv_qtd_doc_preci + ls_glmdhdr_sum-qtd_doctos.
    ENDIF.
  ENDIF.

  lv_porta_enxerto = lv_variedade.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DEFINE_IRRTYP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM define_irrtyp .

*--Internal table declaration
  DATA: lt_index_rows  TYPE TABLE OF lvc_s_row.

  REFRESH gt_index_rows.
  CLEAR: gv_irrtyp, gv_dias, gv_tx_repo.

  CALL METHOD gobj_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = DATA(lt_row_no).

  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save.

  IF lt_index_rows[] IS INITIAL.
*-- Selecione pelo menos uma linha!
    MESSAGE i399(zfmfp).
  ELSE.
    gt_index_rows[] = lt_index_rows[].
*--Calling screen to change irrigation type
    CALL SCREEN 0200 STARTING AT 25 5.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FCODE_EXCLUDES
*&---------------------------------------------------------------------*
FORM fcode_excludes_prepare CHANGING lt_fcode_excludes TYPE ui_functions.

*-- Planejamento de Irrigação
  IF p_plan EQ abap_true.
*
*-- Programação de Irrigação
  ELSEIF p_prog EQ abap_true.
    APPEND 'IRRTYP' TO lt_fcode_excludes.
*    APPEND 'SAVE' TO lt_fcode_excludes.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM title_set .

  CASE sy-dynnr.
    WHEN '0100'.
      SET TITLEBAR 'T100'.
    WHEN '0200'.
      SET TITLEBAR 'T200'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM status_set .

  DATA: lt_fcode_excludes TYPE ui_functions.

  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN '0100'.
      PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
    WHEN '0200'.
      SET PF-STATUS 'S200'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TRANSFER_IRRTYP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM transfer_irrtyp .

  DATA: lt_total LIKE gt_output,
        ls_total LIKE LINE OF lt_total.

  LOOP AT gt_index_rows INTO DATA(ls_row).
    READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<ls_output>)
     INDEX ls_row-index.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    <ls_output>-irrtyp = gv_irrtyp.

    IF gv_irrtyp EQ 'PLENO'.
      IF <ls_output>-weekend EQ 'SIM'.
        <ls_output>-qtd_dias = 7.
      ELSEIF <ls_output>-weekend EQ 'NÃO'.
        <ls_output>-qtd_dias = 5.
      ENDIF.
    ELSE.
      <ls_output>-qtd_dias = gv_dias.
    ENDIF.

    <ls_output>-taxa_reposicao = gv_tx_repo / 100.

    PERFORM calculate CHANGING <ls_output>.
    PERFORM check_days CHANGING <ls_output>.

    ls_total-equnr = <ls_output>-equnr.
    ls_total-shift = <ls_output>-shift.
    ls_total-total_h_mes = <ls_output>-total_h_dia.
    COLLECT ls_total INTO lt_total.
  ENDLOOP.

  SORT lt_total BY equnr shift.

  LOOP AT gt_output ASSIGNING <ls_output>.
    READ TABLE lt_total INTO ls_total
      WITH KEY equnr = <ls_output>-equnr
               shift = <ls_output>-shift BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_output>-total_h_mes = ls_total-total_h_mes.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing_tabstrip .

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.



ENDFORM.                    " FCODE_PROCESSING_TABSTRIP

*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_VAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM check_input_val CHANGING lv_subrc TYPE sysubrc.

*  IF gv_dias GT 7.
**-- Limite máximo de 7 dias excedido!
*    MESSAGE i400(zfmfp).
*    lv_subrc = 4.
*  ENDIF.

  IF gv_irrtyp EQ 'PLENO'.
    IF gv_dias IS NOT INITIAL.
*-- Para Tipo Irrigação 'PLENO', não é permitido informar qtde. de dias!
      MESSAGE i424(zfmfp).
      lv_subrc = 4.
    ENDIF.
  ELSE.
    IF gv_dias GT 4.
*-- Limite máximo de 4 dias excedido!
      MESSAGE i403(zfmfp).
      lv_subrc = 4.
    ENDIF.
  ENDIF.

  IF gv_tx_repo NOT BETWEEN 0 AND 100.
*-- A taxa de reposição % deve estar compreendido entre os valores 0 e 100!
    MESSAGE i401(zfmfp).
    lv_subrc = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_IRRIGATION_PLANNING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_irrigation_planning .

  DATA: ls_planej TYPE zabst_planej.

  IF gt_output[] IS NOT INITIAL.
*-- Planejamento de Irrigação
    IF p_plan EQ abap_true.
      ls_planej-ernam = sy-uname.
      ls_planej-erdat = sy-datum.
      ls_planej-erzet = sy-uzeit.
      MODIFY gt_output FROM ls_planej
        TRANSPORTING ernam erdat erzet
        WHERE ernam IS INITIAL.

      SELECT *
        FROM zabst_planej
        INTO TABLE @DATA(lt_planej_db)
       WHERE werks   EQ @p_werks
         AND equnr   IN @so_proj[]
         AND shift   IN @so_shift[]
         AND irrdate IN @so_date[].

      IF sy-subrc EQ 0.
        DELETE zabst_planej FROM TABLE lt_planej_db[].
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

      MODIFY zabst_planej FROM TABLE gt_output[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

*-- Planejamento de irrigação atualizado com sucesso!
      MESSAGE i402(zfmfp).
*-- Programação de Irrigação
    ELSEIF p_prog EQ abap_true.
      ls_planej-aenam = sy-uname.
      ls_planej-aedat = sy-datum.
      ls_planej-aezet = sy-uzeit.
      ls_planej-programado = abap_true.
      MODIFY gt_output FROM ls_planej
        TRANSPORTING ernam erdat erzet programado
        WHERE equnr IS NOT INITIAL.

      MODIFY zabst_planej FROM TABLE gt_output[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

*-- Programação de irrigação atualizada com sucesso!
      MESSAGE i406(zfmfp).
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_OUTPUT>
*&---------------------------------------------------------------------*
FORM calculate CHANGING ls_output TYPE zabst_planej.

  DATA: lv_aux1 TYPE f,
        lv_aux2 TYPE f.

  IF ls_output-total_d_mes IS NOT INITIAL.
    lv_aux1 = ls_output-evapo_mes / ls_output-total_d_mes.
    lv_aux1 = lv_aux1 * ls_output-taxa_reposicao.
    IF ls_output-lamina IS NOT INITIAL.
      lv_aux2 = lv_aux1 / ls_output-lamina.
    ENDIF.
  ENDIF.

  IF ls_output-irrtyp EQ 'PLENO'.
    IF ls_output-weekend EQ 'SIM'.
      ls_output-total_h_dia = lv_aux2.
    ELSEIF ls_output-weekend EQ 'NÃO'.
      ls_output-total_h_dia = ( lv_aux2 * 7 ) / 5.
    ENDIF.
  ELSEIF ls_output-irrtyp EQ 'PULSO'.
    IF ls_output-qtd_dias IS NOT INITIAL.
      ls_output-total_h_dia = ( lv_aux2 * 7 ) / ls_output-qtd_dias.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_DAYS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_OUTPUT>
*&---------------------------------------------------------------------*
FORM check_days CHANGING ls_output TYPE zabst_planej.

  DATA: lv_aux1 TYPE f,
        lv_aux2 TYPE f,
        lv_aux3 TYPE f.

  CLEAR: lv_aux1, lv_aux2, lv_aux3.

  lv_aux1 = ls_output-evapo_mes * ls_output-taxa_reposicao.

  IF ls_output-lamina IS NOT INITIAL.
    lv_aux2 = lv_aux1 / ls_output-lamina.
  ENDIF.

  IF ls_output-qtd_dias IS NOT INITIAL.
    lv_aux3 = ( lv_aux2 * 7 ) / ls_output-qtd_dias.
  ENDIF.

  ls_output-total_h_dia = lv_aux3.

  IF ls_output-weekend EQ 'NÃO'.
    IF ls_output-day_in_week EQ 6
    OR ls_output-day_in_week EQ 7.
      CLEAR ls_output-total_h_dia.
    ENDIF.
  ENDIF.

  IF ls_output-irrtyp EQ 'PULSO'.
    CASE ls_output-qtd_dias.
      WHEN 1.
        IF ls_output-day_in_week NE 2.
          CLEAR ls_output-total_h_dia.
        ENDIF.
      WHEN 2.
        IF ls_output-day_in_week NE 2
        AND ls_output-day_in_week NE 5.
          CLEAR ls_output-total_h_dia.
        ENDIF.
      WHEN 3.
        IF ls_output-day_in_week NE 1
        AND ls_output-day_in_week NE 3
        AND ls_output-day_in_week NE 5.
          CLEAR ls_output-total_h_dia.
        ENDIF.
      WHEN 4.
        IF ls_output-day_in_week NE 1
        AND ls_output-day_in_week NE 2
        AND ls_output-day_in_week NE 4
        AND ls_output-day_in_week NE 5.
          CLEAR ls_output-total_h_dia.
        ENDIF.
    ENDCASE.
  ENDIF.

  ls_output-total_h_irr = ls_output-total_h_dia.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_IRRIGATION_PLANNING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_irrigation_planning.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_IRRIGATION_DATA_FOR_PROG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_irrigation_data_for_prog .

  DATA: lt_klah           TYPE tt_klah,
        ls_glmdhdr_sum    LIKE LINE OF gt_glmdhdr_sum,
        lref_date         TYPE REF TO data,
        lrt_gyear         TYPE RANGE OF /agri/gyear,
        lv_irrdate        TYPE sydatum,
        lv_begda          TYPE sydatum,
        lv_endda          TYPE sydatum,
        lv_number_of_days TYPE p.

  FIELD-SYMBOLS: <lv_value> TYPE any.

*-- Fetching Equipment header and Equipment master plants data
  SELECT *
    FROM /agri/fmirhdr
    INTO TABLE @gt_fmirhdr
   WHERE equnr IN @so_proj[]
     AND irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp_001
     AND kfrst EQ @abap_false.
  IF sy-subrc NE 0.
*-- Não existem projetos para os parâmetros informados!
    MESSAGE i397(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_fmirhdr BY equnr.

    LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
      INSERT INITIAL LINE INTO TABLE gt_fazendas
        ASSIGNING FIELD-SYMBOL(<ls_fazenda>).
      IF sy-subrc EQ 0.
        <ls_fazenda>-equnr = ls_fmirhdr-equnr.
        <ls_fazenda>-tplma_in = ls_fmirhdr-equnr(6).
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <ls_fazenda>-tplma_in
          IMPORTING
            output     = <ls_fazenda>-tplma_in
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        <ls_fazenda>-tplma_out = ls_fmirhdr-equnr(6).
      ENDIF.
    ENDLOOP.

    SORT gt_fazendas BY equnr tplma_in.
    DELETE ADJACENT DUPLICATES FROM gt_fazendas COMPARING equnr tplma_in.

    SELECT *
      FROM /agri/fmirhdrt
      INTO TABLE @gt_fmirhdrt
      FOR ALL ENTRIES IN @gt_fmirhdr
     WHERE equnr EQ @gt_fmirhdr-equnr
       AND spras EQ @sy-langu.

    SORT gt_fmirhdrt BY equnr.

*-- Fetching Terrain Header data to get terrains
    SELECT *
      FROM /agri/fmirflo
      INTO TABLE @gt_fmirflo
      FOR ALL ENTRIES IN @gt_fmirhdr
     WHERE equnr EQ @gt_fmirhdr-equnr.

    IF sy-subrc NE 0.
*-- Não existem talhões atrelados aos projetos informados!
      MESSAGE i396(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT gt_fmirflo BY equnr tplnr_fl.

*-- Fetching Terrain Header data to get gross terrain arae
      READ TABLE gt_fazendas INTO DATA(ls_fazenda) INDEX 1.
      IF sy-subrc EQ 0.
        SELECT tplnr_fl, pltxt, tplkz, fltyp, tplvl,
               tplma, iwerk, swerk, kokrs, anlnr,
               kostl, garea, kfrst, loevm
          FROM /agri/glflot
          INTO TABLE @gt_glflot
         WHERE tplma    EQ @ls_fazenda-tplma_in
           AND iwerk    EQ @p_werks
           AND kfrst    EQ @abap_false
           AND loevm    EQ @abap_false.
      ENDIF.

      IF gt_glflot[] IS NOT INITIAL.
        SORT gt_glflot BY tplnr_fl.

*-- Fecth all crop season
        SELECT *
          FROM /agri/glflcma
          INTO TABLE @gt_glflcma
          FOR ALL ENTRIES IN @gt_glflot
         WHERE tplnr_fl EQ @gt_glflot-tplnr_fl
           AND astat    EQ 'A'
           AND loevm    EQ @abap_false.

        SORT gt_glflcma BY tplnr_fl contr.

*-- Fetch class header data
        SELECT *
          FROM klah
          INTO TABLE @lt_klah
          WHERE class IN ( 'EVAPORATION', 'PRECIPITACION' ).
        IF sy-subrc EQ 0.
*-- Calling meathod to get attribute header data
          CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
            EXPORTING
              it_klah  = lt_klah
              i_agtyp  = 'X90'
            IMPORTING
              et_athdr = DATA(lt_athdr).
        ENDIF.

        IF sy-uname EQ 'T_H.KABABE'.
          BREAK-POINT.
        ENDIF.

        DATA(lrt_date) = so_date[].
        READ TABLE lrt_date ASSIGNING FIELD-SYMBOL(<lrs_date>) INDEX 1.
        IF sy-subrc EQ 0.
          <lrs_date>-low = <lrs_date>-low - 7.
        ENDIF.

        SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
               h~contr, h~cmnum, h~equnr, h~mpgrp,
               h~mdate, h~mtime, v~atzhl, v~atwrt,
               v~atflv, c~atinn, c~atnam
          INTO TABLE @gt_glmdhdr
          FROM /agri/glmdhdr AS h
          INNER JOIN /agri/glmdatv AS v
          ON v~mdocm EQ h~mdocm
          INNER JOIN cabn AS c
          ON c~atinn EQ v~atinn
          FOR ALL ENTRIES IN @gt_glflot
         WHERE h~mdtyp    EQ 'ZIRG'
           AND h~aslvl    EQ 'I'
           AND h~tplnr_fl EQ @gt_glflot-tplnr_fl
           AND h~mpgrp    IN ( 'EVAPORATION', 'PRECIPITACION' )
           AND h~mdate    IN @lrt_date[]
           AND h~kfrst    EQ @abap_false
           AND h~canceled EQ @abap_false.

        IF gt_glmdhdr[] IS NOT INITIAL
        AND lt_athdr[] IS NOT INITIAL.
          lv_number_of_days = ( so_date[ 1 ]-high - so_date[ 1 ]-low ) + 1.

          SORT lt_athdr BY atinn.
          SORT gt_glmdhdr BY mdate.

          DO lv_number_of_days TIMES.
            DATA(lv_index) = sy-index.
            CLEAR ls_glmdhdr_sum.

            IF lv_index EQ 1.
              lv_irrdate = so_date[ 1 ]-low.
            ELSE.
              ADD 1 TO lv_irrdate.
            ENDIF.

            ls_glmdhdr_sum-irrdate = lv_irrdate.
            lv_begda = lv_irrdate - 7.
            lv_endda = lv_irrdate - 1.
            DATA(lt_glmdhdr_aux) = gt_glmdhdr[].
            DELETE lt_glmdhdr_aux WHERE mdate NOT BETWEEN lv_begda
                                                      AND lv_endda.

            DATA(lt_precip) = lt_glmdhdr_aux[].
            DELETE lt_precip WHERE mpgrp NE 'PRECIPITACION'.
            SORT lt_precip BY atflv DESCENDING.
            DELETE lt_precip WHERE atflv LT 10.
            SORT lt_precip BY mdate.
            DELETE ADJACENT DUPLICATES FROM lt_precip COMPARING mdate.
            LOOP AT lt_precip INTO DATA(ls_precip).
              DELETE lt_glmdhdr_aux WHERE mpgrp EQ 'EVAPORATION'
                                      AND mdate EQ ls_precip-mdate.
            ENDLOOP.

            LOOP AT lt_glmdhdr_aux ASSIGNING FIELD-SYMBOL(<ls_glmdhdr>).
              IF <ls_glmdhdr>-atflv IS NOT INITIAL.
                READ TABLE lt_athdr INTO DATA(ls_athdr)
                  WITH KEY atinn = <ls_glmdhdr>-atinn BINARY SEARCH.
                IF sy-subrc EQ 0.
                  CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
                  ASSIGN lref_date->* TO <lv_value>.
                  <lv_value> = <ls_glmdhdr>-atflv.
                  <ls_glmdhdr>-atwrt = <lv_value>.
                  CONDENSE <ls_glmdhdr>-atwrt NO-GAPS.
                ENDIF.
              ELSE.
                CONDENSE <ls_glmdhdr>-atwrt NO-GAPS.
              ENDIF.

              READ TABLE gt_glflot INTO DATA(ls_glflot)
                WITH KEY tplnr_fl = <ls_glmdhdr>-tplnr_fl BINARY SEARCH.
              IF sy-subrc EQ 0.
                ls_glmdhdr_sum-tplma_in = ls_glflot-tplma.

                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                  EXPORTING
                    input      = ls_glmdhdr_sum-tplma_in
                  IMPORTING
                    output     = ls_glmdhdr_sum-tplma_out
                  EXCEPTIONS
                    not_found  = 1
                    not_active = 2
                    OTHERS     = 3.
              ENDIF.

*              IF lv_check EQ abap_true.
*                DATA(lt_precip) = lt_glmdhdr_aux[].
*                DELETE lt_precip WHERE mpgrp NE 'PRECIPITACION'.
*                lv_check = abap_false.
*                SORT lt_precip BY atflv DESCENDING.
*                DELETE lt_precip WHERE atflv LT 10.
*                SORT lt_precip BY mdate.
*                DELETE ADJACENT DUPLICATES FROM lt_precip COMPARING mdate.
*                LOOP AT lt_precip INTO DATA(ls_precip).
*                  DELETE lt_glmdhdr_aux WHERE mdate EQ ls_precip-mdate.
*                ENDLOOP.
*              ENDIF.

              ls_glmdhdr_sum-qtd_doctos = 1.
              ls_glmdhdr_sum-mpgrp = <ls_glmdhdr>-mpgrp.
              ls_glmdhdr_sum-atflv = <ls_glmdhdr>-atflv.
              ls_glmdhdr_sum-atinn = <ls_glmdhdr>-atinn.
              ls_glmdhdr_sum-atnam = <ls_glmdhdr>-atnam.
              COLLECT ls_glmdhdr_sum INTO gt_glmdhdr_sum.
            ENDLOOP.
          ENDDO.

          SORT gt_glmdhdr_sum BY irrdate tplma_in mpgrp atnam.
        ENDIF.

        IF sy-uname EQ 'T_H.KABABE'.
          BREAK-POINT.
        ENDIF.

*--Fetching Terrain Attribute Values to get Crop Coefficient value
        SELECT g~tplnr_fl, g~atwrt,
               g~atflv, c~atinn
          FROM /agri/glflatv AS g
          JOIN cabn AS c
          ON c~atinn EQ g~atinn
          INTO TABLE @gt_glflatv
          FOR ALL ENTRIES IN @gt_glflot
         WHERE g~tplnr_fl  EQ @gt_glflot-tplnr_fl
           AND g~class     EQ @zcl_abs_abap_maintain=>c_class_info "'INFO_PROJETO'
           AND g~deleted   EQ @space
           AND c~atnam     EQ @zcl_abs_abap_maintain=>c_charact_coef. "'COEF_CULTURA'
        IF sy-subrc EQ 0.
          SORT gt_glflatv BY tplnr_fl.

          REFRESH: lt_klah, lt_athdr.
*--Fetch class header data
          SELECT *
            FROM klah
            INTO TABLE @lt_klah
           WHERE class EQ 'INFO_PROJETO'.
          IF sy-subrc EQ 0.
*--Calling meathod to get attribute header data
            CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
              EXPORTING
                it_klah  = lt_klah
                i_agtyp  = 'X91'
              IMPORTING
                et_athdr = lt_athdr.
          ENDIF.

          IF lt_athdr[] IS NOT INITIAL.
            LOOP AT gt_glflatv ASSIGNING FIELD-SYMBOL(<ls_glflatv>).
              IF <ls_glflatv>-atflv IS NOT INITIAL.
                READ TABLE lt_athdr INTO ls_athdr
                  WITH KEY atinn = <ls_glflatv>-atinn BINARY SEARCH.
                IF sy-subrc EQ 0.
                  CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
                  ASSIGN lref_date->* TO <lv_value>.
                  <lv_value> = <ls_glflatv>-atflv.
                  <ls_glflatv>-atwrt = <lv_value>.
                  CONDENSE <ls_glflatv>-atwrt NO-GAPS.
                ENDIF.
              ELSE.
                CONDENSE <ls_glflatv>-atwrt NO-GAPS.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gt_fazendas[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lrt_gyear
        ASSIGNING FIELD-SYMBOL(<lrs_gyear>).
      IF sy-subrc EQ 0.
        <lrs_gyear> = 'IBT'.
        <lrs_gyear>-low = so_date[ 1 ]-low(4).
        <lrs_gyear>-low = <lrs_gyear>-low - 4.
        <lrs_gyear>-high = so_date[ 1 ]-low(4).
        <lrs_gyear>-high = <lrs_gyear>-high - 1.

        SELECT *
          FROM zabst_clima_hist
          INTO TABLE @gt_clima_hist
          FOR ALL ENTRIES IN @gt_fazendas
         WHERE tplnr_fl EQ @gt_fazendas-tplma_in
           AND ano      IN @lrt_gyear[].

        SORT gt_clima_hist BY tplnr_fl tipo.
      ENDIF.
    ENDIF.
  ENDIF.

*-- Fetching Order Numbers
  SELECT *
    FROM zabst_ordhdr
    INTO TABLE @gt_ordhdr
   WHERE equnr IN @so_proj[]
     AND werks EQ @p_werks.

  IF sy-subrc EQ 0.
    SORT gt_ordhdr BY equnr ordno.

*-- Fetching Work Order Confirmation data
    SELECT *
      FROM zabst_ordcnf
      INTO TABLE @gt_ordcnf
       FOR ALL ENTRIES IN @gt_ordhdr
     WHERE ordno EQ @gt_ordhdr-ordno
       AND vornr IN @so_shift[]
       AND loevm EQ @abap_false.

    SORT gt_ordcnf BY ordno.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form OUTPUT_PREPARE_FOR_PROG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM output_prepare_for_prog .

  TYPES: BEGIN OF ly_fmirflo_sum,
           equnr  TYPE /agri/glequnr,
           garea1 TYPE /agri/glgarea,
           garea2 TYPE /agri/glgarea,
           garea3 TYPE /agri/glgarea,
           garea4 TYPE /agri/glgarea,
           garea5 TYPE /agri/glgarea,
           garea6 TYPE /agri/glgarea,
         END OF ly_fmirflo_sum,

         BEGIN OF ly_ppsortkey,
           irrdate        TYPE sydatum,
           equnr          TYPE zabs_del_projeto,
           variedade1     TYPE zabs_del_key_variedade,
           variedade2     TYPE zabs_del_key_variedade,
           variedade3     TYPE zabs_del_key_variedade,
           variedade4     TYPE zabs_del_key_variedade,
           variedade5     TYPE zabs_del_key_variedade,
           variedade6     TYPE zabs_del_key_variedade,
           porta_enxerto1 TYPE zabs_del_key_enxerto,
           porta_enxerto2 TYPE zabs_del_key_enxerto,
           porta_enxerto3 TYPE zabs_del_key_enxerto,
           porta_enxerto4 TYPE zabs_del_key_enxerto,
           porta_enxerto5 TYPE zabs_del_key_enxerto,
           porta_enxerto6 TYPE zabs_del_key_enxerto,
           evaporation1   TYPE atflv,
           evaporation2   TYPE atflv,
           evaporation3   TYPE atflv,
           evaporation4   TYPE atflv,
           evaporation5   TYPE atflv,
           evaporation6   TYPE atflv,
           qtd_doc_evapo1 TYPE i,
           qtd_doc_evapo2 TYPE i,
           qtd_doc_evapo3 TYPE i,
           qtd_doc_evapo4 TYPE i,
           qtd_doc_evapo5 TYPE i,
           qtd_doc_evapo6 TYPE i,
           precipitacion1 TYPE atflv,
           precipitacion2 TYPE atflv,
           precipitacion3 TYPE atflv,
           precipitacion4 TYPE atflv,
           precipitacion5 TYPE atflv,
           precipitacion6 TYPE atflv,
           qtd_doc_preci1 TYPE i,
           qtd_doc_preci2 TYPE i,
           qtd_doc_preci3 TYPE i,
           qtd_doc_preci4 TYPE i,
           qtd_doc_preci5 TYPE i,
           qtd_doc_preci6 TYPE i,
         END OF ly_ppsortkey,

         BEGIN OF ly_media_hist,
           count     TYPE i,
           tplma_in  TYPE /agri/gltplma,
           tplma_out TYPE /agri/gltplma,
           equnr     TYPE /agri/glequnr,
           tipo      TYPE zabs_del_tipo,
           mes01     TYPE atflv,
           mes02     TYPE atflv,
           mes03     TYPE atflv,
           mes04     TYPE atflv,
           mes05     TYPE atflv,
           mes06     TYPE atflv,
           mes07     TYPE atflv,
           mes08     TYPE atflv,
           mes09     TYPE atflv,
           mes10     TYPE atflv,
           mes11     TYPE atflv,
           mes12     TYPE atflv,
         END OF ly_media_hist.

  DATA: lt_fmirflo_sum    TYPE STANDARD TABLE OF ly_fmirflo_sum INITIAL SIZE 0,
        lt_ppsortkey      TYPE STANDARD TABLE OF ly_ppsortkey INITIAL SIZE 0,
        lt_media_hist     TYPE STANDARD TABLE OF ly_media_hist,
        ls_media_hist     LIKE LINE OF lt_media_hist,
        lt_new_output     LIKE gt_output,
        ls_new_output     LIKE LINE OF lt_new_output,
        ls_ppsortkey      LIKE LINE OF lt_ppsortkey,
        lt_dates          TYPE /scwm/tt_lm_dates,
        ls_fmirflo_sum    LIKE LINE OF lt_fmirflo_sum,
        lo_exception      TYPE REF TO cx_sy_arithmetic_overflow,
        lv_precipita_mes  TYPE zabs_del_evapo_mes,
        lv_text           TYPE string,
        lv_fieldname      TYPE fieldname,
        lv_shift_area     TYPE fieldname,
        lv_shift_value    TYPE fieldname,
        lv_media_chuva    TYPE atflv,
        lv_media_evapo    TYPE atflv,
        lv_null_date      TYPE sydatum,
        lv_day_in_week    TYPE p,
        lv_number_of_days TYPE p,
        lv_aux1           TYPE f,
        lv_aux2           TYPE f.

  FIELD-SYMBOLS: <lv_media_chuva> TYPE atflv,
                 <lv_media_evapo> TYPE atflv.

  SELECT *
    FROM zabst_planej
    INTO TABLE @gt_output_old
   WHERE werks   EQ @p_werks
     AND equnr   IN @so_proj[]
     AND shift   IN @so_shift[]
     AND irrdate IN @so_date[].

*  gt_output = CORRESPONDING #( lt_planning ).

  READ TABLE so_date INTO DATA(ls_date_range) INDEX 1.
  IF sy-subrc EQ 0.
    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = ls_date_range-low
        iv_endda = ls_date_range-high
      IMPORTING
        et_dates = lt_dates.
  ENDIF.

  LOOP AT lt_dates INTO DATA(ls_date).
    DATA(lv_tabix) = sy-tabix.
    CLEAR ls_fmirflo_sum.
    LOOP AT gt_fmirflo INTO DATA(ls_fmirflo).
      READ TABLE gt_glflot INTO DATA(ls_glflot)
        WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_glflot.
      ENDIF.

      DATA(ls_fmirflo_aux) = ls_fmirflo.
      AT NEW equnr.
        CLEAR: ls_ppsortkey, ls_fmirflo_sum.
      ENDAT.

      IF lv_tabix EQ 1.
        ls_fmirflo_sum-equnr = ls_fmirflo_aux-equnr.
        DO 6 TIMES.
          DATA(lv_field_index) = sy-index.
          lv_shift_value = 'ZZCSHIFT' && lv_field_index.
          ASSIGN COMPONENT lv_shift_value OF STRUCTURE ls_fmirflo_aux
            TO FIELD-SYMBOL(<lv_shift_value>).
          IF sy-subrc EQ 0.
            lv_shift_area = 'GAREA' && lv_field_index.
            ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
              TO FIELD-SYMBOL(<lv_shift_area>).
            IF sy-subrc EQ 0.
              IF <lv_shift_value> IS NOT INITIAL.
                <lv_shift_area> = <lv_shift_area> + ls_glflot-garea.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDIF.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift1
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade1
                                                 ls_ppsortkey-porta_enxerto1
                                                 ls_ppsortkey-evaporation1
                                                 ls_ppsortkey-precipitacion1
                                                 ls_ppsortkey-qtd_doc_evapo1
                                                 ls_ppsortkey-qtd_doc_preci1.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift2
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade2
                                                 ls_ppsortkey-porta_enxerto2
                                                 ls_ppsortkey-evaporation2
                                                 ls_ppsortkey-precipitacion2
                                                 ls_ppsortkey-qtd_doc_evapo2
                                                 ls_ppsortkey-qtd_doc_preci2.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift3
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade3
                                                 ls_ppsortkey-porta_enxerto3
                                                 ls_ppsortkey-evaporation3
                                                 ls_ppsortkey-precipitacion3
                                                 ls_ppsortkey-qtd_doc_evapo3
                                                 ls_ppsortkey-qtd_doc_preci3.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift4
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade4
                                                 ls_ppsortkey-porta_enxerto4
                                                 ls_ppsortkey-evaporation4
                                                 ls_ppsortkey-precipitacion4
                                                 ls_ppsortkey-qtd_doc_evapo4
                                                 ls_ppsortkey-qtd_doc_preci4.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift5
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade5
                                                 ls_ppsortkey-porta_enxerto5
                                                 ls_ppsortkey-evaporation5
                                                 ls_ppsortkey-precipitacion5
                                                 ls_ppsortkey-qtd_doc_evapo5
                                                 ls_ppsortkey-qtd_doc_preci5.

      PERFORM check_shift_variety_for_prog USING ls_fmirflo_aux
                                                 ls_fmirflo_aux-zzcshift6
                                                 ls_date
                                        CHANGING ls_ppsortkey-variedade6
                                                 ls_ppsortkey-porta_enxerto6
                                                 ls_ppsortkey-evaporation6
                                                 ls_ppsortkey-precipitacion6
                                                 ls_ppsortkey-qtd_doc_evapo6
                                                 ls_ppsortkey-qtd_doc_preci6.

      AT END OF equnr.
        ls_ppsortkey-equnr = ls_fmirflo_aux-equnr.
        ls_ppsortkey-irrdate = ls_date.
        APPEND ls_ppsortkey TO lt_ppsortkey.
        IF lv_tabix EQ 1.
          APPEND ls_fmirflo_sum TO lt_fmirflo_sum.
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDLOOP.

  SORT: lt_fmirflo_sum BY equnr,
        lt_ppsortkey BY equnr,
        gt_fazendas BY tplma_in.

  LOOP AT gt_clima_hist INTO DATA(ls_clima_hist).
    ls_media_hist-count = 1.
    ls_media_hist-tplma_in = ls_clima_hist-tplnr_fl.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input      = ls_media_hist-tplma_in
      IMPORTING
        output     = ls_media_hist-tplma_out
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.
    ls_media_hist-tipo  = ls_clima_hist-tipo.
    ls_media_hist-mes01 = ls_clima_hist-mes01.
    ls_media_hist-mes02 = ls_clima_hist-mes02.
    ls_media_hist-mes03 = ls_clima_hist-mes03.
    ls_media_hist-mes04 = ls_clima_hist-mes04.
    ls_media_hist-mes05 = ls_clima_hist-mes05.
    ls_media_hist-mes06 = ls_clima_hist-mes06.
    ls_media_hist-mes07 = ls_clima_hist-mes07.
    ls_media_hist-mes08 = ls_clima_hist-mes08.
    ls_media_hist-mes09 = ls_clima_hist-mes09.
    ls_media_hist-mes10 = ls_clima_hist-mes10.
    ls_media_hist-mes11 = ls_clima_hist-mes11.
    ls_media_hist-mes12 = ls_clima_hist-mes12.
    COLLECT ls_media_hist INTO lt_media_hist.
  ENDLOOP.

*  LOOP AT lt_media_hist ASSIGNING FIELD-SYMBOL(<ls_media_hist>).
*    IF <ls_media_hist>-count IS INITIAL.
*      <ls_media_hist>-count = 1.
*    ENDIF.
*    <ls_media_hist>-mes01 = <ls_media_hist>-mes01 / <ls_media_hist>-count.
*    <ls_media_hist>-mes02 = <ls_media_hist>-mes02 / <ls_media_hist>-count.
*    <ls_media_hist>-mes03 = <ls_media_hist>-mes03 / <ls_media_hist>-count.
*    <ls_media_hist>-mes04 = <ls_media_hist>-mes04 / <ls_media_hist>-count.
*    <ls_media_hist>-mes05 = <ls_media_hist>-mes05 / <ls_media_hist>-count.
*    <ls_media_hist>-mes06 = <ls_media_hist>-mes06 / <ls_media_hist>-count.
*    <ls_media_hist>-mes07 = <ls_media_hist>-mes07 / <ls_media_hist>-count.
*    <ls_media_hist>-mes08 = <ls_media_hist>-mes08 / <ls_media_hist>-count.
*    <ls_media_hist>-mes09 = <ls_media_hist>-mes09 / <ls_media_hist>-count.
*    <ls_media_hist>-mes10 = <ls_media_hist>-mes10 / <ls_media_hist>-count.
*    <ls_media_hist>-mes11 = <ls_media_hist>-mes11 / <ls_media_hist>-count.
*    <ls_media_hist>-mes12 = <ls_media_hist>-mes12 / <ls_media_hist>-count.
*  ENDLOOP.

  SORT lt_media_hist BY equnr tipo.

  LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
    CLEAR: lv_media_chuva, lv_media_evapo, lv_aux1, lv_aux2.
    READ TABLE lt_fmirflo_sum INTO ls_fmirflo_sum
      WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_fmirhdrt INTO DATA(ls_fmirhdrt)
             WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_fmirhdrt.
      ENDIF.

      READ TABLE lt_ppsortkey INTO ls_ppsortkey
        WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        lv_tabix = sy-tabix + 1.
*-- Ler talhão aleatório pertencente ao projeto/equipamento
        READ TABLE gt_fmirflo INTO ls_fmirflo
          WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE gt_glflatv INTO DATA(ls_glflatv)
            WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl BINARY SEARCH.
          IF sy-subrc NE 0.
            CLEAR ls_glflatv.
          ENDIF.
        ELSE.
          CLEAR ls_glflatv.
        ENDIF.

        lv_fieldname = 'MES' && so_date[ 1 ]-low+4(2).

        DO 6 TIMES.
          CLEAR lv_precipita_mes.
          lv_field_index = sy-index.
          lv_shift_value = 'ZZCSHIFT' && lv_field_index.
          lv_shift_area = 'GAREA' && lv_field_index.
          ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
            TO FIELD-SYMBOL(<lv_fieldval>).
          IF sy-subrc EQ 0.
            IF <lv_fieldval> IS NOT INITIAL.
              INSERT INITIAL LINE INTO TABLE gt_output
                ASSIGNING FIELD-SYMBOL(<ls_output>).
              IF sy-subrc EQ 0.
*-- Data de Irrigação
                <ls_output>-irrdate = ls_ppsortkey-irrdate.
*-- Semana.Ano
                CALL FUNCTION '/AGRI/G_GET_WEEK_BASED_ON_DATE'
                  EXPORTING
                    i_date = <ls_output>-irrdate
                  IMPORTING
                    e_week = <ls_output>-calweek.
*-- Dia da Semana
                CLEAR lv_day_in_week.
                CALL FUNCTION 'DAY_IN_WEEK'
                  EXPORTING
                    datum = <ls_output>-irrdate
                  IMPORTING
                    wotnr = lv_day_in_week.

                <ls_output>-day_in_week = lv_day_in_week.
*-- Total Dias Mês
                CLEAR lv_number_of_days.
                CALL FUNCTION 'HR_E_NUM_OF_DAYS_OF_MONTH'
                  EXPORTING
                    p_fecha        = <ls_output>-irrdate
                  IMPORTING
                    number_of_days = lv_number_of_days.
                <ls_output>-total_d_mes = lv_number_of_days.
*----------------------*
* Qtde. dias           |
*----------------------*
*|1|2ter|    |    |    |
*|2|2ter|5sex|    |    |
*|3|1seg|3qua|5sex|    |
*|4|1seg|2ter|4qui|5sex|
*----------------------*
*                PERFORM check_days CHANGING <ls_output>.
*-- Centro/Fazenda
                <ls_output>-werks = p_werks.
*-- Projeto/Código Equipamento
                <ls_output>-equnr = ls_fmirhdr-equnr.
*-- Descrição Projeto/Equipamento
                <ls_output>-descr = ls_fmirhdrt-descr.
                CASE lv_field_index.
                  WHEN 1. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-1.
                    <ls_output>-variedade = ls_ppsortkey-variedade1.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto1.
*-- Vazão (m3/h)
                    <ls_output>-vazao = ls_fmirhdr-zzvns01.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo1 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation1 /
                                              ls_ppsortkey-qtd_doc_evapo1.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci1 IS NOT INITIAL.
                      lv_precipita_mes = ls_ppsortkey-precipitacion1 /
                                         ls_ppsortkey-qtd_doc_preci1.
                    ENDIF.
                  WHEN 2. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-2.
                    <ls_output>-variedade = ls_ppsortkey-variedade2.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto2.
                    <ls_output>-vazao = ls_fmirhdr-zzvns02.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo2 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation2 /
                                              ls_ppsortkey-qtd_doc_evapo2.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci2 IS NOT INITIAL.
                      <ls_output>-chuva_mes = ls_ppsortkey-precipitacion2 /
                                              ls_ppsortkey-qtd_doc_preci2.
                    ENDIF.
                  WHEN 3. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-3.
                    <ls_output>-variedade = ls_ppsortkey-variedade3.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto3.
                    <ls_output>-vazao = ls_fmirhdr-zzvns03.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo3 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation3 /
                                              ls_ppsortkey-qtd_doc_evapo3.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci3 IS NOT INITIAL.
                      <ls_output>-chuva_mes = ls_ppsortkey-precipitacion3 /
                                              ls_ppsortkey-qtd_doc_preci3.
                    ENDIF.
                  WHEN 4. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-4.
                    <ls_output>-variedade = ls_ppsortkey-variedade4.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto4.
                    <ls_output>-vazao = ls_fmirhdr-zzvns04.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo4 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation4 /
                                              ls_ppsortkey-qtd_doc_evapo4.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci4 IS NOT INITIAL.
                      <ls_output>-chuva_mes = ls_ppsortkey-precipitacion4 /
                                              ls_ppsortkey-qtd_doc_preci4.
                    ENDIF.
                  WHEN 5. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-5.
                    <ls_output>-variedade = ls_ppsortkey-variedade5.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto5.
                    <ls_output>-vazao = ls_fmirhdr-zzvns05.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo5 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation5 /
                                              ls_ppsortkey-qtd_doc_evapo5.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci5 IS NOT INITIAL.
                      <ls_output>-chuva_mes = ls_ppsortkey-precipitacion5 /
                                              ls_ppsortkey-qtd_doc_preci5.
                    ENDIF.
                  WHEN 6. "Operação/Shift + Variedade + Porta-Enxerto
                    <ls_output>-shift = c_shift-6.
                    <ls_output>-variedade = ls_ppsortkey-variedade6.
                    <ls_output>-porta_enxerto = ls_ppsortkey-porta_enxerto6.
                    <ls_output>-vazao = ls_fmirhdr-zzvns06.
*-- Evaporação Mês
                    IF ls_ppsortkey-qtd_doc_evapo6 IS NOT INITIAL.
                      <ls_output>-evapo_mes = ls_ppsortkey-evaporation6 /
                                              ls_ppsortkey-qtd_doc_evapo6.
                    ENDIF.
*-- Chuva Mês
                    IF ls_ppsortkey-qtd_doc_preci6 IS NOT INITIAL.
                      <ls_output>-chuva_mes = ls_ppsortkey-precipitacion6 /
                                              ls_ppsortkey-qtd_doc_preci6.
                    ENDIF.
                ENDCASE.
**-- Evaporação Mês
*              IF <lv_media_evapo> IS ASSIGNED.
*                ADD <lv_media_evapo> TO <ls_output>-evapo_mes.
*                ADD 1 TO ls_media_evapo-count.
*                <ls_output>-evapo_mes = <ls_output>-evapo_mes /
*                                        ls_media_evapo-count.
*              ENDIF.
**-- Chuva Mês
*              IF <lv_media_chuva> IS ASSIGNED.
*                ADD <lv_media_chuva> TO <ls_output>-chuva_mes.
*                ADD 1 TO ls_media_chuva-count.
*                <ls_output>-chuva_mes = <ls_output>-chuva_mes /
*                                        ls_media_chuva-count.
*              ENDIF.
*-- Total Horas Dia
*                PERFORM calculate CHANGING <ls_output>.
*-- Área Shift
                ASSIGN COMPONENT lv_shift_area OF STRUCTURE ls_fmirflo_sum
                  TO <lv_shift_area>.
                IF sy-subrc EQ 0.
                  <ls_output>-garea = <lv_shift_area>.
                ENDIF.
*-- Final de Semana
                <ls_output>-weekend = ls_fmirhdr-weekend.
*-- Chuva+irrig (mm) <- Factor_Hidrometro
                <ls_output>-chuva_irrig = ls_fmirhdr-zzhydfac.
*-- Lâmina (mm/h)
                <ls_output>-lamina = ls_fmirhdr-lamina.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.
        READ TABLE lt_ppsortkey INTO ls_ppsortkey
          INDEX lv_tabix COMPARING equnr.
      ENDWHILE.
    ENDIF.
  ENDLOOP.

  SORT gt_output_old BY werks equnr shift irrdate.
  LOOP AT gt_output ASSIGNING <ls_output>.
    lv_tabix = sy-tabix.
    READ TABLE gt_output_old INTO DATA(ls_output_old)
      WITH KEY werks   = <ls_output>-werks
               equnr   = <ls_output>-equnr
               shift   = <ls_output>-shift
               irrdate = <ls_output>-irrdate BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE gt_output INDEX lv_tabix.
    ELSE.
      <ls_output>-irrtyp = ls_output_old-irrtyp.
      <ls_output>-qtd_dias = ls_output_old-qtd_dias.
      <ls_output>-taxa_reposicao = ls_output_old-taxa_reposicao.
      <ls_output>-ernam = ls_output_old-ernam.
      <ls_output>-erdat = ls_output_old-erdat.
      <ls_output>-erzet = ls_output_old-erzet.
      <ls_output>-aenam = sy-uname.
      <ls_output>-aedat = sy-datum.
      <ls_output>-aezet = sy-uzeit.
      PERFORM calculate CHANGING <ls_output>.
      PERFORM check_days CHANGING <ls_output>.
      <ls_output>-total_h_mes = <ls_output>-total_h_dia * <ls_output>-total_d_mes.
    ENDIF.
  ENDLOOP.

ENDFORM.
