*&---------------------------------------------------------------------*
*& Report ZFMFP_SEQ_TALHAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfp_seq_talhao.

TABLES: /agri/fmapcsh,
        ys4selpomares,
        /agri/glflot,
        t001w.

TYPE-POOLS: abap.

INCLUDE <icon>.

TYPES: type_week TYPE numc2,
       type_box  TYPE numc4,
       type_cl5  TYPE char4,
       type_cl6  TYPE char4,

       BEGIN OF type_matdoc,
         werks TYPE werks_d,
         menge TYPE menge_d,
         budat TYPE budat,
         bwart TYPE bwart,
         matnr TYPE matnr,
         charg TYPE charg_d,
         lifnr TYPE elifn,
       END OF type_matdoc,

       BEGIN OF type_mch1,
         matnr    TYPE matnr,
         charg    TYPE charg_d,
         cuobj_bm TYPE cuobj_bm,
         objek    TYPE cuobn,
       END OF type_mch1,

       BEGIN OF type_cabn,
         atinn TYPE atinn,
         adzhl TYPE adzhl,
         atnam TYPE atnam,
         atfor TYPE atfor,
         anzst TYPE anzst,
         anzdz TYPE anzdz,
       END OF type_cabn,

       BEGIN OF type_ausp,
         objek TYPE cuobn,
         atinn TYPE atinn,
         atzhl TYPE wzaehl,
         mafid TYPE klmaf,
         klart TYPE klassenart,
         adzhl TYPE adzhl,
         atwrt TYPE atwrt,
         atflv TYPE atflv,
       END OF type_ausp,

       BEGIN OF type_colhendo,
         werks        TYPE werks_d,
         menge        TYPE menge_d,
         budat        TYPE budat,
         bwart        TYPE bwart,
         matnr        TYPE matnr,
         charg        TYPE charg_d,
         cuobj_bm     TYPE cuobj_bm,
         objek        TYPE cuobn,
         tplnr_fl     TYPE /agri/gltplnr_fl,
         cod_imov_c   TYPE /agri/glstrno,
         talhao_c     TYPE /agri/glstrno,
         cod_imov_f   TYPE atflv,
         talhao_f     TYPE atflv,
         ratio        TYPE atflv,
         brix         TYPE atflv,
         solidos      TYPE atflv,
         pond_ratio   TYPE atflv,
         pond_brix    TYPE atflv,
         pond_solidos TYPE atflv,
         ano_safra    TYPE zfmanosafra,
       END OF type_colhendo,

       BEGIN OF type_soma,
         cod_faz_input   TYPE /agri/gltplnr_fl,
         cod_faz_output  TYPE /agri/gltplnr_fl,
         referencia      TYPE char1,
         semana_prog     TYPE fti_week_year,
         ano_safra       TYPE zfmanosafra,
         tplnr_fl        TYPE /agri/gltplnr_fl,
         cod_invalido    TYPE abap_bool,
         cod_imovel_c    TYPE /agri/glstrno,
         talhao_c        TYPE /agri/glstrno,
         cod_imov        TYPE zfmcod_imov,
         talhao          TYPE zfmtalhao,
         matnr           TYPE matnr,
         menge           TYPE menge_d,
         ratio_f         TYPE atflv,
         brix_f          TYPE atflv,
         solidos_f       TYPE atflv,
         media_ratio     TYPE atflv,
         media_brix      TYPE atflv,
         media_solidos   TYPE atflv,
         media_ratio_c   TYPE atwrt,
         media_brix_c    TYPE atwrt,
         media_solidos_c TYPE atwrt,
         setor_orig      TYPE yoe_setor_orig,
         lifnr           TYPE lifnr,
         comprador       TYPE /agri/glflot-stort1,
         zzplantyear     TYPE zabs_del_plantyr,
         zzcsage_month   TYPE zabs_del_month,
         zzcsage         TYPE /agri/glageid,
         cod_imovel_full TYPE /agri/gltplnr_fl,
         iwerk           TYPE iwerk,
         zzfazvartecnia  TYPE zabs_del_vtech,
         florada         TYPE zfmflorada,
         ano_plantio     TYPE zfmanoplantio,
         rainforest      TYPE zfmfp_colheita-rainforest,
         idade_talhao    TYPE zfmfp_colheita-idade_talhao,
         setor           TYPE zfmfp_colheita-setor,
         taxa_kgss       TYPE zfmtaxa_kgss,
         nova_linha      TYPE abap_bool,
       END OF type_soma,

       BEGIN OF type_arrendamento,
         cod_imov_from TYPE zfmcod_imov,
         cod_imov_to   TYPE zfmcod_imov,
         source_in     TYPE /agri/gltplnr_fl,
         target_in     TYPE /agri/gltplnr_fl,
       END OF type_arrendamento,

       BEGIN OF type_glflot,
         tplnr_fl   TYPE /agri/gltplnr_fl,
         pltxt      TYPE /agri/glpltxt,
         tplma      TYPE /agri/gltplma,
         iwerk      TYPE iwerk,
         ptrno      TYPE /agri/glptrno,
         strno      TYPE /agri/glstrno,
         beber1     TYPE beber,
         setor_orig TYPE yoe_setor_orig,
         stort1     TYPE pmloc,
       END OF type_glflot,

       type_glflot_tab TYPE STANDARD TABLE OF type_glflot INITIAL SIZE 0.

DATA: t_matdoc           TYPE STANDARD TABLE OF type_matdoc INITIAL SIZE 0,
      t_glflot           TYPE type_glflot_tab,
      t_mch1             TYPE STANDARD TABLE OF type_mch1 INITIAL SIZE 0,
      t_arrendamento     TYPE STANDARD TABLE OF type_arrendamento INITIAL SIZE 0,
      t_cabn             TYPE STANDARD TABLE OF type_cabn INITIAL SIZE 0,
      t_ausp             TYPE STANDARD TABLE OF type_ausp INITIAL SIZE 0,
      t_colhendo         TYPE STANDARD TABLE OF type_colhendo INITIAL SIZE 0,
      t_colhendo_tot     TYPE STANDARD TABLE OF type_soma INITIAL SIZE 0,
      r_2_weeks          TYPE RANGE OF yosemana_cota,
      r_2_next_weeks     TYPE RANGE OF yosemana_cota,
      r_next_week        TYPE RANGE OF yosemana_cota,
      wa_colhendo_tot    LIKE LINE OF t_colhendo_tot,
      t_resultado        TYPE STANDARD TABLE OF type_soma INITIAL SIZE 0,
      t_update           TYPE STANDARD TABLE OF zfmfp_colheita INITIAL SIZE 0,
      t_periodo          TYPE tstr_gensegyeartab,
      t_fieldcat         TYPE lvc_t_fcat,
      wa_layout          TYPE lvc_s_layo,
      wa_variant         TYPE disvariant,
      ok_code            LIKE sy-ucomm,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      row_id             TYPE lvc_t_roid WITH HEADER LINE,
      row_i              TYPE lvc_s_row,
      col_i              TYPE lvc_s_col,
      v_week_max         TYPE type_week,
      v_week             TYPE type_week,
      v_next_week        TYPE fti_week_year,
      o_ratio            TYPE REF TO data,
      o_brix             TYPE REF TO data,
      o_solidos          TYPE REF TO data.

CONSTANTS: BEGIN OF c_status_talhao,
             colheita       TYPE char1 VALUE '1',
             prog_sem_atual TYPE char1 VALUE '2',
             prog_prox_sem  TYPE char1 VALUE '3',
             sel_pomares    TYPE char1 VALUE '4',
           END OF c_status_talhao,

           BEGIN OF c_variedade,
             precoce  TYPE zfmgrp_variedade VALUE 'PRECOCE',
             pera     TYPE zfmgrp_variedade VALUE 'PERA',
             natal    TYPE zfmgrp_variedade VALUE 'NATAL',
             valencia TYPE zfmgrp_variedade VALUE 'VALÊNCIA',
             outros   TYPE zfmgrp_variedade VALUE 'OUTRAS',
           END OF c_variedade.

*Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.
PARAMETERS: p_table RADIOBUTTON GROUP grp MODIF ID typ DEFAULT 'X'
                    USER-COMMAND grp,
            p_alv   RADIOBUTTON GROUP grp MODIF ID typ.
SELECTION-SCREEN END OF BLOCK b0.

*Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_safra TYPE /agri/fmapcsh-gyear MATCHCODE OBJECT rscalyear MODIF ID sel.
SELECT-OPTIONS: s_imov  FOR ys4selpomares-cod_imov,
                s_tplma FOR /agri/glflot-tplma MODIF ID sel NO-DISPLAY,
                s_werks FOR t001w-werks MODIF ID sel.
PARAMETERS: p_sem TYPE fti_week_year
              AS LISTBOX VISIBLE LENGTH 26 USER-COMMAND v_week MODIF ID sel,
            p_box TYPE type_box.
SELECTION-SCREEN END OF BLOCK b1.

*Classificação 5
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_cl5 TYPE type_cl5
            AS LISTBOX VISIBLE LENGTH 36 USER-COMMAND v_cl5 MODIF ID sel.
SELECTION-SCREEN END OF BLOCK b2.

*Classificação 6
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
PARAMETERS: p_cl6 TYPE type_cl6
            AS LISTBOX VISIBLE LENGTH 36 USER-COMMAND v_cl6 MODIF ID sel.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:
      handle_data_changed_finished
                    FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_on_f4
                    FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender e_fieldname e_fieldvalue es_row_no
                    er_event_data et_bad_cells e_display,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.
    DATA : lr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
    METHODS: my_f4
      IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                et_bad_cells  TYPE lvc_t_modi
                es_row_no     TYPE lvc_s_roid
                er_event_data TYPE REF TO cl_alv_event_data
                e_display     TYPE c
                e_fieldname   TYPE lvc_fname
      EXPORTING lt_f4         TYPE ddshretval_table.

ENDCLASS. "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION .

*---------------------------------------------------------------------*
*       METHOD my_f4  insert here your own f4-help                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
  METHOD my_f4.

    DATA: wa_tab        LIKE LINE OF t_update,
          lt_fcat       TYPE lvc_t_fcat,
          lt_index_rows TYPE lvc_t_row,
          l_tabname     TYPE dd03v-tabname,
          l_fieldname   TYPE dd03v-fieldname,
          l_help_valu   TYPE help_info-fldvalue,
          lt_bad_cell   TYPE lvc_t_modi,
          lp_wa         TYPE REF TO data.

    FIELD-SYMBOLS: <l_field_value> TYPE any,
                   <ls_wa>         TYPE any.

    CALL METHOD sender->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = lt_fcat.

    READ TABLE t_update INDEX es_row_no-row_id INTO wa_tab.
    CREATE DATA lp_wa LIKE LINE OF t_update.
    ASSIGN lp_wa->* TO <ls_wa>.
    <ls_wa> = wa_tab.

    READ TABLE lt_fcat INTO DATA(lwa_fieldcat)
       WITH KEY fieldname = e_fieldname.
    IF sy-subrc EQ 0.
      MOVE lwa_fieldcat-ref_table TO l_tabname.
      MOVE lwa_fieldcat-fieldname TO l_fieldname.
      ASSIGN COMPONENT lwa_fieldcat-fieldname
        OF STRUCTURE wa_tab TO <l_field_value>.
      WRITE <l_field_value> TO l_help_valu.
    ENDIF.

  ENDMETHOD.                                                "my_f4

*-------------------------------------------------------------------
  METHOD handle_data_changed_finished .

    DATA: lwa_modi  TYPE lvc_s_modi.

    CHECK e_modified = 'X'.

    LOOP AT et_good_cells INTO lwa_modi.
      MOVE lwa_modi-row_id TO row_id-row_id.
      MOVE lwa_modi-row_id TO row_i-index.
      MOVE lwa_modi-fieldname TO col_i-fieldname.

    ENDLOOP.

    CALL METHOD gr_grid->refresh_table_display.
    CALL METHOD gr_grid->set_current_cell_via_id
      EXPORTING
        is_row_id    = row_i
        is_column_id = col_i.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD. "handle_data_changed_finished

*-------------------------------------------------------------------
  METHOD handle_toolbar.

  ENDMETHOD.

*-------------------------------------------------------------------
  METHOD handle_on_f4.

    er_event_data->m_event_handled = 'X'.

    CALL METHOD gr_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDMETHOD.

*-------------------------------------------------------------------
  METHOD handle_user_command.

  ENDMETHOD.                           "handle_user_command

ENDCLASS. "lcl_event_handler IMPLEMENTATION

DATA: gr_event_handler TYPE REF TO lcl_event_handler.

INITIALIZATION.
  PERFORM fill_default_values.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.
  IF sy-ucomm EQ 'ONLI'.
    PERFORM validate_param_values.
  ENDIF.

START-OF-SELECTION.
  PERFORM clear.
  PERFORM select_tables.

END-OF-SELECTION.
  IF p_alv EQ abap_true.
    PERFORM show_alv.
  ELSEIF p_table EQ abap_true.
    PERFORM update_table.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form FILL_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_default_values .

  DATA: lv_week       TYPE kweek,
        lv_week_n2    TYPE type_week,
        lv_begin_date TYPE char10,
        lv_end_date   TYPE char10,
        lv_today      TYPE scdatum,
        lt_yeartab    TYPE tstr_yeartab,
        lwa_yeartab   TYPE LINE OF tstr_yeartab,
        lwa_ttstr     TYPE ttstr,
        lt_dynpfields TYPE dynpread_tabtype,
        lwa_dynpfield TYPE dynpread,
        lt_values     TYPE vrm_values,
        lv_vrm_id     TYPE vrm_id.

  REFRESH: t_periodo.

  IF p_safra IS INITIAL.
    p_safra = sy-datum(4).
  ENDIF.

  IF p_sem IS INITIAL.
*...Current week value
    lv_today = sy-datum.
    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_today
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.

    p_sem = lv_week.

*...Maximum week value
    lwa_yeartab = sy-datum+0(4).
    APPEND lwa_yeartab TO lt_yeartab.
    ADD 1 TO lwa_yeartab.
    APPEND lwa_yeartab TO lt_yeartab.
*--BOC-T_T.KONNO-05.25.21
    SUBTRACT 2 FROM lwa_yeartab.
    APPEND lwa_yeartab TO lt_yeartab.
    SORT lt_yeartab.
*--EOC-T_T.KONNO-05.25.21
    CALL FUNCTION 'TSTR_PERIODS_WEEKS'
      EXPORTING
        it_yeartab   = lt_yeartab
        is_ttstr     = lwa_ttstr
      IMPORTING
        et_gensegtab = t_periodo
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.

    IF sy-subrc EQ 0.
      LOOP AT t_periodo INTO DATA(lwa_periodo).
        CLEAR lv_week_n2.
        LOOP AT lwa_periodo-gensegtab INTO DATA(lwa_week).
          ADD 1 TO lv_week_n2.
          INSERT INITIAL LINE INTO TABLE lt_values
            ASSIGNING FIELD-SYMBOL(<lwa_value>).
          IF sy-subrc EQ 0.
            <lwa_value>-key(2) = lv_week_n2.
            <lwa_value>-key+2(4) = lwa_periodo-year.
            <lwa_value>-text = lv_week_n2.
            WRITE: lwa_week-begin_date USING EDIT MASK '__/__/____' TO lv_begin_date,
                   lwa_week-end_date USING EDIT MASK '__/__/____' TO lv_end_date.
            CONCATENATE '(' lv_begin_date '-' lv_end_date ')' INTO <lwa_value>-text.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    lv_vrm_id = 'P_SEM'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = lv_vrm_id
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDIF.

  IF p_cl5 IS INITIAL.
    REFRESH: lt_values.
    lv_vrm_id = 'P_CL5'.
    p_cl5 = 'OPT1'.

    DO 5 TIMES.
      DATA(lv_index) = sy-index.
      INSERT INITIAL LINE INTO TABLE lt_values
        ASSIGNING <lwa_value>.
      IF sy-subrc EQ 0.
        CASE lv_index.
          WHEN 1.
            <lwa_value>-key = 'OPT1'.
*-- Ordenação pelo campo Kgss/cxs
            <lwa_value>-text = TEXT-t11.
          WHEN 2.
            <lwa_value>-key = 'OPT2'.
*-- Ordenação pela taxa de Kgss/cxs
            <lwa_value>-text = TEXT-t12.
          WHEN 3.
            <lwa_value>-key = 'OPT3'.
*...Ordenação pelo ratio
            <lwa_value>-text = TEXT-t08.
          WHEN 4.
            <lwa_value>-key = 'OPT4'.
*...Ordenação pelo brix
            <lwa_value>-text = TEXT-t09.
          WHEN 5.
            <lwa_value>-key = 'OPT5'.
*-- Ordenação pela taxa de Kgss/ha
            <lwa_value>-text = TEXT-t13.
        ENDCASE.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = lv_vrm_id
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDIF.

  IF p_cl6 IS INITIAL.
    REFRESH: lt_values.
    lv_vrm_id = 'P_CL6'.
    p_cl6 = 'OPT1'.

    DO 5 TIMES.
      lv_index = sy-index.
      INSERT INITIAL LINE INTO TABLE lt_values
        ASSIGNING <lwa_value>.
      IF sy-subrc EQ 0.
        CASE lv_index.
          WHEN 1.
            <lwa_value>-key = 'OPT1'.
*-- Ordenação pelo campo Kgss/cxs
            <lwa_value>-text = TEXT-t11.
          WHEN 2.
            <lwa_value>-key = 'OPT2'.
*--BOC-T_T.KONNO-05.25.21
*--Ordenação pelo campo taxa sólidos
*            <lwa_value>-text = TEXT-t05.
*-- Ordenação pela taxa de Kgss/cxs
            <lwa_value>-text = TEXT-t12.
*--EOC-T_T.KONNO-05.25.21
          WHEN 3.
            <lwa_value>-key = 'OPT3'.
*...Ordenação pelo ratio
            <lwa_value>-text = TEXT-t08.
          WHEN 4.
            <lwa_value>-key = 'OPT4'.
*...Ordenação pelo brix
            <lwa_value>-text = TEXT-t09.
          WHEN 5.
            <lwa_value>-key = 'OPT5'.
*-- Ordenação pela taxa de Kgss/ha
            <lwa_value>-text = TEXT-t13.
        ENDCASE.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = lv_vrm_id
        values          = lt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAM_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_param_values.

  DATA: lrt_imov   TYPE RANGE OF /agri/glflot-tplnr_fl,
        lrt_atnam  TYPE RANGE OF atnam,
        lrs_imov   LIKE LINE OF lrt_imov,
        lv_terreno TYPE /agri/glflot-tplnr_fl.

****Ownership
  CONSTANTS : BEGIN OF c_ownership,
                own         TYPE /agri/glownshp VALUE 'OW',
                third_party TYPE /agri/glownshp VALUE 'TP',
              END OF c_ownership.

  IF p_safra IS INITIAL.
    p_safra = sy-datum(4).
  ENDIF.

  IF s_werks IS INITIAL.
    INSERT INITIAL LINE INTO TABLE s_werks
      ASSIGNING FIELD-SYMBOL(<lwa_werks>).
    IF sy-subrc EQ 0.
      <lwa_werks> = 'IEQ'.
      <lwa_werks>-low = '1000'.
      <lwa_werks>-high = '1999'.
    ENDIF.
  ENDIF.

  IF p_sem IS INITIAL.
    p_sem = v_week.
  ENDIF.

  IF p_box IS INITIAL.
    p_box = 2000.
  ENDIF.

  IF p_cl5 IS INITIAL.
    p_cl5 = 'OPT1'.
  ENDIF.

  IF p_cl6 IS INITIAL.
    p_cl6 = 'OPT2'.
  ENDIF.

  IF p_cl5 IS NOT INITIAL
  AND p_cl6 IS NOT INITIAL.
    IF p_cl5 EQ p_cl6.
*...Os campos de classificação 5 e 6 devem ser diferentes!
      MESSAGE i029(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF s_imov[] IS INITIAL.
    INSERT INITIAL LINE INTO TABLE s_tplma
      ASSIGNING FIELD-SYMBOL(<ls_tplma>).
    IF sy-subrc EQ 0.
      <ls_tplma> = 'IEQ'.

      SELECT tplnr_fl AS low
        INTO CORRESPONDING FIELDS OF TABLE @lrt_imov
        FROM /agri/glflot
       WHERE tplma IN @s_tplma[].

      lrs_imov-sign = 'I'.
      lrs_imov-option = 'EQ'.
      MODIFY lrt_imov FROM lrs_imov TRANSPORTING sign option WHERE low <> ''.
      s_tplma[] = lrt_imov[].

      LOOP AT s_tplma INTO DATA(ls_tplma).
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = ls_tplma-low
          IMPORTING
            output = ls_tplma-low.

        SHIFT ls_tplma-low LEFT DELETING LEADING '0'.

        INSERT INITIAL LINE INTO TABLE s_imov
          ASSIGNING FIELD-SYMBOL(<ls_imov>).
        IF sy-subrc EQ 0.
          <ls_imov> = 'IEQ'.
          <ls_imov>-low = ls_tplma-low(6).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    REFRESH s_tplma.
    LOOP AT s_imov INTO DATA(lwa_imov).
      INSERT INITIAL LINE INTO TABLE s_tplma
        ASSIGNING FIELD-SYMBOL(<lwa_tplma>).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lwa_imov TO <lwa_tplma>.
        IF <lwa_tplma>-low IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_tplma>-low
            IMPORTING
              output     = <lwa_tplma>-low
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.

        IF <lwa_tplma>-high IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_tplma>-high
            IMPORTING
              output     = <lwa_tplma>-high
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT tplnr_fl, ownshp
    INTO TABLE @DATA(lt_terreno)
    FROM /agri/glflot
   WHERE tplnr_fl IN @s_tplma[].

  SORT lt_terreno BY tplnr_fl.

  INSERT INITIAL LINE INTO TABLE lrt_atnam
    ASSIGNING FIELD-SYMBOL(<lrs_atnam>).
  IF sy-subrc EQ 0.
    <lrs_atnam> = 'IEQ'.
    <lrs_atnam>-low = 'CIT-IMOVEL-ARREND'.
  ENDIF.

  SELECT c~atinn, c~adzhl, c~atnam,
         c~atfor, c~anzst, c~anzdz,
         g~tplnr_fl, g~atwrt
    FROM cabn AS c
    INNER JOIN /agri/glflatv AS g
    ON c~atinn EQ g~atinn
    INTO TABLE @DATA(lt_glflatv)
   WHERE c~atnam    IN @lrt_atnam[]
     AND g~tplnr_fl IN @s_tplma[].

  SORT lt_glflatv BY tplnr_fl.

  LOOP AT s_tplma INTO ls_tplma.
    DATA(lv_tabix) = sy-tabix.
    READ TABLE lt_terreno INTO DATA(ls_terreno)
      WITH KEY tplnr_fl = ls_tplma-low BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ls_terreno-ownshp EQ c_ownership-third_party.
        READ TABLE lt_glflatv INTO DATA(ls_glflatv)
          WITH KEY tplnr_fl = ls_tplma-low BINARY SEARCH.
        IF sy-subrc EQ 0
        AND ls_glflatv-atwrt IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = ls_tplma-low
            IMPORTING
              output = lv_terreno.

          DELETE s_imov WHERE low EQ lv_terreno.
          DELETE s_tplma INDEX lv_tabix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF s_tplma[] IS INITIAL.
*-- Não existem imóveis válidos para seleção!
    MESSAGE i048(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_TABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_tables .

  TYPES: BEGIN OF ltype_cont,
           cod_imov     TYPE zfmcod_imov,
           sequenciador TYPE zfmsequenciador,
           setor        TYPE zfmsetor,
           faixa_ratio  TYPE zfmfaixa_ratio,
         END OF ltype_cont,

         BEGIN OF ly_solidosha,
           safra         TYPE zfmano_safra,
           tplnr_fl      TYPE /agri/gltplnr_fl,
           semana        TYPE zabs_del_semana,
           cod_imov      TYPE zfmcod_imov,
           taxa_variacao TYPE zabs_del_tx_var,
           talhao        TYPE zfmtalhao,
           tplnr_fl_out  TYPE /agri/gltplnr_fl,
         END OF ly_solidosha,

         BEGIN OF ly_terreno,
           tplnr_fl TYPE /agri/gltplnr_fl,
           ownshp	  TYPE /agri/glownshp,
         END OF ly_terreno,

         ltype_cont_tab TYPE SORTED TABLE OF ltype_cont
           WITH UNIQUE KEY cod_imov setor faixa_ratio.

  DATA: lr_bwart       TYPE RANGE OF bwart,
        lwa_bwart      LIKE LINE OF lr_bwart,
        lt_solidosha   TYPE STANDARD TABLE OF ly_solidosha INITIAL SIZE 0,
        lt_solidos_aux LIKE lt_solidosha,
        lt_terreno     TYPE STANDARD TABLE OF ly_terreno INITIAL SIZE 0,
        lt_controle    TYPE SORTED TABLE OF ltype_cont
                       WITH UNIQUE KEY cod_imov setor faixa_ratio INITIAL SIZE 0,
        lwa_controle   LIKE LINE OF lt_controle,
        lv_talhao      TYPE zfmtalhao,
        lv_cit_florada TYPE /agri/gatwrt,
        lv_florada     TYPE zfmflorada,
        lr_budat       TYPE RANGE OF budat,
        lr_matnr       TYPE RANGE OF matnr,
        lr_atnam       TYPE RANGE OF atnam,
        lr_atnam_ref4  TYPE RANGE OF atnam,
        lr_faixa_safra TYPE RANGE OF yoparam,
        lr_atinn       TYPE RANGE OF atinn,
        lwa_atinn      LIKE LINE OF lr_atinn,
        lr_atinn_ref4  TYPE RANGE OF atinn,
        lr_tplnr       TYPE RANGE OF /agri/glflot-tplnr_fl,
        lr_mpgrp       TYPE RANGE OF /agri/glmpgrp,
        lv_year        TYPE tstr_year,
        lv_sem_proj    TYPE zfmsem_proj,
        lv_third_week  TYPE zfmsem_proj,
        lr_tplma_ref4  TYPE RANGE OF /agri/glflot-tplma,
        lt_glflot_ref4 LIKE t_glflot,
        lv_imovel      TYPE /agri/gltplnr_fl,
        lv_novo_imovel TYPE /agri/gltplnr_fl,
        lv_c15         TYPE fieldname,
        lv_c16         TYPE fieldname,
        lv_p60(6)      TYPE p DECIMALS 0,
        lv_days        TYPE pea_scrdd,
        lv_begda       TYPE sydatum,
        lv_months      TYPE j_1itaxvar-j_1itaxam1,
        lv_years       TYPE j_1itaxvar-j_1itaxam1,
        lv_ano_safra_f TYPE atflv,
        lv_i           TYPE i,
        lv_tabix       TYPE sytabix,
        lv_semana      TYPE numc2.

  CONSTANTS: BEGIN OF lc_tipo_safra,
               tecnica  TYPE zfmtpsafra VALUE 'T',
               contabil TYPE zfmtpsafra VALUE 'C',
             END OF lc_tipo_safra,

             BEGIN OF lc_faixa,
               acidez TYPE yoparam VALUE 'FAIXA_ACIDEZ',
               brix   TYPE yoparam VALUE 'FAIXA_BRIX_2',
               ratio  TYPE yoparam VALUE 'FAIXA_RATIO_2',
             END OF lc_faixa.

****Ownership
  CONSTANTS : BEGIN OF c_ownership,
                own         TYPE /agri/glownshp VALUE 'OW',
                third_party TYPE /agri/glownshp VALUE 'TP',
              END OF c_ownership.

  REFRESH: t_matdoc, t_mch1, t_cabn, t_ausp.

  DATA(lv_fornecedor) = 'YP'.
  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      input  = lv_fornecedor
    IMPORTING
      output = lv_fornecedor.

  SELECT bwart AS low
    INTO CORRESPONDING FIELDS OF TABLE @lr_bwart
    FROM t156
   WHERE bwart IN ('Z93','Z94').

  lwa_bwart = 'IEQ'.
  MODIFY lr_bwart FROM lwa_bwart TRANSPORTING sign option WHERE low <> ''.

  lv_year = p_sem(4).
  lv_tabix = p_sem+4(2).
  READ TABLE t_periodo INTO DATA(lwa_periodo)
    WITH KEY year = lv_year.
  IF sy-subrc EQ 0.
    READ TABLE lwa_periodo-gensegtab INTO DATA(lwa_week)
      INDEX lv_tabix.
    IF sy-subrc EQ 0.
      INSERT INITIAL LINE INTO TABLE lr_budat ASSIGNING FIELD-SYMBOL(<lwa_budat>).
      IF sy-subrc EQ 0.
        <lwa_budat> = 'IBT'.
        lv_begda = lwa_week-begin_date - 7.
        <lwa_budat>-low = lv_begda.
        <lwa_budat>-high = sy-datum.
      ENDIF.
    ENDIF.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lr_matnr ASSIGNING FIELD-SYMBOL(<lwa_matnr>).
  IF sy-subrc EQ 0.
    <lwa_matnr> = 'IBT'.

    <lwa_matnr>-low = '1'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <lwa_matnr>-low
      IMPORTING
        output       = <lwa_matnr>-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    <lwa_matnr>-high = '99'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <lwa_matnr>-high
      IMPORTING
        output       = <lwa_matnr>-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
  ENDIF.

  SELECT werks, menge, budat, bwart,
         matnr, charg, lifnr
    FROM matdoc
    INTO TABLE @t_matdoc
   WHERE werks IN @s_werks
     AND budat IN @lr_budat
     AND bwart IN @lr_bwart
     AND matnr IN @lr_matnr.

  IF sy-subrc EQ 0.
    SORT t_matdoc BY matnr charg.

    SELECT matnr,
           charg,
           cuobj_bm
      FROM mch1
      INTO CORRESPONDING FIELDS OF TABLE @t_mch1
      FOR ALL ENTRIES IN @t_matdoc
     WHERE matnr = @t_matdoc-matnr
       AND charg = @t_matdoc-charg.

    SORT t_mch1 BY matnr charg.

    LOOP AT t_mch1 ASSIGNING FIELD-SYMBOL(<lwa_mch1>).
      <lwa_mch1>-objek = <lwa_mch1>-cuobj_bm.
    ENDLOOP.
  ENDIF.

  DO 5 TIMES.
    DATA(lv_index) = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_atnam ASSIGNING FIELD-SYMBOL(<lwa_atnam>).
    IF sy-subrc EQ 0.
      <lwa_atnam> = 'IEQ'.
      CASE lv_index.
        WHEN '1'.
          <lwa_atnam>-low = 'RC_FR_COD_IM'.
        WHEN '2'.
          <lwa_atnam>-low = 'RC_FR_TALH'.
        WHEN '3'.
          <lwa_atnam>-low = 'RATIOFRUTA'.
        WHEN '4'.
          <lwa_atnam>-low = 'BRIXCORRIGIDOFRUTA'.
        WHEN '5'.
          <lwa_atnam>-low = 'REC-QM-SOLIDOS_SOLUVEIS'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SELECT atinn, adzhl, atnam,
         atfor, anzst, anzdz
    FROM cabn
    INTO TABLE @t_cabn
   WHERE atnam IN @lr_atnam.

  IF sy-subrc EQ 0.
    SORT t_cabn BY atnam atinn.
  ENDIF.

  SELECT *
    FROM zfmfpsafras
    INTO TABLE @DATA(lt_safras)
   WHERE ano_safra  EQ @p_safra
     AND tipo_safra EQ @lc_tipo_safra-tecnica.

  SORT lt_safras BY inicio_safra fim_safra.

  lr_atinn = CORRESPONDING #( t_cabn MAPPING low = atinn ).
  lwa_atinn = 'IEQ'.
  MODIFY lr_atinn FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

  SELECT objek, atinn, atzhl, mafid,
         klart, adzhl, atwrt, atflv
    FROM ausp
    INTO TABLE @t_ausp
    FOR ALL ENTRIES IN @t_mch1
   WHERE objek EQ @t_mch1-objek
     AND atinn IN @lr_atinn.

  SORT t_ausp BY objek atinn.

  READ TABLE t_cabn INTO DATA(lwa_imovel)
    WITH KEY atnam = 'RC_FR_COD_IM'.

  READ TABLE t_cabn INTO DATA(lwa_talhao)
    WITH KEY atnam = 'RC_FR_TALH'.

  READ TABLE t_cabn INTO DATA(lwa_ratio)
    WITH KEY atnam = 'RATIOFRUTA'.
  IF sy-subrc EQ 0.
    CREATE DATA o_ratio TYPE p LENGTH lwa_ratio-anzst DECIMALS lwa_ratio-anzdz.
    ASSIGN o_ratio->* TO FIELD-SYMBOL(<ratio_type>).
  ENDIF.

  READ TABLE t_cabn INTO DATA(lwa_brix)
    WITH KEY atnam = 'BRIXCORRIGIDOFRUTA'.
  IF sy-subrc EQ 0.
    CREATE DATA o_brix TYPE p LENGTH lwa_brix-anzst DECIMALS lwa_brix-anzdz.
    ASSIGN o_brix->* TO FIELD-SYMBOL(<brix_type>).
  ENDIF.

  READ TABLE t_cabn INTO DATA(lwa_solidos)
    WITH KEY atnam = 'REC-QM-SOLIDOS_SOLUVEIS'.
  IF sy-subrc EQ 0.
    CREATE DATA o_solidos TYPE p LENGTH lwa_solidos-anzst DECIMALS lwa_solidos-anzdz.
    ASSIGN o_solidos->* TO FIELD-SYMBOL(<solidos_type>).
  ENDIF.

  LOOP AT t_matdoc INTO DATA(lwa_matdoc).
    LOOP AT lt_safras INTO DATA(lwa_safra)
      WHERE inicio_safra <= lwa_matdoc-budat
        AND fim_safra >= lwa_matdoc-budat.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 4.
      CLEAR lwa_safra.
    ENDIF.

    READ TABLE t_mch1 INTO DATA(lwa_mch1)
      WITH KEY matnr = lwa_matdoc-matnr
               charg = lwa_matdoc-charg BINARY SEARCH.
    WHILE sy-subrc = 0.
      lv_tabix = sy-tabix + 1.
      INSERT INITIAL LINE INTO TABLE t_colhendo
        ASSIGNING FIELD-SYMBOL(<lwa_colhendo>).
      IF sy-subrc EQ 0.
        <lwa_colhendo>-ano_safra = lwa_safra-ano_safra.
        <lwa_colhendo>-werks    = lwa_matdoc-werks.
        IF lwa_matdoc-bwart EQ 'Z94'.
          <lwa_colhendo>-menge = lwa_matdoc-menge * -1.
        ELSE.
          <lwa_colhendo>-menge = lwa_matdoc-menge.
        ENDIF.
        <lwa_colhendo>-budat    = lwa_matdoc-budat.
        <lwa_colhendo>-bwart    = lwa_matdoc-bwart.
        <lwa_colhendo>-matnr    = lwa_matdoc-matnr.
        <lwa_colhendo>-charg    = lwa_matdoc-charg.
        <lwa_colhendo>-cuobj_bm = lwa_mch1-cuobj_bm.
        <lwa_colhendo>-objek    = lwa_mch1-objek.

        READ TABLE t_ausp INTO DATA(lwa_ausp)
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_imovel-atinn BINARY SEARCH.
        IF sy-subrc EQ 0
        AND lwa_ausp-atwrt IS NOT INITIAL.
          <lwa_colhendo>-cod_imov_c = lwa_ausp-atwrt.
          CONDENSE <lwa_colhendo>-cod_imov_c.

          IF <lwa_colhendo>-cod_imov_c IS NOT INITIAL.
            CALL FUNCTION 'CHAR_FLTP_CONVERSION'
              EXPORTING
                string             = <lwa_colhendo>-cod_imov_c
              IMPORTING
                flstr              = <lwa_colhendo>-cod_imov_f
              EXCEPTIONS
                exponent_too_big   = 1
                exponent_too_small = 2
                string_not_fltp    = 3
                too_many_decim     = 4
                OTHERS             = 5.
          ENDIF.
        ENDIF.

        READ TABLE t_ausp INTO lwa_ausp
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_talhao-atinn BINARY SEARCH.
        IF sy-subrc EQ 0
        AND lwa_ausp-atwrt IS NOT INITIAL.
          <lwa_colhendo>-talhao_c = lwa_ausp-atwrt.
          CONDENSE <lwa_colhendo>-talhao_c.

          IF <lwa_colhendo>-talhao_c IS NOT INITIAL.
            CALL FUNCTION 'CHAR_FLTP_CONVERSION'
              EXPORTING
                string             = <lwa_colhendo>-talhao_c
              IMPORTING
                flstr              = <lwa_colhendo>-talhao_f
              EXCEPTIONS
                exponent_too_big   = 1
                exponent_too_small = 2
                string_not_fltp    = 3
                too_many_decim     = 4
                OTHERS             = 5.
          ENDIF.
        ENDIF.

        READ TABLE t_ausp INTO lwa_ausp
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_ratio-atinn BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lwa_colhendo>-ratio = lwa_ausp-atflv.
          <lwa_colhendo>-pond_ratio = <lwa_colhendo>-ratio * <lwa_colhendo>-menge.
        ENDIF.

        READ TABLE t_ausp INTO lwa_ausp
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_brix-atinn BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lwa_colhendo>-brix = lwa_ausp-atflv.
          <lwa_colhendo>-pond_brix = <lwa_colhendo>-brix * <lwa_colhendo>-menge.
        ENDIF.

        READ TABLE t_ausp INTO lwa_ausp
          WITH KEY objek = lwa_mch1-objek
                   atinn = lwa_solidos-atinn BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lwa_colhendo>-solidos = lwa_ausp-atflv.
          <lwa_colhendo>-pond_solidos = <lwa_colhendo>-solidos  * <lwa_colhendo>-menge.
        ENDIF.
      ENDIF.
      READ TABLE t_mch1 INTO lwa_mch1 INDEX lv_tabix
        COMPARING matnr charg.
    ENDWHILE.
  ENDLOOP.

*...Registros inválidos
  DELETE t_colhendo WHERE cod_imov_f IS INITIAL.

  SORT t_colhendo BY ano_safra cod_imov_c talhao_c matnr menge ratio brix solidos.
  LOOP AT t_colhendo INTO DATA(lwa_colhendo).
    CLEAR wa_colhendo_tot.
    wa_colhendo_tot-ano_safra = lwa_colhendo-ano_safra.
    wa_colhendo_tot-tplnr_fl = lwa_colhendo-tplnr_fl.
    wa_colhendo_tot-cod_imovel_c = lwa_colhendo-cod_imov_c.
    CONDENSE wa_colhendo_tot-cod_imovel_c.
    wa_colhendo_tot-cod_imov = wa_colhendo_tot-cod_imovel_c(6).
    wa_colhendo_tot-talhao_c = lwa_colhendo-talhao_c.
    CONDENSE wa_colhendo_tot-talhao_c.
    wa_colhendo_tot-talhao = wa_colhendo_tot-talhao_c(5).
    UNPACK wa_colhendo_tot-talhao TO wa_colhendo_tot-talhao.

    IF wa_colhendo_tot-cod_imov IS NOT INITIAL.
      CONCATENATE wa_colhendo_tot-cod_imov '-' wa_colhendo_tot-talhao
                INTO wa_colhendo_tot-tplnr_fl.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = wa_colhendo_tot-tplnr_fl
        IMPORTING
          output     = wa_colhendo_tot-tplnr_fl
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc NE 0.
        wa_colhendo_tot-cod_invalido = abap_true.
      ENDIF.
    ELSE.
      wa_colhendo_tot-cod_invalido = abap_true.
    ENDIF.

    wa_colhendo_tot-matnr = lwa_colhendo-matnr.
    wa_colhendo_tot-menge = lwa_colhendo-menge.
    wa_colhendo_tot-ratio_f = lwa_colhendo-pond_ratio.
    wa_colhendo_tot-brix_f = lwa_colhendo-pond_brix.
    wa_colhendo_tot-solidos_f = lwa_colhendo-pond_solidos.
    COLLECT wa_colhendo_tot INTO t_colhendo_tot.
  ENDLOOP.

  LOOP AT t_colhendo_tot ASSIGNING FIELD-SYMBOL(<lwa_colhendo_tot>).
    <lwa_colhendo_tot>-cod_faz_input = <lwa_colhendo_tot>-cod_imov.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = <lwa_colhendo_tot>-cod_faz_input
      IMPORTING
        output     = <lwa_colhendo_tot>-cod_faz_input
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.

    <lwa_colhendo_tot>-cod_faz_output = <lwa_colhendo_tot>-cod_imov.
    IF <lwa_colhendo_tot>-menge IS NOT INITIAL.
      <lwa_colhendo_tot>-media_ratio = <lwa_colhendo_tot>-ratio_f / <lwa_colhendo_tot>-menge.
      <lwa_colhendo_tot>-media_brix = <lwa_colhendo_tot>-brix_f / <lwa_colhendo_tot>-menge.
      <lwa_colhendo_tot>-media_solidos = <lwa_colhendo_tot>-solidos_f / <lwa_colhendo_tot>-menge.

      IF <ratio_type> IS ASSIGNED.
        <ratio_type> = <lwa_colhendo_tot>-media_ratio.
        <lwa_colhendo_tot>-media_ratio_c = <ratio_type>.
      ENDIF.

      IF <brix_type> IS ASSIGNED.
        <brix_type> = <lwa_colhendo_tot>-media_brix.
        <lwa_colhendo_tot>-media_brix_c = <brix_type>.
      ENDIF.

      IF <solidos_type> IS ASSIGNED.
        <solidos_type> = <lwa_colhendo_tot>-media_solidos.
        <lwa_colhendo_tot>-media_solidos_c = <solidos_type>.
      ENDIF.

      CONDENSE: <lwa_colhendo_tot>-media_ratio_c,
                <lwa_colhendo_tot>-media_brix_c,
                <lwa_colhendo_tot>-media_solidos_c.
    ENDIF.
  ENDLOOP.

  IF s_imov[] IS NOT INITIAL.
    DELETE t_colhendo_tot WHERE cod_faz_output NOT IN s_imov[].
  ENDIF.

  SELECT *
    FROM ys4selpomares
    INTO TABLE @DATA(lt_pomares)
   WHERE cod_imov IN @s_imov[]
     AND sem_proj EQ @p_sem
     AND safra    EQ @p_safra.

  SORT lt_pomares BY cod_var cod_imov talhao.

  PERFORM check_next_week_numc USING p_sem
                            CHANGING v_next_week.

  IF t_colhendo_tot[] IS NOT INITIAL.
    LOOP AT t_colhendo_tot ASSIGNING <lwa_colhendo_tot>.
      <lwa_colhendo_tot>-referencia = c_status_talhao-colheita.
      <lwa_colhendo_tot>-semana_prog = v_next_week.
      IF <lwa_colhendo_tot>-menge LT p_box.
        READ TABLE lt_pomares INTO DATA(lwa_pomares)
          WITH KEY cod_var  = <lwa_colhendo_tot>-matnr
                   cod_imov = <lwa_colhendo_tot>-cod_imov
                   talhao   = <lwa_colhendo_tot>-talhao BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lwa_colhendo_tot>-media_brix      = lwa_pomares-brix_proj.
          <lwa_colhendo_tot>-media_brix_c    = lwa_pomares-brix_proj.
          <lwa_colhendo_tot>-media_ratio     = lwa_pomares-ratio_proj.
          <lwa_colhendo_tot>-media_ratio_c   = lwa_pomares-ratio_proj.
          <lwa_colhendo_tot>-media_solidos   = lwa_pomares-kgss_proj.
          <lwa_colhendo_tot>-media_solidos_c = lwa_pomares-kgss_proj.

          CONDENSE: <lwa_colhendo_tot>-media_ratio_c,
                    <lwa_colhendo_tot>-media_brix_c,
                    <lwa_colhendo_tot>-media_solidos_c.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DO 2 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE r_2_weeks
      ASSIGNING FIELD-SYMBOL(<lwa_2_weeks>).
    IF sy-subrc EQ 0.
      CASE lv_index.
        WHEN '1'.
          <lwa_2_weeks> = 'IEQ'.
          <lwa_2_weeks>-low = p_sem.
        WHEN '2'.
          <lwa_2_weeks> = 'IEQ'.
          <lwa_2_weeks>-low = v_next_week.
      ENDCASE.
    ENDIF.
  ENDDO.

*...Acesso deve considerar SEMANA ATUAL (referência = 2)
*...e PRÓXIMA SEMANA (referência = 3)
  SELECT *
    INTO TABLE @DATA(lt_cotaseg)
    FROM yocotaseg
   WHERE semana IN @r_2_weeks[]
     AND imov   IN @s_imov[].

  SORT lt_cotaseg BY imov talhao dtvlcota DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_cotaseg COMPARING imov talhao.

  SORT t_colhendo_tot BY matnr cod_imov talhao.
  t_resultado[] = t_colhendo_tot[].

*...Verifica talhões programados para semana corrente (Referência = 2)
*...e para a próxima semana (Referência = 3)
  LOOP AT lt_cotaseg INTO DATA(lwa_cotaseg).
*...Verifica se talhão está em coheita (Referência = 1).
*...Caso exista, deve ser desconsiderado.
    READ TABLE t_colhendo_tot TRANSPORTING NO FIELDS
      WITH KEY matnr       = lwa_cotaseg-variedade
               cod_imov    = lwa_cotaseg-imov
               talhao      = lwa_cotaseg-talhao BINARY SEARCH.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE t_resultado
        ASSIGNING FIELD-SYMBOL(<lwa_resultado>).
      IF sy-subrc EQ 0.
        PERFORM check_next_week USING lwa_cotaseg-semana
                             CHANGING <lwa_resultado>-semana_prog.
        <lwa_resultado>-florada = lwa_cotaseg-florada.
        <lwa_resultado>-matnr = lwa_cotaseg-variedade.
        <lwa_resultado>-cod_imov = <lwa_resultado>-cod_imovel_c = lwa_cotaseg-imov.
        <lwa_resultado>-talhao = <lwa_resultado>-talhao_c = lwa_cotaseg-talhao.
        <lwa_resultado>-lifnr = lwa_cotaseg-lifnr.
        IF lwa_cotaseg-semana EQ p_sem.
          <lwa_resultado>-referencia = c_status_talhao-prog_sem_atual.
        ELSEIF lwa_cotaseg-semana GT p_sem.
          <lwa_resultado>-referencia = c_status_talhao-prog_prox_sem.
        ENDIF.
        LOOP AT lt_safras INTO lwa_safra
          WHERE inicio_safra <= lwa_cotaseg-dtvlcota
            AND fim_safra >= lwa_cotaseg-dtvlcota.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          <lwa_resultado>-ano_safra = lwa_safra-ano_safra.
        ENDIF.
        <lwa_resultado>-media_ratio = lwa_cotaseg-ratiomed.
        <lwa_resultado>-media_brix = lwa_cotaseg-brixmed.
        <lwa_resultado>-media_solidos = lwa_cotaseg-solidmed.

        IF <ratio_type> IS ASSIGNED.
          <ratio_type> = <lwa_resultado>-media_ratio.
          <lwa_resultado>-media_ratio_c = <ratio_type>.
        ENDIF.

        IF <brix_type> IS ASSIGNED.
          <brix_type> = <lwa_resultado>-media_brix.
          <lwa_resultado>-media_brix_c = <brix_type>.
        ENDIF.

        IF <solidos_type> IS ASSIGNED.
          <solidos_type> = <lwa_resultado>-media_solidos.
          <lwa_resultado>-media_solidos_c = <solidos_type>.
        ENDIF.

        CONDENSE: <lwa_resultado>-media_ratio_c,
                  <lwa_resultado>-media_brix_c,
                  <lwa_resultado>-media_solidos_c.

        IF <lwa_resultado>-cod_imov IS NOT INITIAL.
          CONCATENATE <lwa_resultado>-cod_imov '-' <lwa_resultado>-talhao
                    INTO <lwa_resultado>-tplnr_fl.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_resultado>-tplnr_fl
            IMPORTING
              output     = <lwa_resultado>-tplnr_fl
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
          IF sy-subrc NE 0.
            <lwa_resultado>-cod_invalido = abap_true.
          ENDIF.
        ELSE.
          <lwa_resultado>-cod_invalido = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  REFRESH r_next_week.
  CLEAR lv_sem_proj.
  DO 2 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE r_next_week
      ASSIGNING FIELD-SYMBOL(<lwa_next_week>).
    IF sy-subrc EQ 0.
      IF lv_sem_proj IS INITIAL.
        lv_sem_proj = p_sem.
      ENDIF.
      PERFORM check_next_week_numc USING lv_sem_proj
                                CHANGING lv_sem_proj.
      <lwa_next_week> = 'IEQ'.
      <lwa_next_week>-low = lv_sem_proj.
    ENDIF.
  ENDDO.

*...Verifica seleção de pomares (Referência = 4)
*...considera somente PRÓXIMA SEMANA
  SELECT *
    FROM ys4selpomares
    INTO TABLE @DATA(lt_pomares_ref4)
   WHERE cod_imov IN @s_imov[]
     AND sem_proj IN @r_next_week
     AND safra    EQ @p_safra.

  IF sy-subrc EQ 0.
    LOOP AT lt_pomares_ref4 INTO DATA(lwa_pomar_ref4).
      INSERT INITIAL LINE INTO TABLE lr_tplma_ref4
        ASSIGNING FIELD-SYMBOL(<lwa_tplma_ref4>).
      IF sy-subrc EQ 0.
        <lwa_tplma_ref4> = 'IEQ'.
        UNPACK lwa_pomar_ref4-talhao TO lwa_pomar_ref4-talhao.
        CONCATENATE lwa_pomar_ref4-cod_imov '-' lwa_pomar_ref4-talhao
          INTO <lwa_tplma_ref4>-low.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lwa_tplma_ref4>-low
          IMPORTING
            output     = <lwa_tplma_ref4>-low
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
      ENDIF.

      INSERT INITIAL LINE INTO TABLE t_resultado
        ASSIGNING <lwa_resultado>.
      IF sy-subrc EQ 0.
        <lwa_resultado>-matnr = lwa_pomar_ref4-cod_var.
        <lwa_resultado>-florada = lwa_pomar_ref4-florada.
        <lwa_resultado>-lifnr = lwa_pomar_ref4-cd_fornecedor.
        <lwa_resultado>-cod_imovel_c = <lwa_resultado>-cod_imov = lwa_pomar_ref4-cod_imov.
        <lwa_resultado>-talhao_c = <lwa_resultado>-talhao = lwa_pomar_ref4-talhao.
        <lwa_resultado>-referencia = c_status_talhao-sel_pomares.
        READ TABLE t_resultado INTO DATA(lwa_resultado_aux)
          WITH KEY cod_imovel_c = lwa_pomar_ref4-cod_imov
                   talhao_c     = lwa_pomar_ref4-talhao.
        IF sy-subrc EQ 0.
          <lwa_resultado>-referencia = lwa_resultado_aux-referencia.
        ENDIF.
        <lwa_resultado>-zzplantyear = lwa_pomar_ref4-plantio.
        <lwa_resultado>-semana_prog = lwa_pomar_ref4-sem_proj.
        <lwa_resultado>-ano_safra = lwa_pomar_ref4-safra.
        <lwa_resultado>-tplnr_fl = <lwa_tplma_ref4>-low.
        <lwa_resultado>-media_ratio = lwa_pomar_ref4-ratio_proj.
        <lwa_resultado>-media_brix = lwa_pomar_ref4-brix_proj.
        <lwa_resultado>-media_solidos = lwa_pomar_ref4-kgss_proj.
        <lwa_resultado>-cod_faz_input = <lwa_resultado>-cod_imov.
        <lwa_resultado>-cod_faz_output = <lwa_resultado>-cod_imov.

        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lwa_resultado>-cod_faz_input
          IMPORTING
            output     = <lwa_resultado>-cod_faz_input
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        IF <ratio_type> IS ASSIGNED.
          <ratio_type> = <lwa_resultado>-media_ratio.
          <lwa_resultado>-media_ratio_c = <ratio_type>.
        ENDIF.

        IF <brix_type> IS ASSIGNED.
          <brix_type> = <lwa_resultado>-media_brix.
          <lwa_resultado>-media_brix_c = <brix_type>.
        ENDIF.

        IF <solidos_type> IS ASSIGNED.
          <solidos_type> = <lwa_resultado>-media_solidos.
          <lwa_resultado>-media_solidos_c = <solidos_type>.
        ENDIF.

        CONDENSE: <lwa_resultado>-media_ratio_c,
                  <lwa_resultado>-media_brix_c,
                  <lwa_resultado>-media_solidos_c.

        IF <lwa_resultado>-cod_imov IS NOT INITIAL.
          CONCATENATE <lwa_resultado>-cod_imov '-' <lwa_resultado>-talhao
                    INTO <lwa_resultado>-tplnr_fl.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_resultado>-tplnr_fl
            IMPORTING
              output     = <lwa_resultado>-tplnr_fl
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
          IF sy-subrc NE 0.
            <lwa_resultado>-cod_invalido = abap_true.
          ENDIF.
        ELSE.
          <lwa_resultado>-cod_invalido = abap_true.
        ENDIF.

        <lwa_resultado>-ano_plantio = lwa_pomar_ref4-plantio.
      ENDIF.
    ENDLOOP.

    DELETE t_resultado WHERE cod_invalido EQ abap_true.

    SORT t_resultado BY semana_prog matnr cod_imov talhao referencia.
    DELETE ADJACENT DUPLICATES FROM t_resultado
      COMPARING semana_prog matnr cod_imov talhao.

    DELETE ADJACENT DUPLICATES FROM t_resultado
      COMPARING referencia ano_safra semana_prog tplnr_fl
      cod_imov talhao matnr.

    IF t_resultado[] IS NOT INITIAL.
      DO 2 TIMES.
        lv_index = sy-index.
        INSERT INITIAL LINE INTO TABLE lr_mpgrp
          ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
        IF sy-subrc EQ 0.
          <lwa_mpgrp> = 'IEQ'.
          CASE lv_index.
            WHEN '1'.
              <lwa_mpgrp>-low = 'ENCERRAR_FLORADA'.
            WHEN '2'.
              <lwa_mpgrp>-low = 'ENCERRAR_COLHEITA'.
          ENDCASE.
        ENDIF.
      ENDDO.

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~equnr, h~mpgrp,
             v~atzhl, v~atwrt, v~atflv,
             c~atinn, c~atnam
        INTO TABLE @DATA(lt_glmdhdr_join)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @t_resultado
       WHERE mdtyp    EQ 'ZPTA' "Medições ao nível de safra
         AND tplnr_fl EQ @t_resultado-tplnr_fl
         AND mpgrp    IN @lr_mpgrp.
    ENDIF.
  ENDIF.

  SELECT tplnr_fl, pltxt, tplma, iwerk, ptrno,
         strno, beber1, setor_orig, stort1
    FROM /agri/glflot
    INTO TABLE @lt_glflot_ref4
   WHERE tplnr_fl IN @lr_tplma_ref4.

  IF lt_glflot_ref4[] IS NOT INITIAL.
    DO 2 TIMES.
      lv_index = sy-index.
      INSERT INITIAL LINE INTO TABLE lr_atnam_ref4
        ASSIGNING FIELD-SYMBOL(<lwa_atnam_ref4>).
      IF sy-subrc EQ 0.
        <lwa_atnam_ref4> = 'IEQ'.
        CASE lv_index.
          WHEN '1'.
            <lwa_atnam_ref4>-low = 'CIT-SAFRA'.
          WHEN '2'.
            <lwa_atnam_ref4>-low = 'CIT-FLORADA'.
        ENDCASE.
      ENDIF.
    ENDDO.

    SELECT atinn, adzhl, atnam,
           atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @DATA(lt_cabn_ref4)
     WHERE atnam IN @lr_atnam_ref4.

    IF sy-subrc EQ 0.
      SORT lt_cabn_ref4 BY atnam atinn.
    ENDIF.

    lr_atinn_ref4 = CORRESPONDING #( lt_cabn_ref4 MAPPING low = atinn ).
    lwa_atinn = 'IEQ'.
    MODIFY lr_atinn_ref4 FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

    SELECT *
      FROM /agri/glflatv
      INTO TABLE @DATA(lt_flatv_ref4)
      FOR ALL ENTRIES IN @lt_glflot_ref4
     WHERE tplnr_fl EQ @lt_glflot_ref4-tplnr_fl
       AND atinn IN @lr_atinn_ref4[].
  ENDIF.

  REFRESH lr_atnam.
  DO 3 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_atnam ASSIGNING <lwa_atnam>.
    IF sy-subrc EQ 0.
      <lwa_atnam> = 'IEQ'.
      CASE lv_index.
        WHEN '1'.
          <lwa_atnam>-low = 'CIT-RAINFOREST'.
        WHEN '2'.
          <lwa_atnam>-low = 'CIT-IMOVEL-ARREND'.
        WHEN '3'.
          <lwa_atnam>-low = 'CIT-SETOR-COLHEITA'.
      ENDCASE.
    ENDIF.
  ENDDO.

  REFRESH r_2_next_weeks.
  CLEAR lv_sem_proj.
  DO 2 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE r_2_next_weeks
      ASSIGNING <lwa_next_week>.
    IF sy-subrc EQ 0.
      IF lv_sem_proj IS INITIAL.
        lv_sem_proj = p_sem.
      ENDIF.
      PERFORM check_next_week_numc USING lv_sem_proj
                                CHANGING lv_sem_proj.
      <lwa_next_week> = 'IEQ'.
      <lwa_next_week>-low = lv_sem_proj.
    ENDIF.
  ENDDO.

  DO 4 TIMES.
    lv_index = sy-index.
    INSERT INITIAL LINE INTO TABLE lr_faixa_safra
      ASSIGNING FIELD-SYMBOL(<lwa_faixa_safra>).
    IF sy-subrc EQ 0.
      <lwa_faixa_safra> = 'IEQ'.
      CASE lv_index.
        WHEN 1.
          <lwa_faixa_safra>-low = 'FAIXA_ACIDEZ'.
        WHEN 2.
          <lwa_faixa_safra>-low = 'FAIXA_BRIX_2'.
        WHEN 3.
          <lwa_faixa_safra>-low = 'FAIXA_RATIO_2'.
        WHEN 4.
          <lwa_faixa_safra>-low = 'FAIXA_RATIO_LOW'.
      ENDCASE.
    ENDIF.
  ENDDO.

  IF t_resultado[] IS NOT INITIAL.
    SELECT *
      FROM yoprmsafra
      INTO TABLE @DATA(lt_safra)
      FOR ALL ENTRIES IN @t_resultado
     WHERE safra EQ @t_resultado-ano_safra
       AND parametro IN @lr_faixa_safra[].

    SORT lt_safra BY safra parametro semana DESCENDING.

    SELECT *
      FROM zfmfp_variedades
      INTO TABLE @DATA(lt_variedades).

    SORT lt_variedades BY grupo_variedade.
  ENDIF.

  IF t_resultado[] IS NOT INITIAL.
    SELECT tplnr_fl, pltxt, tplma, iwerk, ptrno,
           strno, beber1, setor_orig, stort1
      FROM /agri/glflot
      APPENDING TABLE @t_glflot
      FOR ALL ENTRIES IN @t_resultado
     WHERE tplnr_fl EQ @t_resultado-cod_faz_input.

    IF sy-subrc EQ 0.
      SORT t_glflot BY tplnr_fl.
      DELETE ADJACENT DUPLICATES FROM t_glflot COMPARING tplnr_fl.

      SELECT ptrno, posnr, parvw, lifnr
        FROM /agri/glflptr
        INTO TABLE @DATA(lt_glflptr)
        FOR ALL ENTRIES IN @t_glflot
       WHERE ptrno = @t_glflot-ptrno
         AND parvw = @lv_fornecedor.

      SORT lt_glflptr BY ptrno.
      DELETE ADJACENT DUPLICATES FROM lt_glflptr COMPARING ptrno.
    ENDIF.
  ENDIF.

  IF t_resultado[] IS NOT INITIAL.
    SELECT *
      FROM ys4selpomares
      INTO TABLE @DATA(lt_pomares_check)
      FOR ALL ENTRIES IN @t_resultado
     WHERE cod_var  EQ @t_resultado-matnr
       AND cod_imov EQ @t_resultado-cod_imov
       AND talhao   EQ @t_resultado-talhao
       AND sem_proj IN @r_2_next_weeks
       AND florada  EQ @t_resultado-florada
       AND safra    EQ @t_resultado-ano_safra.

    IF sy-subrc EQ 0.
      SORT lt_pomares_check BY cod_var cod_imov
        talhao sem_proj florada safra.
    ENDIF.
  ENDIF.

  SELECT atinn, adzhl, atnam,
         atfor, anzst, anzdz
    FROM cabn
    INTO TABLE @DATA(t_cabn_agri)
   WHERE atnam IN @lr_atnam.

  IF sy-subrc EQ 0.
    SORT t_cabn_agri BY atnam atinn.

    REFRESH lr_atinn.
    lr_atinn = CORRESPONDING #( t_cabn_agri MAPPING low = atinn ).
    lwa_atinn = 'IEQ'.
    MODIFY lr_atinn FROM lwa_atinn TRANSPORTING sign option WHERE low <> ''.

    SELECT tplnr_fl, pltxt, tplma, iwerk, ptrno,
           strno, beber1, setor_orig, stort1
      FROM /agri/glflot
      INTO TABLE @t_glflot
     WHERE tplma IN @s_tplma[].

    SELECT tplnr_fl, pltxt, tplma, iwerk, ptrno,
           strno, beber1, setor_orig, stort1
      FROM /agri/glflot
      APPENDING TABLE @t_glflot
     WHERE tplnr_fl IN @s_tplma[].

    IF t_glflot[] IS NOT INITIAL.
      SELECT *
        FROM /agri/glflatv
        INTO TABLE @DATA(t_flatv)
        FOR ALL ENTRIES IN @t_glflot
       WHERE tplnr_fl EQ @t_glflot-tplnr_fl
         AND atinn IN @lr_atinn[].

      APPEND LINES OF lt_glflot_ref4 TO t_glflot.

      SELECT ptrno, posnr, parvw, lifnr
        FROM /agri/glflptr
        APPENDING TABLE @lt_glflptr
        FOR ALL ENTRIES IN @t_glflot
       WHERE ptrno = @t_glflot-ptrno
         AND parvw = @lv_fornecedor.

      SELECT tplnr_fl, contr, cmnum, varia,
             season, datab, datbi, ymatnr,
             zzfazplantio, zzfazvartecnia,
             zzreplant
        FROM /agri/glflcma
        INTO TABLE @DATA(lt_glflcma)
        FOR ALL ENTRIES IN @t_glflot
       WHERE tplnr_fl = @t_glflot-tplnr_fl
        AND  zzprevisto = ''
        AND  astat = 'A'.

      SORT: t_glflot BY tplnr_fl,
            lt_glflptr BY ptrno,
            lt_glflcma BY tplnr_fl contr DESCENDING.
    ENDIF.

    SELECT tplnr_fl, ownshp
      INTO TABLE @lt_terreno
      FROM /agri/glflot
      WHERE tplnr_fl IN @s_tplma[].

    SORT lt_terreno BY tplnr_fl.

    SORT lt_glmdhdr_join BY tplnr_fl mpgrp atnam atflv.
    DATA(lv_resultado) = lines( t_resultado ).
    LOOP AT t_resultado ASSIGNING <lwa_resultado>.
      lv_tabix = sy-tabix.

      IF <lwa_resultado>-nova_linha EQ abap_true.
        EXIT.
      ELSE.
        IF <lwa_resultado>-cod_faz_output IS INITIAL.
          <lwa_resultado>-cod_faz_output = <lwa_resultado>-cod_imovel_c.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_resultado>-cod_faz_output
            IMPORTING
              output     = <lwa_resultado>-cod_faz_input
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.
      ENDIF.

      READ TABLE t_cabn_agri INTO DATA(lwa_colheita)
        WITH KEY atnam = 'CIT-SETOR-COLHEITA ' BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR lwa_colheita.
      ELSE.
        READ TABLE t_flatv INTO DATA(lwa_flatv)
          WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl
                   atinn = lwa_colheita-atinn.
        IF sy-subrc EQ 0.
          CONDENSE lwa_flatv-atwrt.
          <lwa_resultado>-setor = lwa_flatv-atwrt(9).
        ENDIF.
      ENDIF.

      <lwa_resultado>-cod_faz_input = <lwa_resultado>-cod_imov.
      <lwa_resultado>-cod_faz_output = <lwa_resultado>-cod_imov.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = <lwa_resultado>-cod_faz_input
        IMPORTING
          output     = <lwa_resultado>-cod_faz_input
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      lv_imovel = <lwa_resultado>-cod_imov.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = lv_imovel
        IMPORTING
          output     = lv_imovel
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      IF sy-subrc EQ 0.
        READ TABLE t_cabn_agri INTO DATA(lwa_rainforest)
          WITH KEY atnam = 'CIT-RAINFOREST' BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR lwa_rainforest.
        ELSE.
          READ TABLE t_flatv INTO lwa_flatv
            WITH KEY tplnr_fl = lv_imovel
                     atinn = lwa_rainforest-atinn.
          IF sy-subrc EQ 0.
            CONDENSE lwa_flatv-atwrt.
            <lwa_resultado>-rainforest = lwa_flatv-atwrt(3).
          ENDIF.
        ENDIF.
      ENDIF.

      lv_ano_safra_f = <lwa_resultado>-ano_safra.
      READ TABLE lt_glmdhdr_join INTO DATA(lwa_glmdhdr_join)
        WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl
                 mpgrp = 'ENCERRAR_COLHEITA'
                 atnam = 'CIT-SAFRA'
                 atflv = lv_ano_safra_f BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE t_resultado INDEX lv_tabix.
        CONTINUE.
      ELSE.
        IF <lwa_resultado>-florada IS NOT INITIAL.
          READ TABLE lt_glmdhdr_join INTO DATA(lwa_cit_safra)
            WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl
                     mpgrp = 'ENCERRAR_FLORADA'
                     atnam = 'CIT-SAFRA'
                     atflv = lv_ano_safra_f BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE lt_glmdhdr_join INTO DATA(lwa_cit_florada)
              WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl
                       mpgrp = 'ENCERRAR_FLORADA'
                       atnam = 'CIT-FLORADA'
                       atflv = lv_ano_safra_f BINARY SEARCH.
            IF sy-subrc EQ 0.
              lv_cit_florada = lwa_cit_florada-atflv.
              CONDENSE lv_cit_florada.
              lv_florada = lv_cit_florada.
              IF lv_florada EQ <lwa_resultado>-florada.
                DELETE t_resultado INDEX lv_tabix.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE t_glflot INTO DATA(lwa_glflot)
        WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_resultado>-setor_orig = lwa_glflot-setor_orig.
        <lwa_resultado>-comprador = lwa_glflot-stort1.
        <lwa_resultado>-iwerk = lwa_glflot-iwerk.

        READ TABLE lt_glflptr INTO DATA(lwa_glflptr)
          WITH KEY ptrno = lwa_glflot-ptrno BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lwa_resultado>-lifnr = lwa_glflptr-lifnr.
        ENDIF.

        READ TABLE lt_glflcma INTO DATA(lwa_glflcma)
          WITH KEY tplnr_fl = <lwa_resultado>-tplnr_fl BINARY SEARCH.
        IF sy-subrc EQ 0.
          DATA(lv_pdate) = lwa_glflcma-zzfazplantio.
          DATA(lv_datbi) = lwa_glflcma-datbi.

          CLEAR: lv_days, lv_months, lv_years, lv_days.
          IF lv_pdate IS NOT INITIAL.
            IF lv_datbi LE sy-datum.
*--FM to get number of days between end date and Planting date
              CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
                EXPORTING
                  date1                       = lv_datbi
                  date2                       = lv_pdate
                  output_format               = zcl_abs_abap_maintain=>c_output_days_format
                IMPORTING
                  days                        = lv_days
                EXCEPTIONS
                  overflow_long_years_between = 1
                  invalid_dates_specified     = 2
                  OTHERS                      = 3.
            ELSEIF lv_datbi GE sy-datum.
*--FM to get number of days between current date and Planting date
              CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
                EXPORTING
                  date1                       = sy-datum
                  date2                       = lv_pdate
                  output_format               = zcl_abs_abap_maintain=>c_output_days_format
                IMPORTING
                  days                        = lv_days
                EXCEPTIONS
                  overflow_long_years_between = 1
                  invalid_dates_specified     = 2
                  OTHERS                      = 3.
            ENDIF.

            lv_months = lv_days / 30.
            IF lv_months IS NOT INITIAL.
*--FM to roundoff the month to nearest value
              CALL FUNCTION 'J_1I6_ROUND_TO_NEAREST_AMT'
                EXPORTING
                  i_amount = lv_months
                IMPORTING
                  e_amount = lv_months.
              IF sy-subrc <> 0.
                CLEAR lv_months.
              ENDIF.

              lv_years = lv_months / 12.
              CALL FUNCTION 'ROUND'
                EXPORTING
                  decimals      = 0
                  input         = lv_years
                  sign          = '-'
                IMPORTING
                  output        = lv_years
                EXCEPTIONS
                  input_invalid = 1
                  overflow      = 2
                  type_invalid  = 3
                  OTHERS        = 4.
            ENDIF.
            <lwa_resultado>-zzcsage_month = lv_months.
            <lwa_resultado>-zzcsage = lv_days.
            <lwa_resultado>-zzplantyear = lwa_glflcma-zzfazplantio+0(4).
            <lwa_resultado>-zzfazvartecnia = lwa_glflcma-zzfazvartecnia.
            IF lv_years LE 99.
              <lwa_resultado>-idade_talhao = lv_years.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE t_cabn_agri INTO DATA(lwa_imovel_arrend)
        WITH KEY atnam = 'CIT-IMOVEL-ARREND' BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE t_flatv INTO lwa_flatv
          WITH KEY tplnr_fl = <lwa_resultado>-cod_faz_input
                   atinn = lwa_imovel_arrend-atinn.
        IF sy-subrc EQ 0.
          IF lwa_flatv-atwrt IS NOT INITIAL.
            lv_novo_imovel = lwa_flatv-atwrt.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = lv_novo_imovel
              IMPORTING
                output     = lv_novo_imovel
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            DATA(lv_terceiro) = abap_false.
            READ TABLE lt_terreno INTO DATA(lwa_terreno)
              WITH KEY tplnr_fl = lv_novo_imovel.
            IF sy-subrc NE 0.
              CLEAR lwa_terreno.
              SELECT tplnr_fl ownshp
                FROM /agri/glflot
                APPENDING TABLE lt_terreno
               WHERE tplnr_fl = lv_novo_imovel.
              IF sy-subrc EQ 0.
                READ TABLE lt_terreno INTO lwa_terreno
                  WITH KEY tplnr_fl = lv_novo_imovel.
              ENDIF.
            ENDIF.
            IF lwa_terreno-ownshp EQ c_ownership-third_party.
              lv_terceiro = abap_true.
            ENDIF.

            IF lv_terceiro EQ abap_true.
              INSERT INITIAL LINE INTO TABLE t_resultado
                ASSIGNING FIELD-SYMBOL(<lwa_new_line>).
              IF sy-subrc EQ 0.
                <lwa_resultado>-nova_linha = abap_true.
                <lwa_new_line> = <lwa_resultado>.
                CONDENSE lwa_flatv-atwrt.
                INSERT INITIAL LINE INTO TABLE t_arrendamento
                  ASSIGNING FIELD-SYMBOL(<lwa_arrendamento>).
                IF sy-subrc EQ 0.
                  DATA(lv_insert) = abap_true.
                  <lwa_arrendamento>-cod_imov_from = <lwa_resultado>-cod_imov.
                  <lwa_arrendamento>-source_in = <lwa_resultado>-cod_faz_input.
                ELSE.
                  lv_insert = abap_false.
                ENDIF.

                READ TABLE t_update INTO DATA(ls_update)
                  WITH KEY cod_imov   = <lwa_resultado>-cod_imov
                           talhao     = <lwa_resultado>-talhao
                           sem_proj   = <lwa_resultado>-semana_prog
                           florada    = <lwa_resultado>-florada
                           ano_safra  = <lwa_resultado>-ano_safra
                           referencia = <lwa_resultado>-referencia
                           variedade  = <lwa_resultado>-matnr
                           fornecedor = <lwa_resultado>-lifnr.
                IF sy-subrc EQ 0.
                  INSERT INITIAL LINE INTO TABLE t_update
                    ASSIGNING FIELD-SYMBOL(<lwa_new_update>).
                  IF sy-subrc EQ 0.
                    <lwa_new_update> = ls_update.
                    <lwa_new_update>-cod_imov = lwa_flatv-atwrt(6).
                  ENDIF.
                ENDIF.

                <lwa_new_line>-cod_imovel_c = lwa_flatv-atwrt(6).
                <lwa_new_line>-cod_faz_output = lwa_flatv-atwrt(6).
                <lwa_new_line>-cod_imov = lwa_flatv-atwrt(6).

                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
                  EXPORTING
                    input      = <lwa_new_line>-cod_faz_output
                  IMPORTING
                    output     = <lwa_new_line>-cod_faz_input
                  EXCEPTIONS
                    not_found  = 1
                    not_active = 2
                    OTHERS     = 3.

                IF lv_insert EQ abap_true.
                  <lwa_arrendamento>-cod_imov_to = <lwa_new_line>-cod_imov.
                  <lwa_arrendamento>-target_in = <lwa_new_line>-cod_faz_input.
                ENDIF.

                IF <lwa_new_line>-cod_imov IS NOT INITIAL.
                  CONCATENATE <lwa_new_line>-cod_imov '-' <lwa_new_line>-talhao
                            INTO <lwa_new_line>-tplnr_fl.
                  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
                    EXPORTING
                      input      = <lwa_new_line>-tplnr_fl
                    IMPORTING
                      output     = <lwa_new_line>-tplnr_fl
                    EXCEPTIONS
                      not_found  = 1
                      not_active = 2
                      OTHERS     = 3.
                  IF sy-subrc NE 0.
                    <lwa_new_line>-cod_invalido = abap_true.
                  ENDIF.
                ELSE.
                  <lwa_new_line>-cod_invalido = abap_true.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR lwa_imovel_arrend.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT t_arrendamento BY target_in.
  DELETE ADJACENT DUPLICATES FROM t_arrendamento COMPARING ALL FIELDS.

  IF t_resultado[] IS NOT INITIAL.
    SELECT tplnr_fl, contr, cmnum, varia,
           season, datab, datbi, ymatnr,
           zzfazplantio, zzfazvartecnia,
           zzreplant
      FROM /agri/glflcma
      APPENDING TABLE @lt_glflcma
      FOR ALL ENTRIES IN @t_resultado
     WHERE tplnr_fl = @t_resultado-tplnr_fl
      AND  zzprevisto = ''
      AND  astat = 'A'.

    SORT lt_glflcma BY tplnr_fl contr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_glflcma
      COMPARING tplnr_fl contr.
  ENDIF.

  LOOP AT t_resultado INTO DATA(lwa_resultado).
    lv_sem_proj = lwa_resultado-semana_prog.
    PERFORM check_next_week_numc USING lv_sem_proj
                              CHANGING lv_sem_proj.

    DATA(lv_imovel_input) = lwa_resultado-cod_faz_input.
    DATA(lv_imovel_out) = lwa_resultado-cod_imov.
    READ TABLE t_arrendamento INTO DATA(ls_arrendamento)
      WITH KEY target_in = lwa_resultado-cod_faz_input BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_imovel_input = ls_arrendamento-source_in.
      lv_imovel_out = ls_arrendamento-cod_imov_from.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE t_update
      ASSIGNING FIELD-SYMBOL(<lwa_update>).
    IF sy-subrc EQ 0.
      READ TABLE t_glflot INTO DATA(lwa_glflot_fazenda)
        WITH KEY tplnr_fl = lv_imovel_input BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_glflptr INTO DATA(lwa_glflptr_fazenda)
          WITH KEY ptrno = lwa_glflot_fazenda-ptrno BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR lwa_glflptr_fazenda.
        ENDIF.
      ELSE.
        CLEAR lwa_glflot_fazenda.
      ENDIF.

      <lwa_update>-cod_imov = lwa_resultado-cod_imov.
      <lwa_update>-talhao = lwa_resultado-talhao.
      <lwa_update>-sem_proj = lwa_resultado-semana_prog.
      <lwa_update>-florada = lwa_resultado-florada.
      <lwa_update>-ano_safra = lwa_resultado-ano_safra.
      <lwa_update>-referencia = lwa_resultado-referencia.

*      <lwa_update>-variedade = lwa_resultado-matnr.
      READ TABLE lt_glflcma INTO lwa_glflcma
        WITH KEY tplnr_fl = lwa_resultado-tplnr_fl BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_update>-variedade = lwa_glflcma-ymatnr.
      ENDIF.

      <lwa_update>-fornecedor = lwa_glflptr_fazenda-lifnr.
      <lwa_update>-ano_plantio = lwa_resultado-zzplantyear.
      <lwa_update>-brix_proj = lwa_resultado-media_brix.
      <lwa_update>-ratio_proj = lwa_resultado-media_ratio.
      <lwa_update>-kgss_proj = lwa_resultado-media_solidos.
      <lwa_update>-idade_talhao = lwa_resultado-idade_talhao.
      <lwa_update>-localizacao = lwa_glflot_fazenda-stort1.
      <lwa_update>-area_operac = lwa_glflot_fazenda-beber1.
      <lwa_update>-rainforest = lwa_resultado-rainforest.
      <lwa_update>-setor = lwa_resultado-setor.
      <lwa_update>-rainforest = lwa_resultado-rainforest.
      <lwa_update>-taxa_kgss = lwa_resultado-taxa_kgss.

      READ TABLE lt_pomares_check INTO DATA(lwa_pomar)
        WITH KEY cod_var       = lwa_resultado-matnr
                 cod_imov      = lv_imovel_out
                 talhao        = lwa_resultado-talhao
                 sem_proj      = lv_sem_proj
                 florada       = lwa_resultado-florada
                 safra         = lwa_resultado-ano_safra BINARY SEARCH.

      IF sy-subrc EQ 0.
        IF lwa_pomar-kgss_proj IS INITIAL.
          CLEAR lwa_resultado-taxa_kgss.
        ELSE.
          <lwa_update>-taxa_kgss = lwa_pomar-kgss_proj - lwa_resultado-media_solidos.
        ENDIF.
      ELSE.
        CLEAR lwa_pomar.
      ENDIF.

      READ TABLE lt_safra INTO DATA(lwa_faixa_brix)
        WITH KEY safra  = <lwa_update>-ano_safra
                 parametro = lc_faixa-brix BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF lwa_faixa_brix-paramde LE <lwa_update>-brix_proj
        AND lwa_faixa_brix-paramate GE <lwa_update>-brix_proj.
          <lwa_update>-faixa_brix = '2'.
        ELSEIF <lwa_update>-brix_proj LT lwa_faixa_brix-paramde.
          <lwa_update>-faixa_brix = '3'.
        ELSEIF <lwa_update>-brix_proj GT lwa_faixa_brix-paramate.
          <lwa_update>-faixa_brix = '1'.
        ENDIF.
      ENDIF.

      READ TABLE lt_safra INTO DATA(lwa_faixa_ratio)
        WITH KEY safra  = <lwa_update>-ano_safra
                 parametro = lc_faixa-ratio BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF lwa_faixa_ratio-paramde LE <lwa_update>-ratio_proj
        AND lwa_faixa_ratio-paramate GE <lwa_update>-ratio_proj.
          <lwa_update>-faixa_ratio = '2'.
        ELSEIF <lwa_update>-ratio_proj LT lwa_faixa_ratio-paramde.
          <lwa_update>-faixa_ratio = '3'.
        ELSEIF <lwa_update>-ratio_proj GT lwa_faixa_ratio-paramate.
          <lwa_update>-faixa_ratio = '1'.
        ENDIF.
      ENDIF.

      IF <lwa_update>-brix_proj IS NOT INITIAL
      AND <lwa_update>-ratio_proj IS NOT INITIAL.
        DATA(lv_acidez) = <lwa_update>-brix_proj / <lwa_update>-ratio_proj.
        <lwa_update>-acidez = <lwa_update>-brix_proj / <lwa_update>-ratio_proj.
        READ TABLE lt_safra INTO DATA(lwa_faixa_acidez)
          WITH KEY safra  = <lwa_update>-ano_safra
                   parametro = lc_faixa-acidez BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF lwa_faixa_acidez-paramde LE lv_acidez
          AND lwa_faixa_acidez-paramate GE lv_acidez.
            <lwa_update>-faixa_acidez = '2'.
          ELSEIF lv_acidez LT lwa_faixa_acidez-paramde.
            <lwa_update>-faixa_acidez = '3'.
          ELSEIF lv_acidez GT lwa_faixa_acidez-paramate.
            <lwa_update>-faixa_acidez = '1'.
          ENDIF.
        ENDIF.
      ENDIF.

      DATA(lv_variedade) = <lwa_update>-variedade.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = lv_variedade
        IMPORTING
          output = lv_variedade.
      CASE lv_variedade.
        WHEN '8'
          OR '9'
          OR '10'.
          READ TABLE lt_variedades INTO DATA(lwa_variedade)
            WITH KEY grupo_variedade = c_variedade-precoce BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE lwa_variedade-prioridade 'H' INTO <lwa_update>-classif_classe.
          ENDIF.
        WHEN '1'.
          READ TABLE lt_variedades INTO lwa_variedade
            WITH KEY grupo_variedade = c_variedade-pera BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE lwa_variedade-prioridade 'P' INTO <lwa_update>-classif_classe.
          ENDIF.
        WHEN '2'.
          READ TABLE lt_variedades INTO lwa_variedade
            WITH KEY grupo_variedade = c_variedade-natal BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE lwa_variedade-prioridade 'N' INTO <lwa_update>-classif_classe.
          ENDIF.
        WHEN '3'
          OR '4'.
          READ TABLE lt_variedades INTO lwa_variedade
            WITH KEY grupo_variedade = c_variedade-valencia BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE lwa_variedade-prioridade 'V' INTO <lwa_update>-classif_classe.
          ENDIF.
        WHEN OTHERS. "5,6,7, >=11
          READ TABLE lt_variedades INTO lwa_variedade
            WITH KEY grupo_variedade = c_variedade-outros BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE lwa_variedade-prioridade 'O' INTO <lwa_update>-classif_classe.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  DATA(t_update_new) = t_update[].

  LOOP AT r_2_next_weeks INTO DATA(ls_week).
    CASE sy-tabix.
      WHEN 1.
        DATA(lv_semana_2) = ls_week-low.
      WHEN 2.
        DATA(lv_semana_3) = ls_week-low.
    ENDCASE.
  ENDLOOP.

  SORT t_arrendamento BY cod_imov_to.

  PERFORM check_sort_field USING p_cl5
                        CHANGING lv_c15.

  PERFORM check_sort_field USING p_cl6
                        CHANGING lv_c16.

*--BOC-T_T.KONNO-05.25.21
*  IF lv_c15 = 'KGSS_PROJ'
*  OR lv_c15 = 'RATIO_PROJ'
*  OR lv_c15 = 'BRIX_PROJ'.
*    IF lv_c16 = 'KGSS_PROJ'
*    OR lv_c16 = 'RATIO_PROJ'
*    OR lv_c16 = 'BRIX_PROJ'.
*      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
*        classif_classe (lv_c15) DESCENDING (lv_c16) DESCENDING.
*    ELSE.
*      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
*        classif_classe (lv_c15) DESCENDING (lv_c16).
*    ENDIF.
*  ELSEIF lv_c15 = 'TAXA_KGSS'
*      OR lv_c15 = 'TAXA_VARIACAO'.
*    IF lv_c16 = 'TAXA_KGSS'
*    OR lv_c16 = 'TAXA_VARIACAO'.
*      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
*        classif_classe (lv_c15) ASCENDING (lv_c16) ASCENDING.
*    ELSE.
*      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
*        classif_classe (lv_c15) ASCENDING (lv_c16) DESCENDING.
*    ENDIF.
*  ENDIF.
*--EOC-T_T.KONNO-05.25.21

  LOOP AT t_update ASSIGNING <lwa_update>.
    CLEAR lwa_controle.
    READ TABLE lt_controle ASSIGNING FIELD-SYMBOL(<lwa_controle>)
      WITH KEY cod_imov    = <lwa_update>-cod_imov
               setor       = <lwa_update>-setor
               faixa_ratio = <lwa_update>-faixa_ratio BINARY SEARCH.
    IF sy-subrc NE 0.
      lwa_controle-cod_imov = <lwa_update>-cod_imov.
      lwa_controle-setor = <lwa_update>-setor.
      lwa_controle-faixa_ratio = <lwa_update>-faixa_ratio.
      lwa_controle-sequenciador = 1.
      INSERT lwa_controle INTO TABLE lt_controle.
    ELSE.
      ADD 1 TO <lwa_controle>-sequenciador.
      lwa_controle = <lwa_controle>.
    ENDIF.

    <lwa_update>-sequenciador = lwa_controle-sequenciador.
  ENDLOOP.

  IF t_update[] IS NOT INITIAL.
    SELECT *
      FROM zabst_solidosha
      INTO CORRESPONDING FIELDS OF TABLE @lt_solidosha
      FOR ALL ENTRIES IN @t_update
     WHERE safra    = @t_update-ano_safra
       AND semana   = @t_update-sem_proj
       AND cod_imov = @t_update-cod_imov.

    IF sy-subrc EQ 0.
      LOOP AT lt_solidosha ASSIGNING FIELD-SYMBOL(<lwa_solido>).
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = <lwa_solido>-tplnr_fl
          IMPORTING
            output = <lwa_solido>-tplnr_fl_out.

        <lwa_solido>-talhao = <lwa_solido>-tplnr_fl_out+7(5).
      ENDLOOP.

      SORT lt_solidosha BY safra cod_imov talhao semana.
    ENDIF.
  ENDIF.

  SORT t_arrendamento BY cod_imov_from cod_imov_to.
  DELETE ADJACENT DUPLICATES FROM t_arrendamento COMPARING cod_imov_from cod_imov_to.
  REFRESH lt_solidos_aux.

  LOOP AT lt_solidosha INTO DATA(ls_solidosha).
    READ TABLE t_arrendamento INTO ls_arrendamento
      WITH KEY cod_imov_from = ls_solidosha-cod_imov BINARY SEARCH.
    WHILE sy-subrc EQ 0.
      lv_tabix = sy-tabix + 1.
      INSERT INITIAL LINE INTO TABLE lt_solidos_aux
        ASSIGNING FIELD-SYMBOL(<ls_solidosha_new>).
      IF sy-subrc EQ 0.
        <ls_solidosha_new> = ls_solidosha.
        <ls_solidosha_new>-cod_imov = ls_arrendamento-cod_imov_to.
        <ls_solidosha_new>-tplnr_fl(6) = ls_arrendamento-cod_imov_to(6).
        <ls_solidosha_new>-tplnr_fl_out(6) = ls_arrendamento-cod_imov_to(6).
      ENDIF.
      READ TABLE t_arrendamento INTO ls_arrendamento
        INDEX lv_tabix COMPARING cod_imov_from.
    ENDWHILE.
  ENDLOOP.

  APPEND LINES OF lt_solidos_aux TO lt_solidosha.
  SORT lt_solidosha BY safra cod_imov talhao semana.

  LOOP AT t_update ASSIGNING <lwa_update>.
    READ TABLE lt_solidosha ASSIGNING <lwa_solido>
      WITH KEY safra    = <lwa_update>-ano_safra
               cod_imov = <lwa_update>-cod_imov
               talhao   = <lwa_update>-talhao
               semana   = <lwa_update>-sem_proj BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_update>-taxa_variacao = <lwa_solido>-taxa_variacao.
    ENDIF.
  ENDLOOP.

  SORT t_update BY cod_imov talhao sem_proj referencia.
  DELETE ADJACENT DUPLICATES FROM t_update COMPARING cod_imov talhao sem_proj.

*--BOC-T_T.KONNO-05.25.21
  IF lv_c15 = 'KGSS_PROJ'
  OR lv_c15 = 'RATIO_PROJ'
  OR lv_c15 = 'BRIX_PROJ'.
    IF lv_c16 = 'KGSS_PROJ'
    OR lv_c16 = 'RATIO_PROJ'
    OR lv_c16 = 'BRIX_PROJ'.
      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
        classif_classe (lv_c15) DESCENDING (lv_c16) DESCENDING.
    ELSE.
      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
        classif_classe (lv_c15) DESCENDING (lv_c16).
    ENDIF.
  ELSEIF lv_c15 = 'TAXA_KGSS'
      OR lv_c15 = 'TAXA_VARIACAO'.
    IF lv_c16 = 'TAXA_KGSS'
    OR lv_c16 = 'TAXA_VARIACAO'.
      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
        classif_classe (lv_c15) ASCENDING (lv_c16) ASCENDING.
    ELSE.
      SORT t_update BY sem_proj cod_imov setor referencia faixa_ratio
        classif_classe (lv_c15) ASCENDING (lv_c16) DESCENDING.
    ENDIF.
  ENDIF.
*--EOC-T_T.KONNO-05.25.21



  DATA(lv_seq_geral) = 0.
  LOOP AT t_update ASSIGNING <lwa_update>.
    lv_seq_geral = lv_seq_geral + 1.
    <lwa_update>-seq_geral = lv_seq_geral.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_NEXT_WEEK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_COTASEG_SEMANA
*&      <-- <LWA_RESULTADO>_SEMANA_PROG
*&---------------------------------------------------------------------*
FORM check_next_week USING uv_ref_week TYPE yosemana_cota
                  CHANGING cv_next_week.

  DATA: lv_next_week_year TYPE tstr_year,
        lv_next_week      TYPE numc2.

  lv_next_week_year = uv_ref_week(4).
  DATA(lv_tabix) = uv_ref_week+4(2).
  READ TABLE t_periodo INTO DATA(lwa_periodo)
    WITH KEY year = lv_next_week_year.
  IF sy-subrc EQ 0.
    IF lv_tabix LT lines( lwa_periodo-gensegtab ).
      lv_next_week = lv_tabix + 1.
    ELSE.
      ADD 1 TO lv_next_week_year.
      READ TABLE t_periodo INTO lwa_periodo
        WITH KEY year = lv_next_week_year.
      IF sy-subrc EQ 0.
        lv_next_week = 1.
      ENDIF.
    ENDIF.
  ENDIF.

  CONCATENATE lv_next_week_year lv_next_week INTO cv_next_week.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_NEXT_WEEK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_COTASEG_SEMANA
*&      <-- <LWA_RESULTADO>_SEMANA_PROG
*&---------------------------------------------------------------------*
FORM check_next_week_numc USING uv_ref_week TYPE yosemproj
                       CHANGING cv_next_week.

  DATA: lv_next_week_year TYPE tstr_year,
        lv_next_week      TYPE numc2.

  lv_next_week_year = uv_ref_week(4).
  DATA(lv_tabix) = uv_ref_week+4(2).
  READ TABLE t_periodo INTO DATA(lwa_periodo)
    WITH KEY year = lv_next_week_year.
  IF sy-subrc EQ 0.
    IF lv_tabix LT lines( lwa_periodo-gensegtab ).
      lv_next_week = lv_tabix + 1.
    ELSE.
      ADD 1 TO lv_next_week_year.
      READ TABLE t_periodo INTO lwa_periodo
        WITH KEY year = lv_next_week_year.
      IF sy-subrc EQ 0.
        lv_next_week = 1.
      ENDIF.
    ENDIF.
  ENDIF.

  CONCATENATE lv_next_week_year lv_next_week INTO cv_next_week.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_alv.

  PERFORM prepare_catalog.
  PERFORM prepare_layout.
  CALL SCREEN 100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_catalog.

  DATA: lt_fieldcat_slis TYPE slis_t_fieldcat_alv.

  REFRESH: t_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZFMFP_COLHEITA'
    CHANGING
      ct_fieldcat            = lt_fieldcat_slis
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fieldcat_slis
    IMPORTING
      et_fieldcat_lvc = t_fieldcat[]
    TABLES
      it_data         = t_update[]
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_layout.

  wa_layout-zebra = abap_true.
  wa_layout-smalltitle = abap_true.
  wa_layout-cwidth_opt = abap_true.
* allow to select multiple lines
  wa_layout-sel_mode = 'A'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      PERFORM clear.
      PERFORM exit_program.
    WHEN OTHERS.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  DATA: lt_f4      TYPE lvc_t_f4,
        lwa_f4     TYPE lvc_s_f4,
        lt_buttons TYPE ui_functions,
        lwa_button TYPE ui_func.

  SET TITLEBAR 'MAIN100'.
  SET PF-STATUS 'MAIN100'.
  IF g_custom_container IS INITIAL.

*... Toolbar Button CHECK
    lwa_button = cl_gui_alv_grid=>mc_fc_check.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button REFRESH
    lwa_button = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button UNDO
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button PRINT
    lwa_button = cl_gui_alv_grid=>mc_fc_print_back.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button Print Preview
    lwa_button = cl_gui_alv_grid=>mc_fc_print_prev.
    APPEND lwa_button TO lt_buttons.
*... Menu Button PASTE add Menu Item PASTE NEW ROW
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button INSERT ROW
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button DELETE ROW
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button COPY ROW
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button APPEND ROW
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button CUT
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button INFORMATION
    lwa_button = cl_gui_alv_grid=>mc_fc_info.
    APPEND lwa_button TO lt_buttons.
*--- Menu Button SUBTOTAL
    lwa_button = cl_gui_alv_grid=>mc_mb_subtot.
    APPEND lwa_button TO lt_buttons.
*... Menu Button SUM add Menu Item SUM
    lwa_button = cl_gui_alv_grid=>mc_fc_sum.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_mb_view.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_mb_sum.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND lwa_button TO lt_buttons.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = g_custom_container.
    wa_variant-report = sy-repid.
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant
        i_save                        = 'A'
        is_layout                     = wa_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZFMFP_COLHEITA'
      CHANGING
        it_outtab                     = t_update
        it_fieldcatalog               = t_fieldcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT gr_event_handler.
    SET HANDLER:
     gr_event_handler->handle_data_changed_finished FOR gr_grid,
     gr_event_handler->handle_user_command FOR gr_grid,
     gr_event_handler->handle_toolbar FOR gr_grid.

    CALL METHOD gr_grid->set_toolbar_interactive.

    CALL METHOD gr_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD gr_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD cl_gui_control=>set_focus EXPORTING control = gr_grid.

    SET HANDLER gr_event_handler->handle_on_f4 FOR gr_grid.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear .

  REFRESH: t_matdoc, t_glflot, t_mch1, t_cabn, t_ausp, t_colhendo,
           t_colhendo_tot, r_2_weeks, r_2_next_weeks,
           t_resultado, t_update, t_fieldcat,
           row_id.

  CLEAR: wa_colhendo_tot, wa_layout, ok_code, row_i,
         col_i, v_week_max, v_week,v_next_week.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit_program .

  SET SCREEN '0'.
  LEAVE SCREEN.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_SORT_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_CL5
*&---------------------------------------------------------------------*
FORM check_sort_field USING uv_parameter TYPE type_cl5
                  CHANGING cv_sort_field TYPE fieldname.

  CASE uv_parameter.
    WHEN 'OPT1'.
      cv_sort_field = 'KGSS_PROJ'.
    WHEN 'OPT2'.
      cv_sort_field = 'TAXA_KGSS'.
    WHEN 'OPT3'.
      cv_sort_field = 'RATIO_PROJ'.
    WHEN 'OPT4'.
      cv_sort_field = 'BRIX_PROJ'.
    WHEN 'OPT5'.
      cv_sort_field = 'TAXA_VARIACAO'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_table.

  DATA: lt_delete TYPE STANDARD TABLE OF zfmfp_colheita INITIAL SIZE 0.

  IF t_update[] IS NOT INITIAL.
    SELECT *
      FROM zfmfp_colheita
      INTO TABLE lt_delete
      FOR ALL ENTRIES IN t_update
     WHERE cod_imov = t_update-cod_imov
       AND sem_proj = t_update-sem_proj.

*-- Apaga os registros do código do imóvel e semana de projeção
    IF sy-subrc EQ 0.
      DELETE zfmfp_colheita FROM TABLE lt_delete.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

    MODIFY zfmfp_colheita FROM TABLE t_update.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
*-- Dados atualizados com sucesso!
    MESSAGE i048(zfmfp).
  ENDIF.

ENDFORM.
