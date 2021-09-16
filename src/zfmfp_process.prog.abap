*&---------------------------------------------------------------------*
*& Report  /AGRI/FMDFP_PROCESS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfmfp_process.

INCLUDE: /agri/global_constants,
         /agri/global_macros,
         /agri/gprolog_macros.

TABLES: /agri/fmfphdr,
        /agri/fmfpitm,
        /agri/fmfpcom,
        /agri/glflot,
        jcds.

TYPES: BEGIN OF type_out,
         icon         TYPE icon_d,
         aufnr        TYPE /agri/fmfphdr-aufnr,
         posnr        TYPE /agri/fmfpitm-posnr,
         aufnr_to     TYPE /agri/fmfpitm-aufnr_to,
         udate        TYPE jcds-udate,
         ltxa1        TYPE /agri/fmfpitm-ltxa1,
         tplnr_fl     TYPE /agri/fmfphdr-tplnr_fl,
         cmnum        TYPE /agri/fmfphdr-cmnum,
         varia        TYPE /agri/fmfphdr-varia,
         cpros        TYPE /agri/fmfphdr-cpros,
         iwerk        TYPE /agri/fmfphdr-iwerk,
         matnr        TYPE /agri/fmfpcom-matnr,
         maktx        TYPE makt-maktx,
         gamng        TYPE /agri/fmfpitm-gamng,
         tomng        TYPE /agri/fmfpitm-tomng,
         meinh        TYPE /agri/fmfpitm-meinh,
         erfmg        TYPE /agri/fmfpcom-erfmg,
         comng        TYPE /agri/s_fmfpcom-comng,
         lmnga        TYPE /agri/s_fmfpcom-lmnga,
         erfme        TYPE /agri/fmfpcom-erfme,
         dose_prog    TYPE /agri/fmfpitm-gamng,
         dose_real    TYPE /agri/fmfpitm-gamng,
         desvio       TYPE kbetr,
         zzreason     TYPE /agri/fmfpitm-zzreason,
         zzreason_txt TYPE val_text,
         zznote       TYPE /agri/fmfpitm-zznote.
TYPES: t_color TYPE lvc_t_scol,
       END OF type_out,

       BEGIN OF type_matdoc,
         budat TYPE budat,
         cpudt TYPE cpudt,
         cputm TYPE cputm,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         zeile TYPE mblpo,
         aufnr TYPE aufnr,
         anln1 TYPE anln1,
         anln2 TYPE anln2,
         bwart TYPE bwart,
       END OF type_matdoc,

       BEGIN OF type_glflot,
         tplnr_fl TYPE /agri/gltplnr_fl,
         pltxt    TYPE /agri/glpltxt,
         strno    TYPE /agri/glstrno,
       END OF type_glflot,

       BEGIN OF type_fmocopr,
         ocnum TYPE /agri/fmocnum,
         rueck TYPE co_rueck,
         rmzhl TYPE co_rmzhl,
         aufnr TYPE aufnr,
         posnr TYPE co_posnr,
         vornr TYPE vornr,
         ltxa1 TYPE ltxa1,
         lmnga TYPE /agri/fmlmnga,
         meinh TYPE vorme,
       END OF type_fmocopr,

       type_matdoc_tab  TYPE STANDARD TABLE OF type_matdoc INITIAL SIZE 0,
       type_glflot_tab  TYPE STANDARD TABLE OF type_glflot INITIAL SIZE 0,
       type_fmocopr_tab TYPE STANDARD TABLE OF type_fmocopr INITIAL SIZE 0,
       type_txw_note    TYPE TABLE OF txw_note.

*...Rendimento-Início
*--Table type declarations
TYPES: tty_to_monitor TYPE TABLE OF zabs_str_to_monitor_fcat.

*--Global table declarations
DATA: gt_to_monitor TYPE tty_to_monitor.

*--Constant declarations
CONSTANTS: c_domname_doc_cat TYPE dd07l-domname  VALUE '/AGRI/GL_AUTYP'.
*...Rendimento-Fim

DATA: gt_fmfphdr      TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gt_fmfphdr_to   TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gt_fmfpitm      TYPE STANDARD TABLE OF /agri/fmfpitm INITIAL SIZE 0,
      gt_fmfpitm_copy LIKE gt_fmfpitm,
      gt_fmfpcom      TYPE STANDARD TABLE OF /agri/fmfpcom INITIAL SIZE 0,
      gt_ocnum        TYPE STANDARD TABLE OF /agri/fmocindx INITIAL SIZE 0,
      gt_fmfpbch      TYPE STANDARD TABLE OF /agri/fmfpbch INITIAL SIZE 0,
      gt_afwi         TYPE STANDARD TABLE OF afwi INITIAL SIZE 0,
      gt_jest         TYPE STANDARD TABLE OF jest INITIAL SIZE 0,
      gt_jcds         TYPE STANDARD TABLE OF jcds INITIAL SIZE 0,
      gt_matdoc       TYPE type_matdoc_tab,
      gt_glflot       TYPE type_glflot_tab,
      gt_fmocopr      TYPE type_fmocopr_tab,
      gt_fmocopr_sum  LIKE gt_fmocopr,
      gt_out          TYPE STANDARD TABLE OF type_out INITIAL SIZE 0.

DATA: go_table      TYPE REF TO cl_salv_table,
      go_selections TYPE REF TO cl_salv_selections,
      go_events     TYPE REF TO cl_salv_events_table,
      gt_cell_type  TYPE salv_t_int4_column.

CONSTANTS: BEGIN OF c_move_type,
             goods_receipt TYPE bwart VALUE '101',
           END OF c_move_type.

CONSTANTS: BEGIN OF c_syst_stat,
             created  TYPE j_istat VALUE 'I0001',
             released TYPE j_istat VALUE 'I0002',
             tec_comp TYPE j_istat VALUE 'I0045',
           END OF c_syst_stat.

DEFINE key_text_get.
  CALL FUNCTION '/AGRI/G_KEYTEXT_GET'
    EXPORTING
      i_keytype             = 'KEY'
      i_tabname             = &1
      i_fieldname           = &2
      i_fieldvalue          = &3
      i_workarea            = &4
    IMPORTING
      e_text                = &5
    EXCEPTIONS
      ##fm_subrc_ok
      inadequate_parameters = 1
      text_not_found        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       Define eventos para relatório
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS. "lcl_handle_events DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    DATA: lgo_selections TYPE REF TO cl_salv_selections,
          lt_note        TYPE STANDARD TABLE OF txw_note INITIAL SIZE 0,
          lt_rows        TYPE salv_t_row,
          lt_aufnr       TYPE /agri/t_fmaufnr,
          lv_note        TYPE zabs_note,
          lv_reason      TYPE zabs_reason,
          lv_reason_txt  TYPE val_text,
          message        TYPE string.

    lgo_selections = go_table->get_selections( ).
    lt_rows = lgo_selections->get_selected_rows( ).
    DATA(lv_rows) = lines( lt_rows ).

    CASE e_salv_function.
      WHEN 'NOTA'.
        IF lv_rows IS INITIAL.
          "Nenhuma tarefa selecionada!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '004'.
        ELSEIF lv_rows EQ 1.
          READ TABLE lt_rows INTO DATA(lwa_row) INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>) INDEX lwa_row.
            IF sy-subrc EQ 0.
              PERFORM read_note USING lt_rows
                             CHANGING lt_note.

              CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
                EXPORTING
                  edit_mode = 'X'
                TABLES
                  t_txwnote = lt_note.

              PERFORM update_note USING lt_note
                                        lt_rows.
            ENDIF.
          ENDIF.
        ELSE.
          CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
            EXPORTING
              edit_mode = 'X'
            TABLES
              t_txwnote = lt_note.

          PERFORM update_note USING lt_note
                                    lt_rows.
        ENDIF.
        go_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
        go_table->refresh( ).
      WHEN 'MOTIVO'.
        IF lv_rows IS INITIAL.
          "Nenhuma tarefa selecionada!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '004'.
        ELSE.
          PERFORM select_reason CHANGING lv_reason
                                         lv_reason_txt.
          PERFORM update_reason USING lt_rows
                                      lv_reason
                                      lv_reason_txt.
        ENDIF.
        go_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
        go_table->refresh( ).
      WHEN 'DISP'.
        IF lv_rows IS INITIAL.
*         "Nenhuma ordem de processo selecionada!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '004'.
        ELSEIF lv_rows GT 1.
*         "Selecione somente uma ordem de processo!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '005'.
        ELSE.
          READ TABLE lt_rows INTO lwa_row INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_out ASSIGNING <lwa_out> INDEX lwa_row.
            IF sy-subrc EQ 0.
              IF <lwa_out>-aufnr IS NOT INITIAL.
                PERFORM display_production_order USING <lwa_out>-aufnr
                                                       'AUFNR'.
              ELSE.
*               "Ordem de processo inexistente para a linha selecionada!
                MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '009'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD. "on_user_command

  METHOD on_link_click.

    DATA: lgo_selections TYPE REF TO cl_salv_selections,
          lt_note        TYPE STANDARD TABLE OF txw_note INITIAL SIZE 0,
          lt_rows        TYPE salv_t_row,
          lt_aufnr       TYPE /agri/t_fmaufnr,
          lv_note        TYPE zabs_note,
          message        TYPE string.

    lgo_selections = go_table->get_selections( ).
    lt_rows = lgo_selections->get_selected_rows( ).
    DATA(lv_rows) = lines( lt_rows ).

    CASE column.
      WHEN 'AUFNR'.
        IF lv_rows IS INITIAL.
*         "Nenhuma ordem de processo selecionada!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '004'.
        ELSEIF lv_rows GT 1.
*         "Selecione somente uma ordem de processo!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '005'.
        ELSE.
          READ TABLE lt_rows INTO DATA(lwa_row) INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>) INDEX lwa_row.
            IF sy-subrc EQ 0.
              IF <lwa_out>-aufnr IS NOT INITIAL.
                PERFORM display_production_order USING <lwa_out>-aufnr
                                                       'AUFNR'.
              ELSE.
*               "Ordem de processo inexistente para a linha selecionada!
                MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '009'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'AUFNR_TO'.
        IF lv_rows IS INITIAL.
*         "Nenhuma ordem de tarefa selecionada!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '006'.
        ELSEIF lv_rows GT 1.
*         "Selecione somente uma ordem de tarefa!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '007'.
        ELSE.
          READ TABLE lt_rows INTO lwa_row INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_out ASSIGNING <lwa_out> INDEX lwa_row.
            IF sy-subrc EQ 0.
              IF <lwa_out>-aufnr_to IS NOT INITIAL.
                PERFORM display_production_order USING <lwa_out>-aufnr_to
                                                       'AUFNR_TO'.
              ELSE.
*               "Ordem de tarefa inexistente para a linha selecionada!
                MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '008'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'TPLNR_FL'.
        IF lv_rows IS INITIAL.
*         "Nenhum terreno selecionado!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '010'.
        ELSEIF lv_rows GT 1.
*         "Selecione somente um terreno!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '011'.
        ELSE.
          READ TABLE lt_rows INTO lwa_row INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE gt_out ASSIGNING <lwa_out> INDEX lwa_row.
            IF sy-subrc EQ 0.
              IF <lwa_out>-tplnr_fl IS NOT INITIAL.
                PERFORM display_terrain USING <lwa_out>-tplnr_fl.
              ELSE.
*               "Terreno inexistente para a linha selecionada!
                MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '012'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "on_link_click

ENDCLASS. "lcl_handle_events IMPLEMENTATION

DATA: go_functions     TYPE REF TO cl_salv_functions,
      go_disp_settings TYPE REF TO cl_salv_display_settings,
      go_columns       TYPE REF TO cl_salv_columns_table,
      go_column        TYPE REF TO cl_salv_column_table,
      go_layout        TYPE REF TO cl_salv_layout,
      go_sorts         TYPE REF TO cl_salv_sorts,
      key              TYPE salv_s_layout_key,
      event_handler    TYPE REF TO lcl_handle_events,
      color            TYPE lvc_s_colo.

CONSTANTS: BEGIN OF c_document_category,
             production_order TYPE /agri/gl_autyp VALUE 'AO',
             task_order       TYPE /agri/gl_autyp VALUE 'TO',
             produce_reciept  TYPE /agri/gl_autyp VALUE 'PR',
             work_order       TYPE /agri/gl_autyp VALUE 'WO',
             purchase_order   TYPE /agri/gl_autyp VALUE 'PO',
             confirmation     TYPE /agri/gl_autyp VALUE 'OC',
             reversals        TYPE /agri/gl_autyp VALUE 'CR',
             measurements     TYPE /agri/gl_autyp VALUE 'MD',
           END OF c_document_category.

SELECTION-SCREEN BEGIN OF BLOCK plan WITH FRAME TITLE TEXT-t03.
PARAMETERS: p_plan  RADIOBUTTON GROUP plan MODIF ID typ DEFAULT 'X'
                    USER-COMMAND plan,
            p_nplan RADIOBUTTON GROUP plan MODIF ID typ,
*...Rendimento-Início
            p_ren   RADIOBUTTON GROUP plan MODIF ID typ.
*...Rendimento-Fim
SELECTION-SCREEN END OF BLOCK plan.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-t05.
SELECT-OPTIONS: so_udate FOR jcds-udate MODIF ID pln.
SELECT-OPTIONS: so_schdt FOR /agri/fmfpitm-schdt MODIF ID npl.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: so_auart FOR /agri/fmfphdr-auart MODIF ID gen,
                so_aufnr FOR /agri/fmfphdr-aufnr MODIF ID gen,
                so_strno FOR /agri/glflot-strno MODIF ID gen,
                so_cmnum FOR /agri/fmfphdr-cmnum MODIF ID gen,
                so_varia FOR /agri/fmfphdr-varia MODIF ID gen,
                so_matnr FOR /agri/fmfphdr-matnr MODIF ID gen,
                so_iwerk FOR /agri/fmfphdr-iwerk MODIF ID gen,
                so_task  FOR /agri/fmfpitm-aufnr_to MODIF ID gen,
                so_tmatn FOR /agri/fmfpcom-matnr MODIF ID gen.
PARAMETERS: p_class TYPE /agri/fmfphdr-class DEFAULT '1' NO-DISPLAY,
            p_cstat TYPE /agri/fmfphdr-cstat DEFAULT 'CNF' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
SELECT-OPTIONS: so_ernam   FOR /agri/fmfphdr-ernam MODIF ID gen,
                so_erdat   FOR /agri/fmfphdr-erdat MODIF ID gen,
                so_aenam   FOR /agri/fmfphdr-aenam MODIF ID gen,
                so_aedat   FOR /agri/fmfphdr-aedat MODIF ID gen.
SELECTION-SCREEN END OF BLOCK b02.

*...Rendimento-Início
SELECTION-SCREEN BEGIN OF BLOCK b06 WITH FRAME TITLE TEXT-t06.
PARAMETERS: p_werks TYPE /agri/fmfphdr-iwerk MODIF ID ren.
SELECT-OPTIONS: s_matnr FOR /agri/fmfphdr-matnr MODIF ID ren,
                s_tplnr FOR /agri/fmfphdr-tplnr_fl MODIF ID ren,
                s_cmnum FOR /agri/fmfphdr-cmnum NO INTERVALS NO-EXTENSION MODIF ID ren,
                s_datum FOR sy-datum NO-EXTENSION MODIF ID ren.
SELECTION-SCREEN END OF BLOCK b06.
*...Rendimento-Fim

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_update.

AT SELECTION-SCREEN.
  IF p_ren EQ abap_true
  AND sy-ucomm EQ 'ONLI'.
*--Selection Screen Validations
    PERFORM selection_validations.
  ENDIF.

START-OF-SELECTION.
  PERFORM data_select.

END-OF-SELECTION.
  IF p_plan EQ abap_true
  OR p_nplan EQ abap_true.
    IF gt_fmfpitm[] IS NOT INITIAL.
      PERFORM data_build USING p_plan.
      PERFORM data_display USING p_plan.
    ELSE.
*"Nenhum documento a ser processado; verificar seleção
      MESSAGE s751(/agri/global).
    ENDIF.
  ELSEIF p_ren EQ abap_true.
*--Initializing Global Data
    PERFORM initialize_global_data.
*--Preparing Task Order Monitor Data
    PERFORM build_task_order_data.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_display USING p_plan TYPE abap_bool.

  DATA: lt_columns_ref TYPE salv_t_column_ref,
        lo_column      TYPE REF TO cl_salv_column,
        lt_color       TYPE lvc_t_scol,
*        lo_column      TYPE REF TO cl_salv_column_table,
        lo_exc         TYPE REF TO cx_root,
        lv_list_header TYPE lvc_title,
        lv_msg_text    TYPE string.

  IF gt_out[] IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = go_table
          CHANGING
            t_table      = gt_out ).
      CATCH cx_salv_msg.
    ENDTRY.

    IF go_table IS NOT BOUND.
* Create ALV
      TRY.
          cl_salv_table=>factory(
          EXPORTING
            list_display = if_salv_c_bool_sap=>false
            IMPORTING
              r_salv_table = go_table
            CHANGING
              t_table      = gt_out ).
        CATCH cx_salv_msg INTO lo_exc.
          lv_msg_text = lo_exc->if_message~get_text( ).
          MESSAGE lv_msg_text TYPE 'I' DISPLAY LIKE 'A'.
      ENDTRY.
    ENDIF.

    IF go_table IS BOUND.
*... §3 Functions
      go_table->set_screen_status(
        pfstatus      = 'ZSALV_TABLE_STANDARD'
        report        = sy-repid
        set_functions = go_table->c_functions_all ).
*...Layout Settings
      go_layout = go_table->get_layout( ).
      key-report = sy-repid.
      go_layout->set_key( key ).
      go_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      go_layout->set_default( if_salv_c_bool_sap=>true ).

      TRY.
*...Change columns
          go_columns = go_table->get_columns( ).
          go_columns->set_optimize( if_salv_c_bool_sap=>true ).
          IF go_columns IS NOT INITIAL.
            lt_columns_ref = go_columns->get( ).

            LOOP AT lt_columns_ref INTO DATA(lwa_column_ref).
              IF ( p_plan EQ abap_false AND
                  ( lwa_column_ref-columnname EQ 'GAMNG' OR
                    lwa_column_ref-columnname EQ 'TOMNG' OR
                    lwa_column_ref-columnname EQ 'MEINH' OR
                    lwa_column_ref-columnname EQ 'MATNR' OR
                    lwa_column_ref-columnname EQ 'MAKTX' OR
                    lwa_column_ref-columnname EQ 'ERFMG' OR
                    lwa_column_ref-columnname EQ 'ERFME' OR
                    lwa_column_ref-columnname EQ 'DOSE_PROG' OR
                    lwa_column_ref-columnname EQ 'DOSE_REAL' OR
                    lwa_column_ref-columnname EQ 'DESVIO' OR
                    lwa_column_ref-columnname EQ 'COMNG' ) )
              OR lwa_column_ref-columnname EQ 'POSNR'.
                lwa_column_ref-r_column->set_visible( if_salv_c_bool_sap=>false ).
              ENDIF.

              IF lwa_column_ref-columnname EQ 'LMNGA'.
                lwa_column_ref-r_column->set_visible( if_salv_c_bool_sap=>false ).
              ENDIF.

              TRY.
                  go_column ?= go_columns->get_column( lwa_column_ref-columnname ).
                CATCH cx_salv_not_found.
              ENDTRY.
              IF lwa_column_ref-columnname EQ 'AUFNR'.
                go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
**                go_column->set_output_length('12').
              ELSEIF lwa_column_ref-columnname EQ 'AUFNR_TO'.
                go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              ELSEIF lwa_column_ref-columnname EQ 'TPLNR_FL'.
                go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
**                go_column->set_output_length('14').
              ELSEIF lwa_column_ref-columnname EQ 'ZZNOTE'.
**                go_column->set_output_length('128').
              ELSEIF lwa_column_ref-columnname EQ 'ICON'.
                go_column->set_icon( if_salv_c_bool_sap=>true ).
                go_column->set_short_text( 'Status' ).
                go_column->set_medium_text( 'Status' ).
                go_column->set_long_text( 'Status' ).
              ELSEIF lwa_column_ref-columnname EQ 'ZZREASON_TXT'.
                go_column->set_short_text( 'Txt.Motivo' ).
                go_column->set_medium_text( 'Txt.Motivo' ).
                go_column->set_long_text( 'Txt.Motivo' ).
              ELSEIF lwa_column_ref-columnname EQ 'GAMNG'.
                go_column->set_short_text( 'Área Prog.' ).
                go_column->set_medium_text( 'Área Programada' ).
                go_column->set_long_text( 'Área Programada' ).
              ELSEIF lwa_column_ref-columnname EQ 'TOMNG'.
                go_column->set_short_text( 'Área Real.' ).
                go_column->set_medium_text( 'Área Realizada' ).
                go_column->set_long_text( 'Área Realizada' ).
              ELSEIF lwa_column_ref-columnname EQ 'ERFMG'.
                go_column->set_short_text( 'Prod.Prog.' ).
                go_column->set_medium_text( 'Prod.Programado' ).
                go_column->set_long_text( 'Produto Programado' ).
              ELSEIF lwa_column_ref-columnname EQ 'UDATE'.
                IF p_plan EQ abap_true.
                  go_column->set_short_text( 'Dta.Enc.' ).
                  go_column->set_medium_text( 'Dta.Encer.Téc.' ).
                  go_column->set_long_text( 'Dta.Encer.Téc.' ).
                ELSE.
                  go_column->set_short_text( 'Dta.Progr.' ).
                  go_column->set_medium_text( 'Dta.Progr.' ).
                  go_column->set_long_text( 'Dta.Progr.' ).
                ENDIF.
              ELSEIF lwa_column_ref-columnname EQ 'COMNG'.
                go_column->set_short_text( 'Prod.Real.' ).
                go_column->set_medium_text( 'Prod.Realizado' ).
                go_column->set_long_text( 'Produto Realizado' ).
              ELSEIF lwa_column_ref-columnname EQ 'LMNGA'.
                go_column->set_short_text( 'Prod.Conf.' ).
                go_column->set_medium_text( 'Prod.Confirmar' ).
                go_column->set_long_text( 'Prod.Confirmar' ).
              ELSEIF lwa_column_ref-columnname EQ 'DOSE_PROG'.
                go_column->set_short_text( 'Dose Prog.' ).
                go_column->set_medium_text( 'Dose Programada' ).
                go_column->set_long_text( 'Dose Programada' ).
              ELSEIF lwa_column_ref-columnname EQ 'DOSE_REAL'.
                go_column->set_short_text( 'Dose Real.' ).
                go_column->set_medium_text( 'Dose Realizada' ).
                go_column->set_long_text( 'Dose Realizada' ).
              ELSEIF lwa_column_ref-columnname EQ 'DESVIO'.
                go_column->set_short_text( '% Desvio' ).
                go_column->set_medium_text( '% Desvio' ).
                go_column->set_long_text( '% Desvio' ).
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH cx_salv_not_found INTO lo_exc.
          lv_msg_text = lo_exc->if_message~get_text( ).
          MESSAGE lv_msg_text TYPE 'I' DISPLAY LIKE 'A'.
      ENDTRY.

      TRY.
          go_columns->set_color_column( 'T_COLOR' ).
        CATCH cx_salv_data_error .
      ENDTRY.

      go_events = go_table->get_event( ).
      CREATE OBJECT event_handler.
      SET HANDLER event_handler->on_user_command FOR go_events.
      SET HANDLER event_handler->on_link_click FOR go_events.
*...Set up selections
      go_selections = go_table->get_selections( ).
      go_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).
*...Set title
      lv_list_header = TEXT-004. "Justificar Ordens de Tarefa
      go_disp_settings ?= go_table->get_display_settings( ).
      go_disp_settings->set_list_header( value = lv_list_header ).
      go_disp_settings->set_striped_pattern( cl_salv_display_settings=>true ).
      go_sorts = go_table->get_sorts( ).
* Display
      go_table->display( ).
    ENDIF.
  ELSE.
*"Nenhum documento a ser processado; verificar seleção
    MESSAGE s751(/agri/global).
  ENDIF.

ENDFORM.                    " DATA_DISPLAY

*&---------------------------------------------------------------------*
*& Form DATA_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_PLAN
*&---------------------------------------------------------------------*
FORM data_build USING p_plan TYPE abap_bool.

  DATA: lt_dd07         TYPE dd07v_tab,
        lwa_makt        TYPE makt,
        lwa_fmfpitm_to  LIKE LINE OF gt_fmfpitm,
        lwa_fmocopr_sum LIKE LINE OF gt_fmocopr_sum,
        lv_domval       TYPE domvalue_l,
        lv_tech_comp    TYPE abap_bool,
        lv_udate        TYPE cddatum,
        lv_note         TYPE zabs_note.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZABS_DOM_REASON'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_dd07
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  IF p_plan EQ abap_true.
    LOOP AT gt_fmfphdr ASSIGNING FIELD-SYMBOL(<lwa_fmfphdr>).
      READ TABLE gt_fmfpitm INTO DATA(lwa_fmfpitm)
        WITH KEY aufnr = <lwa_fmfphdr>-aufnr BINARY SEARCH.

      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.

        DATA(lv_skip) = abap_false.
        IF lwa_fmfpitm-aufnr_to IS NOT INITIAL.
          DATA(lv_ref_aufnr) = lwa_fmfpitm-aufnr_to.
          DATA(lv_to) = abap_true.
          PERFORM check_task_order_status USING lwa_fmfpitm-aufnr_to
                                       CHANGING lv_tech_comp
                                                lv_udate.

          IF lv_tech_comp EQ abap_false.
            lv_skip = abap_true.
          ELSE.
            lv_skip = abap_false.
          ENDIF.
        ELSE.
          lv_ref_aufnr = lwa_fmfpitm-aufnr.
          lv_to = abap_false.
          lv_skip = abap_false.
        ENDIF.

        IF lv_skip EQ abap_false.
          READ TABLE gt_fmfpcom TRANSPORTING NO FIELDS
            WITH KEY aufnr = lv_ref_aufnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            LOOP AT gt_fmfpcom INTO DATA(lwa_fmfpcom) FROM sy-tabix.
              IF lwa_fmfpcom-aufnr NE lv_ref_aufnr.
                EXIT.
              ENDIF.

              INSERT INITIAL LINE INTO TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>).
              IF sy-subrc EQ 0.
                <lwa_out>-tplnr_fl = <lwa_fmfphdr>-tplnr_fl.
                <lwa_out>-cmnum    = <lwa_fmfphdr>-cmnum.
                <lwa_out>-varia    = <lwa_fmfphdr>-varia.
                <lwa_out>-cpros    = <lwa_fmfphdr>-cpros.
                <lwa_out>-iwerk    = <lwa_fmfphdr>-iwerk.
                <lwa_out>-udate    = lv_udate.
                <lwa_out>-aufnr    = lwa_fmfpitm-aufnr.
                <lwa_out>-posnr    = lwa_fmfpitm-posnr.
                <lwa_out>-aufnr_to = lwa_fmfpitm-aufnr_to.
                <lwa_out>-gamng    = lwa_fmfpitm-gamng.
                <lwa_out>-meinh    = lwa_fmfpitm-meinh.
                <lwa_out>-zzreason = lwa_fmfpitm-zzreason.

                lv_domval = lwa_fmfpitm-zzreason.
                IF lv_domval IS NOT INITIAL.
                  READ TABLE lt_dd07 INTO DATA(lwa_dd07)
                    WITH KEY domvalue_l = lv_domval.
                  IF sy-subrc EQ 0.
                    <lwa_out>-zzreason_txt = lwa_dd07-ddtext.
                  ENDIF.
                ENDIF.

                <lwa_out>-zznote = lwa_fmfpitm-zznote.
                IF <lwa_out>-zznote IS INITIAL
                AND <lwa_out>-zzreason IS INITIAL.
                  <lwa_out>-icon = icon_red_light.
                ELSEIF <lwa_out>-zznote IS INITIAL
                    OR <lwa_out>-zzreason IS INITIAL.
                  <lwa_out>-icon = icon_yellow_light.
                ELSE.
                  <lwa_out>-icon = icon_green_light.
                ENDIF.

                key_text_get 'MAKT' 'MATNR' lwa_fmfpcom-matnr
                         lwa_makt <lwa_out>-ltxa1.

                <lwa_out>-matnr = lwa_fmfpcom-matnr.

                key_text_get 'MAKT' 'MATNR' lwa_fmfpcom-matnr
                         lwa_makt <lwa_out>-maktx.

                <lwa_out>-erfmg = lwa_fmfpcom-erfmg.
                <lwa_out>-comng = lwa_fmfpcom-comng.
                IF lwa_fmfpcom-erfmg GT lwa_fmfpcom-comng.
                  <lwa_out>-lmnga = lwa_fmfpcom-erfmg - lwa_fmfpcom-comng.
                ENDIF.
                <lwa_out>-erfme = lwa_fmfpcom-erfme.

                IF <lwa_out>-gamng IS NOT INITIAL.
                  <lwa_out>-dose_prog = <lwa_out>-erfmg / <lwa_out>-gamng.
                ENDIF.

                IF lv_to = abap_true.
                  CLEAR: lwa_fmfpitm_to, lwa_fmocopr_sum.
                  READ TABLE gt_fmfpitm INTO lwa_fmfpitm_to
                    WITH KEY aufnr = lv_ref_aufnr BINARY SEARCH.

                  IF sy-subrc EQ 0.
                    READ TABLE gt_fmocopr_sum INTO lwa_fmocopr_sum
                      WITH KEY aufnr = lwa_fmfpitm_to-aufnr
                               posnr = lwa_fmfpitm_to-posnr
                               rueck = lwa_fmfpitm_to-rueck BINARY SEARCH.

                    <lwa_out>-tomng = lwa_fmocopr_sum-lmnga.

                    IF <lwa_out>-tomng IS NOT INITIAL.
                      <lwa_out>-dose_real = <lwa_out>-comng / <lwa_out>-tomng.
                    ENDIF.

                    IF <lwa_out>-dose_prog IS NOT INITIAL.
                      IF <lwa_out>-dose_real IS INITIAL.
                        <lwa_out>-desvio = 100.
                      ELSE.
                        <lwa_out>-desvio = ( (  <lwa_out>-dose_real * 100 ) /
                                                <lwa_out>-dose_prog ) - 100.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        READ TABLE gt_fmfpitm INTO lwa_fmfpitm
        INDEX lv_tabix COMPARING aufnr.
      ENDWHILE.
    ENDLOOP.
  ELSE.
    LOOP AT gt_fmfphdr ASSIGNING <lwa_fmfphdr>. "WHERE aufnr EQ '000020000040'.
      READ TABLE gt_fmfpitm INTO lwa_fmfpitm
        WITH KEY aufnr = <lwa_fmfphdr>-aufnr BINARY SEARCH.

      WHILE sy-subrc EQ 0.
        lv_tabix = sy-tabix + 1.

        INSERT INITIAL LINE INTO TABLE gt_out ASSIGNING <lwa_out>.
        IF sy-subrc EQ 0.
          <lwa_out>-tplnr_fl = <lwa_fmfphdr>-tplnr_fl.
          <lwa_out>-cmnum    = <lwa_fmfphdr>-cmnum.
          <lwa_out>-varia    = <lwa_fmfphdr>-varia.
          <lwa_out>-cpros    = <lwa_fmfphdr>-cpros.
          <lwa_out>-iwerk    = <lwa_fmfphdr>-iwerk.
          <lwa_out>-udate    = lwa_fmfpitm-schdt.
          <lwa_out>-aufnr    = lwa_fmfpitm-aufnr.
          <lwa_out>-posnr    = lwa_fmfpitm-posnr.
          <lwa_out>-aufnr_to = lwa_fmfpitm-aufnr_to.
          <lwa_out>-gamng    = lwa_fmfpitm-gamng.
          <lwa_out>-meinh    = lwa_fmfpitm-meinh.
          <lwa_out>-zzreason = lwa_fmfpitm-zzreason.

          lv_domval = lwa_fmfpitm-zzreason.
          IF lv_domval IS NOT INITIAL.
            READ TABLE lt_dd07 INTO lwa_dd07
              WITH KEY domvalue_l = lv_domval.
            IF sy-subrc EQ 0.
              <lwa_out>-zzreason_txt = lwa_dd07-ddtext.
            ENDIF.
          ENDIF.

          <lwa_out>-zznote = lwa_fmfpitm-zznote.
          IF <lwa_out>-zznote IS INITIAL
          AND <lwa_out>-zzreason IS INITIAL.
            <lwa_out>-icon = icon_red_light.
          ELSEIF <lwa_out>-zznote IS INITIAL
              OR <lwa_out>-zzreason IS INITIAL.
            <lwa_out>-icon = icon_yellow_light.
          ELSE.
            <lwa_out>-icon = icon_green_light.
          ENDIF.
        ENDIF.

        CLEAR lwa_fmfpcom.
        READ TABLE gt_fmfpcom INTO lwa_fmfpcom
          WITH KEY aufnr = lwa_fmfpitm-aufnr
                   posnr = lwa_fmfpitm-posnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          key_text_get 'MAKT' 'MATNR' lwa_fmfpcom-matnr
                   lwa_makt <lwa_out>-ltxa1.
        ELSE.
          CLEAR <lwa_out>-ltxa1.
        ENDIF.

        READ TABLE gt_fmfpitm INTO lwa_fmfpitm
        INDEX lv_tabix COMPARING aufnr.
      ENDWHILE.
    ENDLOOP.
  ENDIF.

  SORT gt_out BY udate.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DATA_SELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM data_select.

  DATA: lr_stat  TYPE RANGE OF j_status,
        lwa_stat LIKE LINE OF lr_stat.

  REFRESH: gt_fmfphdr, gt_fmfphdr_to, gt_fmfpitm, gt_fmfpitm_copy,
           gt_fmfpcom, gt_ocnum, gt_fmfpbch, gt_afwi, gt_matdoc,
           gt_glflot,  gt_fmocopr, gt_fmocopr_sum, gt_out, gt_jest,
           gt_jcds.

*--Get terrain
  IF so_strno[] IS NOT INITIAL.
    SELECT tplnr_fl,
           strno,
           pltxt
      FROM /agri/glflot
      INTO TABLE @gt_glflot
      WHERE strno IN @so_strno[].
    IF sy-subrc EQ 0.
      SORT gt_glflot BY tplnr_fl.
    ENDIF.
  ENDIF.

*--Fetch data from farm process header
  IF gt_glflot[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfphdr
      INTO TABLE @gt_fmfphdr
      FOR ALL ENTRIES IN @gt_glflot
     WHERE aufnr    IN @so_aufnr[]
       AND auart    IN @so_auart[]
       AND autyp    EQ @c_document_category-production_order
       AND tplnr_fl EQ @gt_glflot-tplnr_fl
       AND cmnum    IN @so_cmnum[]
       AND varia    IN @so_varia[]
       AND class    EQ @p_class
       AND matnr    IN @so_matnr[]
*       AND cstat    EQ @p_cstat
       AND iwerk    IN @so_iwerk[]
       AND tecom    EQ @space
       AND ernam    IN @so_ernam[]
       AND erdat    IN @so_erdat[]
       AND aenam    IN @so_aenam[]
       AND aedat    IN @so_aedat[].
  ELSE.
    SELECT * FROM /agri/fmfphdr
      INTO TABLE @gt_fmfphdr
      WHERE aufnr IN @so_aufnr[]
        AND auart IN @so_auart[]
        AND autyp EQ @c_document_category-production_order
        AND cmnum IN @so_cmnum[]
        AND varia IN @so_varia[]
        AND class EQ @p_class
        AND matnr IN @so_matnr[]
*        AND cstat EQ @p_cstat
        AND iwerk IN @so_iwerk[]
        AND tecom EQ @space
        AND ernam IN @so_ernam[]
        AND erdat IN @so_erdat[]
        AND aenam IN @so_aenam[]
        AND aedat IN @so_aedat[].
  ENDIF.
  SORT gt_fmfphdr BY aufnr.

*--Fetch data from farm process items
  IF gt_fmfphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpitm
      INTO TABLE @gt_fmfpitm
      FOR ALL ENTRIES IN @gt_fmfphdr
     WHERE aufnr    EQ @gt_fmfphdr-aufnr
       AND aufnr_to IN @so_task[]
       AND schdt    IN @so_schdt[].

    IF p_plan EQ abap_true.
      DELETE gt_fmfpitm WHERE aufnr_to IS INITIAL.
    ELSE.
      DELETE gt_fmfpitm WHERE unpln EQ abap_false.
    ENDIF.

    gt_fmfpitm_copy[] = gt_fmfpitm[].
    DELETE gt_fmfpitm_copy WHERE aufnr_to IS INITIAL.
    IF gt_fmfpitm_copy[] IS NOT INITIAL.
      SELECT * FROM /agri/fmfpitm
        APPENDING TABLE @gt_fmfpitm
        FOR ALL ENTRIES IN @gt_fmfpitm_copy
       WHERE aufnr EQ @gt_fmfpitm_copy-aufnr_to.

      IF sy-subrc EQ 0.
        SELECT * FROM /agri/fmfphdr
          INTO TABLE @gt_fmfphdr_to
          FOR ALL ENTRIES IN @gt_fmfpitm_copy
         WHERE aufnr EQ @gt_fmfpitm_copy-aufnr_to.
      ENDIF.

      SELECT * FROM /agri/fmfpcom
        APPENDING TABLE @gt_fmfpcom
        FOR ALL ENTRIES IN @gt_fmfpitm_copy
       WHERE aufnr EQ @gt_fmfpitm_copy-aufnr_to.
    ENDIF.
    SORT: gt_fmfphdr    BY aufnr,
          gt_fmfphdr_to BY aufnr,
          gt_fmfpitm    BY aufnr posnr,
          gt_fmfpcom    BY aufnr posnr contr.

    IF gt_fmfphdr_to[] IS NOT INITIAL
    AND p_plan EQ abap_true.
      lwa_stat = 'IEQ'.
      lwa_stat-low = c_syst_stat-created.  APPEND lwa_stat TO lr_stat.
      lwa_stat-low = c_syst_stat-released. APPEND lwa_stat TO lr_stat.
      lwa_stat-low = c_syst_stat-tec_comp. APPEND lwa_stat TO lr_stat.

      SELECT * FROM jest
        INTO TABLE @gt_jest
        FOR ALL ENTRIES IN @gt_fmfphdr_to
       WHERE objnr EQ @gt_fmfphdr_to-objnr
         AND stat  IN @lr_stat[].

      IF sy-subrc EQ 0.
        SELECT * FROM jcds
          INTO TABLE @gt_jcds
          FOR ALL ENTRIES IN @gt_jest
         WHERE objnr EQ @gt_jest-objnr
           AND stat  EQ @gt_jest-stat
           AND udate IN @so_udate[].

        SORT: gt_jest BY objnr stat inact,
              gt_jcds BY objnr stat.

        IF so_udate[] IS NOT INITIAL.
          DELETE gt_jcds WHERE udate NOT IN so_udate[].
        ENDIF.
      ENDIF.
    ENDIF.

*--Get order confirmation
    SELECT *
      FROM /agri/fmocindx
      INTO TABLE @gt_ocnum
      FOR ALL ENTRIES IN @gt_fmfpitm
     WHERE aufnr EQ @gt_fmfpitm-aufnr.

    IF gt_ocnum[] IS NOT INITIAL.
      SORT gt_ocnum BY aufnr.
*--Get operations based on confirmation
      SELECT ocnum,
             rueck,
             rmzhl,
             aufnr,
             posnr,
             vornr,
             ltxa1,
             lmnga,
             meinh
        FROM /agri/fmocopr
        INTO TABLE @gt_fmocopr
        FOR ALL ENTRIES IN @gt_ocnum
       WHERE ocnum EQ @gt_ocnum-ocnum.

      IF sy-subrc EQ 0.
        LOOP AT gt_fmocopr INTO DATA(lwa_fmocopr).
          CLEAR: lwa_fmocopr-ocnum, lwa_fmocopr-vornr,
                 lwa_fmocopr-rmzhl, lwa_fmocopr-ltxa1.
          COLLECT lwa_fmocopr INTO gt_fmocopr_sum.
        ENDLOOP.
        SORT: gt_fmocopr BY ocnum,
              gt_fmocopr_sum BY aufnr posnr rueck.

*--Get posted goods data
        SELECT *
          FROM afwi
          INTO TABLE @gt_afwi
          FOR ALL ENTRIES IN @gt_fmocopr
         WHERE rueck EQ @gt_fmocopr-rueck
           AND rmzhl EQ @gt_fmocopr-rmzhl.

        IF gt_afwi[] IS NOT INITIAL.
          SORT gt_afwi BY rueck rmzhl.

*--Get posting date
          SELECT budat,
                 cpudt,
                 cputm,
                 mblnr,
                 mjahr,
                 zeile,
                 aufnr,
                 anln1,
                 anln2,
                 bwart
            FROM matdoc
            INTO TABLE @gt_matdoc
            FOR ALL ENTRIES IN @gt_afwi
           WHERE mblnr      EQ @gt_afwi-mblnr
             AND mjahr      EQ @gt_afwi-mjahr
             AND zeile      EQ @gt_afwi-mblpo
             AND cancelled  EQ @space
             AND bwart      EQ @c_move_type-goods_receipt.

          SORT gt_matdoc BY aufnr.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT * FROM /agri/fmfpitm
      INTO TABLE @gt_fmfpitm
     WHERE aufnr_to IN @so_task[].
  ENDIF.

  IF p_plan EQ abap_false.
    DELETE gt_fmfpitm WHERE unpln EQ abap_false.
  ENDIF.

  IF gt_fmfphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpcom
      APPENDING TABLE @gt_fmfpcom
      FOR ALL ENTRIES IN @gt_fmfphdr
     WHERE aufnr EQ @gt_fmfphdr-aufnr
       AND matnr IN @so_tmatn[].
  ELSE.
    SELECT * FROM /agri/fmfpcom
      APPENDING TABLE @gt_fmfpcom
     WHERE aufnr IN @so_aufnr[]
       AND matnr IN @so_tmatn[].
  ENDIF.
  SORT gt_fmfpcom BY aufnr posnr contr.

  IF gt_fmfphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpbch
      INTO TABLE @gt_fmfpbch
      FOR ALL ENTRIES IN @gt_fmfphdr
     WHERE aufnr = @gt_fmfphdr-aufnr.
  ELSE.
    SELECT * FROM /agri/fmfpbch
      INTO TABLE @gt_fmfpbch
     WHERE aufnr IN @so_aufnr[].
  ENDIF.
  SORT gt_fmfpbch BY aufnr contr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_PRODUCTION_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LWA_OUT>_AUFNR_TO
*&---------------------------------------------------------------------*
FORM display_production_order USING lv_aufnr
                                    lv_field.

  DATA: lwa_aufnr TYPE /agri/s_fmaufnr,
        lt_aufnr  TYPE /agri/t_fmaufnr.

  CHECK lv_aufnr IS NOT INITIAL.
  lwa_aufnr-aufnr = lv_aufnr.
  APPEND lwa_aufnr TO lt_aufnr.

  CASE lv_field.
    WHEN 'AUFNR'.
      CALL FUNCTION '/AGRI/FMFP_PROCESS'
        EXPORTING
          i_mode                        = 'A'
          i_display_only                = 'X'
          i_save                        = space
          it_aufnr                      = lt_aufnr
        EXCEPTIONS
          invalid_parameter_combination = 1
          enter_order_number            = 2
          no_documents_to_process       = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    WHEN 'AUFNR_TO'.
      CALL FUNCTION '/AGRI/FMCO_PROCESS'
        EXPORTING
          i_save                        = space
          it_aufnr                      = lt_aufnr
        EXCEPTIONS
          enter_order_number            = 1
          invalid_parameter_combination = 2
          OTHERS                        = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TERRAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_terrain USING lv_tplnr TYPE /agri/gltplnr_fl.

  DATA: lt_tplnr  TYPE /agri/t_gltplnr,
        lwa_tplnr TYPE /agri/s_gltplnr.
  REFRESH: lt_tplnr.

  lwa_tplnr-tplnr_fl = lv_tplnr.
  APPEND lwa_tplnr TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLFL_PROCESS'
    EXPORTING
*     I_MODE        = 'A'
*     I_DISPLAY_ONLY = 'X'
      it_tplnr      = lt_tplnr
    EXCEPTIONS
      enter_terrain = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_TERRAIN

*&---------------------------------------------------------------------*
*& Form UPDATE_NOTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_NOTE
*&      --> LT_ROWS
*&---------------------------------------------------------------------*
FORM update_note USING lt_note TYPE type_txw_note
                       lt_rows TYPE salv_t_row.

  DATA: lv_note TYPE zabs_note.

  LOOP AT lt_note INTO DATA(lwa_note).
    IF sy-tabix EQ 1.
      lv_note = lwa_note.
    ELSE.
      CONCATENATE lv_note space lwa_note INTO lv_note.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_rows INTO DATA(lwa_row).
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>) INDEX lwa_row.
    IF sy-subrc EQ 0.
      <lwa_out>-zznote = lv_note.
      IF <lwa_out>-zznote IS INITIAL
      AND <lwa_out>-zzreason IS INITIAL.
        <lwa_out>-icon = icon_red_light.
      ELSEIF <lwa_out>-zznote IS INITIAL
          OR <lwa_out>-zzreason IS INITIAL.
        <lwa_out>-icon = icon_yellow_light.
      ELSE.
        <lwa_out>-icon = icon_green_light.
      ENDIF.
      READ TABLE gt_fmfpitm INTO DATA(lwa_fmfpitm)
        WITH KEY aufnr = <lwa_out>-aufnr
                 posnr = <lwa_out>-posnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        UPDATE /agri/fmfpitm SET zznote = lv_note
          WHERE aufnr = <lwa_out>-aufnr
            AND posnr = <lwa_out>-posnr.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_NOTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ROWS
*&      <-- LT_NOTE
*&---------------------------------------------------------------------*
FORM read_note USING lt_rows TYPE salv_t_row
            CHANGING lt_note TYPE type_txw_note.

  DATA: lt_split  TYPE STANDARD TABLE OF txw_note INITIAL SIZE 0,
        lr_split  TYPE REF TO txw_note,
        lv_zznote TYPE zabs_note,
        lv_len    TYPE int4.

  READ TABLE lt_rows INTO DATA(lwa_row) INDEX 1.
  IF sy-subrc EQ 0.
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>) INDEX lwa_row.
    IF sy-subrc EQ 0.
      SPLIT <lwa_out>-zznote AT space INTO TABLE lt_split.
      LOOP AT lt_split REFERENCE INTO lr_split.
        lv_len = strlen( lv_zznote ) + strlen( lr_split->* ).
        IF lv_len LT 72.
          CONCATENATE lv_zznote lr_split->* INTO lv_zznote SEPARATED BY space.
        ELSE.
          APPEND lv_zznote TO lt_note.
          lv_zznote = lr_split->*.
        ENDIF.
      ENDLOOP.
      APPEND lv_zznote TO lt_note.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_REASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_reason CHANGING lv_reason     TYPE zabs_reason
                            lv_reason_txt TYPE val_text.

  DATA: lt_sval   TYPE TABLE OF sval,
        lt_dd07   TYPE dd07v_tab,
        lv_domval TYPE domvalue_l,
        lv_rcode  TYPE c.

  CLEAR: lv_reason, lv_reason_txt.
  INSERT INITIAL LINE INTO TABLE lt_sval
    ASSIGNING FIELD-SYMBOL(<lwa_sval>).
  IF sy-subrc EQ 0.
    <lwa_sval>-fieldname = 'ZZREASON'.
    <lwa_sval>-tabname   = '/AGRI/FMFPITM'.
    CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
      EXPORTING
        popup_title     = TEXT-t04
      IMPORTING
        returncode      = lv_rcode
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc EQ 0.
      READ TABLE lt_sval INTO DATA(lwa_sval) INDEX 1.
      IF sy-subrc EQ 0.
        lv_domval = lv_reason = lwa_sval-value.
      ENDIF.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZABS_DOM_REASON'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_dd07
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0
      AND lv_reason IS NOT INITIAL.
        READ TABLE lt_dd07 INTO DATA(lwa_dd07)
          WITH KEY domvalue_l = lv_domval.
        IF sy-subrc EQ 0.
          lv_reason_txt = lwa_dd07-ddtext.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_REASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_REASON
*&      --> LV_REASON_TXT
*&---------------------------------------------------------------------*
FORM update_reason USING lt_rows TYPE salv_t_row
                         lv_reason
                         lv_reason_txt.

  LOOP AT lt_rows INTO DATA(lwa_row).
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<lwa_out>) INDEX lwa_row.
    IF sy-subrc EQ 0.
      <lwa_out>-zzreason = lv_reason.
      <lwa_out>-zzreason_txt = lv_reason_txt.
      IF <lwa_out>-zznote IS INITIAL
      AND <lwa_out>-zzreason IS INITIAL.
        <lwa_out>-icon = icon_red_light.
      ELSEIF <lwa_out>-zznote IS INITIAL
          OR <lwa_out>-zzreason IS INITIAL.
        <lwa_out>-icon = icon_yellow_light.
      ELSE.
        <lwa_out>-icon = icon_green_light.
      ENDIF.

      READ TABLE gt_fmfpitm INTO DATA(lwa_fmfpitm)
        WITH KEY aufnr = <lwa_out>-aufnr
                 posnr = <lwa_out>-posnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        UPDATE /agri/fmfpitm SET zzreason = lv_reason
          WHERE aufnr = <lwa_out>-aufnr
            AND posnr = <lwa_out>-posnr.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TASK_ORDER_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> AUFNR_TO
*&      <-- DATA(LV_TECH_COMP)
*&---------------------------------------------------------------------*
FORM check_task_order_status USING lv_aufnr_to TYPE /agri/fmfpnum
                          CHANGING lv_tech_comp
                                   lv_udate.

  CLEAR lv_udate.
  lv_tech_comp = abap_true.
  READ TABLE gt_fmfphdr_to INTO DATA(lwa_fmfphdr)
    WITH KEY aufnr = lv_aufnr_to BINARY SEARCH.
  IF sy-subrc EQ 0.
    READ TABLE gt_jest TRANSPORTING NO FIELDS
      WITH KEY objnr = lwa_fmfphdr-objnr
               stat  = c_syst_stat-created
               inact = abap_true BINARY SEARCH.
    IF sy-subrc NE 0.
      lv_tech_comp = abap_false.
    ELSE.
      READ TABLE gt_jest TRANSPORTING NO FIELDS
        WITH KEY objnr = lwa_fmfphdr-objnr
                 stat  = c_syst_stat-released
                 inact = abap_true BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_tech_comp = abap_false.
      ELSE.
        READ TABLE gt_jest TRANSPORTING NO FIELDS
          WITH KEY objnr = lwa_fmfphdr-objnr
                   stat  = c_syst_stat-tec_comp
                   inact = abap_false BINARY SEARCH.
        IF sy-subrc NE 0.
          lv_tech_comp = abap_false.
        ELSE.
          READ TABLE gt_jcds INTO DATA(lwa_jcds)
            WITH KEY objnr = lwa_fmfphdr-objnr
                     stat  = c_syst_stat-tec_comp BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_udate = lwa_jcds-udate.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCREEN_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_update .

  LOOP AT SCREEN.
    IF sy-uname NE 'T_T.KONNO'.
      IF screen-name EQ 'P_REN'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-group1 EQ 'PLN'.
      IF p_plan EQ abap_true.
        screen-active = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF screen-group1 EQ 'NPL'.
      IF p_plan EQ abap_true
      OR p_ren EQ abap_true.
        screen-active = '0'.
        MODIFY SCREEN.
      ELSE.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF screen-group1 EQ 'GEN'.
      IF p_plan EQ abap_true
      OR p_nplan EQ abap_true.
        screen-active = '1'.
        MODIFY SCREEN.
      ELSEIF p_ren EQ abap_true.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF screen-group1 EQ 'REN'.
      IF p_plan EQ abap_true
      OR p_nplan EQ abap_true.
        screen-active = '0'.
        MODIFY SCREEN.
      ELSEIF p_ren EQ abap_true.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

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
  REFRESH gt_to_monitor.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_TASK_ORDER_DATA
*&---------------------------------------------------------------------*
*& Preparing Task Order Monitor Data
*&---------------------------------------------------------------------*
FORM build_task_order_data.

*--Local structure declarations
  DATA: ls_to_monitor TYPE zabs_str_to_monitor_fcat.

*--Local table declarations
  DATA: lt_dd07v      TYPE TABLE OF dd07v.

*--Local variable declarations
  DATA: lv_domvalue   TYPE domvalue_l,
        lv_duration   TYPE i,
        lv_time_elpsd TYPE i.

*--Fetch data from farm process header
  SELECT aufnr,
         autyp,
         tplnr_fl,
         cmnum,
         class,
         matnr,
         gamng,
         gwemg,
         gmein,
         gstrp,
         gltrp
  FROM /agri/fmfphdr
  INTO TABLE @DATA(lt_fmfphdr)
  WHERE matnr     IN @s_matnr
    AND tplnr_fl  IN @s_tplnr
    AND cmnum     IN @s_cmnum
    AND ( ( gstrp GE @s_datum-low
    AND gstrp     LE @s_datum-high )
    OR  ( gltrp   GE @s_datum-low
    AND gltrp     LE @s_datum-high ) )
    AND iwerk     EQ @p_werks
    AND tecom     EQ @space.

  IF sy-subrc NE 0.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ENDIF.

*--Get terrain description
  DATA(lt_tplnr_fl) = lt_fmfphdr.
  SORT lt_tplnr_fl BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_tplnr_fl COMPARING tplnr_fl.
  IF lt_tplnr_fl IS NOT INITIAL.
    SELECT tplnr_fl,
           pltxt
      FROM /agri/glflot
      INTO TABLE @DATA(lt_pltxt)
      FOR ALL ENTRIES IN @lt_tplnr_fl
      WHERE tplnr_fl EQ @lt_tplnr_fl-tplnr_fl.
    IF sy-subrc = 0.
      SORT lt_pltxt BY tplnr_fl.
    ENDIF.
  ENDIF.

*--Get document category description
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = c_domname_doc_cat
    TABLES
      values_tab      = lt_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    SORT lt_dd07v BY domvalue_l.
  ENDIF.

*--Get task material description
  DATA(lt_matnr) = lt_fmfphdr.
  SORT lt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.
  IF lt_matnr IS NOT INITIAL.
    SELECT matnr,
           maktx
      FROM makt
      INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr EQ @lt_matnr-matnr
        AND spras EQ @sy-langu.
    IF sy-subrc = 0.
      SORT lt_makt BY matnr.
    ENDIF.
  ENDIF.

*--Get application text
  SELECT class,
         descr
    FROM /agri/tabclst
    INTO TABLE @DATA(lt_apltxt)
    WHERE spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_apltxt BY class.
  ENDIF.

*--Get order confirmation
  SELECT ocnum
    FROM /agri/fmocindx
    INTO TABLE @DATA(lt_ocnum)
    FOR ALL ENTRIES IN @lt_fmfphdr
    WHERE aufnr EQ @lt_fmfphdr-aufnr.
  IF lt_ocnum IS NOT INITIAL.
*--Get operations based on confirmation
    SELECT ocnum,
           rueck,
           rmzhl
      FROM /agri/fmocopr
      INTO TABLE @DATA(lt_ocopr)
      FOR ALL ENTRIES IN @lt_ocnum
      WHERE ocnum EQ @lt_ocnum-ocnum.

    IF lt_ocopr IS NOT INITIAL.
*--Get posted goods data
      SELECT *
        FROM afwi
        INTO TABLE @DATA(lt_afwi)
        FOR ALL ENTRIES IN @lt_ocopr
        WHERE rueck EQ @lt_ocopr-rueck
          AND rmzhl EQ @lt_ocopr-rmzhl.
      IF sy-subrc EQ 0.
*--Get posting date
        SELECT budat,
               cpudt,
               cputm,
               aufnr
          FROM matdoc
          INTO TABLE @DATA(lt_matdoc)
          FOR ALL ENTRIES IN @lt_afwi
          WHERE mblnr     EQ @lt_afwi-mblnr
            AND mjahr     EQ @lt_afwi-mjahr
            AND zeile     EQ @lt_afwi-mblpo
            AND cancelled EQ @space
            AND bwart     EQ @zcl_abs_abap_maintain=>c_move_type_grn_create.
        IF sy-subrc EQ 0.
          SORT lt_matdoc BY aufnr cpudt DESCENDING cputm DESCENDING.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*--Processing farm process data to build final data.
  LOOP AT lt_fmfphdr INTO DATA(ls_fmfphdr).

    ls_to_monitor-tplnr_fl = ls_fmfphdr-tplnr_fl. "Terrain
    ls_to_monitor-cmnum    = ls_fmfphdr-cmnum.    "Crop
    ls_to_monitor-matnr    = ls_fmfphdr-matnr.    "Process Material
    ls_to_monitor-aufnr    = ls_fmfphdr-aufnr.    "Order
    ls_to_monitor-autyp    = ls_fmfphdr-autyp.    "Doc Category
    ls_to_monitor-class    = ls_fmfphdr-class.    "Application
    ls_to_monitor-gstrp    = ls_fmfphdr-gstrp.    "Basic Start Date
    ls_to_monitor-gltrp    = ls_fmfphdr-gltrp.    "Basic End Date
    ls_to_monitor-gamng    = ls_fmfphdr-gamng.    "Total Quantity
    ls_to_monitor-gmein    = ls_fmfphdr-gmein.    "UOM
    ls_to_monitor-gwemg    = ls_fmfphdr-gwemg.    "Delivered Quantity

*--Posting Date fill
    READ TABLE lt_matdoc INTO DATA(ls_matdoc)
      WITH KEY aufnr = ls_fmfphdr-aufnr
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-lp_date = ls_matdoc-budat.
    ENDIF.

*--Filling terrain description
    READ TABLE lt_pltxt INTO DATA(ls_pltxt)
      WITH KEY tplnr_fl = ls_fmfphdr-tplnr_fl
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-pltxt = ls_pltxt-pltxt.
    ENDIF.

*--Filling description for document category
    lv_domvalue = ls_fmfphdr-autyp.
    READ TABLE lt_dd07v INTO DATA(ls_dd07v)
      WITH KEY domvalue_l = lv_domvalue
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-autyp_desc = ls_dd07v-ddtext.
    ENDIF.

*--Filling description for process material
    READ TABLE lt_makt INTO DATA(ls_makt)
      WITH KEY matnr = ls_fmfphdr-matnr
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-maktx = ls_makt-maktx.
    ENDIF.

*--Filling Description for application
    READ TABLE lt_apltxt INTO DATA(ls_apltxt)
      WITH KEY class = ls_fmfphdr-class
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-class_desc = ls_apltxt-descr.
    ENDIF.

*--Calculating Duration
    CLEAR lv_duration.
    PERFORM calculation_for_duration USING ls_fmfphdr-gstrp
                                           ls_fmfphdr-gltrp
                                  CHANGING lv_duration.

    ls_to_monitor-duration  = lv_duration.
    ls_to_monitor-plan_qty  = ls_fmfphdr-gamng / lv_duration.
    ls_to_monitor-curr_date = sy-datum.

*--Calculating elapsed time from start date to current date.
    CLEAR lv_time_elpsd.
    PERFORM calculation_for_duration USING ls_fmfphdr-gstrp
                                           sy-datum
                                  CHANGING lv_time_elpsd.
    ls_to_monitor-time_elpsd = lv_time_elpsd.

    ls_to_monitor-telpsd_per = ls_to_monitor-time_elpsd * 100 / lv_duration.

    IF ls_to_monitor-time_elpsd GT ls_to_monitor-duration.
      ls_to_monitor-act_qty    = ls_fmfphdr-gwemg / ls_to_monitor-duration.
    ELSE.
      ls_to_monitor-act_qty    = ls_fmfphdr-gwemg / ls_to_monitor-time_elpsd.
    ENDIF.

    ls_to_monitor-advance  = ls_fmfphdr-gwemg * 100 / ls_fmfphdr-gamng.

    APPEND ls_to_monitor TO gt_to_monitor.
    CLEAR :ls_to_monitor,ls_matdoc,ls_pltxt,ls_dd07v,ls_makt,ls_apltxt.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATION_FOR_DURATION
*&---------------------------------------------------------------------*
*& Calculating number of days
*&---------------------------------------------------------------------*
FORM calculation_for_duration  USING    ps_start_date TYPE d
                                        ps_end_date   TYPE d
                               CHANGING pv_days       TYPE i.

*--Local data declaration
  DATA: lt_holidays TYPE TABLE OF iscal_day.

*--Calling Function to get holidays betwwn date range
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar           = zcl_abs_abap_maintain=>c_holi_cal_brazil
      factory_calendar           = zcl_abs_abap_maintain=>c_fact_cal_brazil
      date_from                  = ps_start_date
      date_to                    = ps_end_date
    TABLES
      holidays                   = lt_holidays
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.
  IF sy-subrc = 0.
*--Collecting number of holidays
    DESCRIBE TABLE lt_holidays LINES pv_days.
    pv_days = ( ps_end_date - ps_start_date ) - pv_days + 1.
  ENDIF.

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

*--Plant Validation
  SELECT SINGLE werks
    FROM t001w
    INTO p_werks
    WHERE werks = p_werks.
  IF sy-subrc <> 0.
*...Informe Centro Válido
    MESSAGE e033(zabs_msgcls).
  ENDIF.

*--Crop Validation
  IF s_cmnum IS NOT INITIAL.
    SELECT SINGLE cmnum
      FROM /agri/glcmhdr
      INTO s_cmnum-low
      WHERE cmnum EQ s_cmnum-low.
    IF sy-subrc <> 0.
*.....Informe Mestre de Cultura Válido
      MESSAGE e034(zabs_msgcls).
    ENDIF.
  ENDIF.

  IF s_datum-low IS INITIAL.
*...Insira a data de início
    MESSAGE e031(zabs_msgcls).
  ENDIF.

  IF s_datum-high IS INITIAL.
*...Insira a data final
    MESSAGE e032(zabs_msgcls).
  ENDIF.

ENDFORM.
