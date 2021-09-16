*&---------------------------------------------------------------------*
*& Report ZFMFP_SEQ_TALHAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_prodviv.

*&---------------------------------------------------------------------*
*&                     INCLUDES
*&---------------------------------------------------------------------*
INCLUDE <icon>.

*&---------------------------------------------------------------------*
*&                     TABLES
*&---------------------------------------------------------------------*
TABLES: matdoc.

*&---------------------------------------------------------------------*
*&                      TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS: abap.

*&---------------------------------------------------------------------*
*&                      TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_matdoc,
         meins TYPE meins,
         menge TYPE menge_d,
         budat TYPE budat,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         zeile TYPE mblpo,
         matnr TYPE matnr,
         werks TYPE werks_d,
         lgort TYPE lgort_d,
         charg TYPE charg_d,
         bwtar TYPE bwtar_d,
         wempf TYPE wempf,
       END OF ty_matdoc,

       tty_matdoc TYPE STANDARD TABLE OF ty_matdoc INITIAL SIZE 0.

*--Global data declarations
DATA: gt_matdoc          TYPE tty_matdoc,
      gt_fieldcat        TYPE lvc_t_fcat,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      gs_row_id          TYPE lvc_t_roid WITH HEADER LINE,
      gs_row_i           TYPE lvc_s_row,
      gs_col_i           TYPE lvc_s_col,
      gv_error           TYPE abap_bool,
      ok_code            LIKE sy-ucomm.

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_budat FOR matdoc-budat,
                s_matnr FOR matdoc-matnr,
                s_werks FOR matdoc-werks,
                s_lgort FOR matdoc-lgort MATCHCODE OBJECT h_t001l,
                s_charg FOR matdoc-charg MATCHCODE OBJECT h_mcha,
                s_wempf FOR matdoc-wempf.
SELECTION-SCREEN END OF BLOCK b1.

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

    DATA: ls_tab        LIKE LINE OF gt_matdoc,
          lt_fcat       TYPE lvc_t_fcat,
          lt_index_rows TYPE lvc_t_row,
          lv_tabname    TYPE dd03v-tabname,
          lv_fieldname  TYPE dd03v-fieldname,
          lv_help_valu  TYPE help_info-fldvalue,
          lt_bad_cell   TYPE lvc_t_modi,
          lp_wa         TYPE REF TO data.

    FIELD-SYMBOLS: <lv_field_value> TYPE any,
                   <ls_wa>          TYPE any.

    CALL METHOD sender->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = lt_fcat.

    READ TABLE gt_matdoc INDEX es_row_no-row_id INTO ls_tab.
    CREATE DATA lp_wa LIKE LINE OF gt_matdoc.
    ASSIGN lp_wa->* TO <ls_wa>.
    <ls_wa> = ls_tab.

    READ TABLE lt_fcat INTO DATA(ls_fieldcat)
       WITH KEY fieldname = e_fieldname.
    IF sy-subrc EQ 0.
      MOVE ls_fieldcat-ref_table TO lv_tabname.
      MOVE ls_fieldcat-fieldname TO lv_fieldname.
      ASSIGN COMPONENT ls_fieldcat-fieldname
        OF STRUCTURE ls_tab TO <lv_field_value>.
      WRITE <lv_field_value> TO lv_help_valu.
    ENDIF.

  ENDMETHOD.                                                "my_f4

*-------------------------------------------------------------------
  METHOD handle_data_changed_finished .

    CHECK e_modified = 'X'.

    LOOP AT et_good_cells INTO DATA(ls_modi).
      MOVE ls_modi-row_id    TO gs_row_id-row_id.
      MOVE ls_modi-row_id    TO gs_row_i-index.
      MOVE ls_modi-fieldname TO gs_col_i-fieldname.
    ENDLOOP.

    CALL METHOD gr_grid->refresh_table_display.
    CALL METHOD gr_grid->set_current_cell_via_id
      EXPORTING
        is_row_id    = gs_row_i
        is_column_id = gs_col_i.

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

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                           "handle_user_command

ENDCLASS. "lcl_event_handler IMPLEMENTATION

DATA: gr_event_handler TYPE REF TO lcl_event_handler.

INITIALIZATION.
  PERFORM fill_default_values.

START-OF-SELECTION.
  PERFORM validate_param_values.
  PERFORM clear.
  PERFORM select_tables.

END-OF-SELECTION.
  PERFORM show_alv.


*&---------------------------------------------------------------------*
*& Form FILL_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_default_values .

*  DATA: lv_week       TYPE kweek,
*        lv_week_n2    TYPE type_week,
*        lv_begin_date TYPE char10,
*        lv_end_date   TYPE char10,
*        lv_today      TYPE scdatum,
*        lt_yeartab    TYPE tstr_yeartab,
*        lwa_yeartab   TYPE LINE OF tstr_yeartab,
*        lwa_ttstr     TYPE ttstr,
*        lt_dynpfields TYPE dynpread_tabtype,
*        lwa_dynpfield TYPE dynpread,
*        lt_values     TYPE vrm_values,
*        lv_vrm_id     TYPE vrm_id.
*
*  REFRESH: t_periodo.
*
*  IF p_safra IS INITIAL.
*    p_safra = sy-datum(4).
*  ENDIF.
*
*  IF p_sem IS INITIAL.
**...Current week value
*    lv_today = sy-datum.
*    CALL FUNCTION 'DATE_GET_WEEK'
*      EXPORTING
*        date         = lv_today
*      IMPORTING
*        week         = lv_week
*      EXCEPTIONS
*        date_invalid = 1
*        OTHERS       = 2.
*
*    p_sem = lv_week.
*
**...Maximum week value
*    lwa_yeartab = sy-datum+0(4).
*    APPEND lwa_yeartab TO lt_yeartab.
*    ADD 1 TO lwa_yeartab.
*    APPEND lwa_yeartab TO lt_yeartab.
*    CALL FUNCTION 'TSTR_PERIODS_WEEKS'
*      EXPORTING
*        it_yeartab   = lt_yeartab
*        is_ttstr     = lwa_ttstr
*      IMPORTING
*        et_gensegtab = t_periodo
*      EXCEPTIONS
*        error        = 1
*        OTHERS       = 2.
*
*    IF sy-subrc EQ 0.
*      LOOP AT t_periodo INTO DATA(lwa_periodo).
*        CLEAR lv_week_n2.
*        LOOP AT lwa_periodo-gensegtab INTO DATA(lwa_week).
*          ADD 1 TO lv_week_n2.
*          INSERT INITIAL LINE INTO TABLE lt_values
*            ASSIGNING FIELD-SYMBOL(<lwa_value>).
*          IF sy-subrc EQ 0.
*            <lwa_value>-key(2) = lv_week_n2.
*            <lwa_value>-key+2(4) = lwa_periodo-year.
*            <lwa_value>-text = lv_week_n2.
*            WRITE: lwa_week-begin_date USING EDIT MASK '__/__/____' TO lv_begin_date,
*                   lwa_week-end_date USING EDIT MASK '__/__/____' TO lv_end_date.
*            CONCATENATE '(' lv_begin_date '-' lv_end_date ')' INTO <lwa_value>-text.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*
*    lv_vrm_id = 'P_SEM'.
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = lv_vrm_id
*        values          = lt_values
*      EXCEPTIONS
*        id_illegal_name = 1
*        OTHERS          = 2.
*  ENDIF.
*
*  IF p_cl5 IS INITIAL.
*    REFRESH: lt_values.
*    lv_vrm_id = 'P_CL5'.
*    p_cl5 = 'OPT1'.
*
*    DO 4 TIMES.
*      DATA(lv_index) = sy-index.
*      INSERT INITIAL LINE INTO TABLE lt_values
*        ASSIGNING <lwa_value>.
*      IF sy-subrc EQ 0.
*        CASE lv_index.
*          WHEN 1.
*            <lwa_value>-key = 'OPT1'.
**...Ordenação pelo campo sólidos
*            <lwa_value>-text = TEXT-t04.
*          WHEN 2.
*            <lwa_value>-key = 'OPT2'.
**...Ordenação pelo campo taxa sólidos
*            <lwa_value>-text = TEXT-t05.
*          WHEN 3.
*            <lwa_value>-key = 'OPT3'.
**...Ordenação pelo ratio
*            <lwa_value>-text = TEXT-t08.
*          WHEN 4.
*            <lwa_value>-key = 'OPT4'.
**...Ordenação pelo brix
*            <lwa_value>-text = TEXT-t09.
*        ENDCASE.
*      ENDIF.
*    ENDDO.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = lv_vrm_id
*        values          = lt_values
*      EXCEPTIONS
*        id_illegal_name = 1
*        OTHERS          = 2.
*  ENDIF.
*
*  IF p_cl6 IS INITIAL.
*    REFRESH: lt_values.
*    lv_vrm_id = 'P_CL6'.
*    p_cl6 = 'OPT1'.
*
*    DO 4 TIMES.
*      lv_index = sy-index.
*      INSERT INITIAL LINE INTO TABLE lt_values
*        ASSIGNING <lwa_value>.
*      IF sy-subrc EQ 0.
*        CASE lv_index.
*          WHEN 1.
*            <lwa_value>-key = 'OPT1'.
**...Ordenação pelo campo sólidos
*            <lwa_value>-text = TEXT-t04.
*          WHEN 2.
*            <lwa_value>-key = 'OPT2'.
**...Ordenação pelo campo taxa sólidos
*            <lwa_value>-text = TEXT-t05.
*          WHEN 3.
*            <lwa_value>-key = 'OPT3'.
**...Ordenação pelo ratio
*            <lwa_value>-text = TEXT-t08.
*          WHEN 4.
*            <lwa_value>-key = 'OPT4'.
**...Ordenação pelo brix
*            <lwa_value>-text = TEXT-t09.
*        ENDCASE.
*      ENDIF.
*    ENDDO.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = lv_vrm_id
*        values          = lt_values
*      EXCEPTIONS
*        id_illegal_name = 1
*        OTHERS          = 2.
*  ENDIF.

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

  CLEAR gv_error.

  IF s_budat[] IS INITIAL.
*-- Parâmetro obrigatório: Informe a Data de lançamento!
    MESSAGE i099(zfmfp).
    gv_error = abap_true.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF s_matnr[] IS INITIAL.
**-- Parâmetro obrigatório: Informe o Material!
*    MESSAGE i100(zfmfp).
*    gv_error = abap_true.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  IF s_werks[] IS INITIAL.
*-- Parâmetro obrigatório: Informe o Centro!
    MESSAGE i101(zfmfp).
    gv_error = abap_true.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT s_charg ASSIGNING FIELD-SYMBOL(<ls_charg>).
    TRANSLATE: <ls_charg>-low  TO UPPER CASE,
               <ls_charg>-high TO UPPER CASE.
  ENDLOOP.

  LOOP AT s_wempf ASSIGNING FIELD-SYMBOL(<ls_wempf>).
    TRANSLATE: <ls_wempf>-low  TO UPPER CASE,
               <ls_wempf>-high TO UPPER CASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_TABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_tables.

  REFRESH gt_matdoc.
  SELECT meins, menge, budat, mblnr, mjahr,
         zeile, matnr, werks, lgort, charg,
         bwtar, wempf
    FROM matdoc
    INTO TABLE @DATA(lt_matdoc)
   WHERE budat IN @s_budat[]
     AND matnr IN @s_matnr[]
     AND werks IN @s_werks[]
     AND lgort IN @s_lgort[]
     AND charg IN @s_charg[]
     AND wempf IN @s_wempf[].

  gt_matdoc[] = lt_matdoc[].

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

  REFRESH gt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_NURSERY'
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
      et_fieldcat_lvc = gt_fieldcat[]
    TABLES
      it_data         = gt_matdoc[]
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

  gs_layout-zebra = abap_true.
  gs_layout-smalltitle = abap_true.
  gs_layout-cwidth_opt = abap_true.
*--Allow to select multiple lines
  gs_layout-sel_mode = 'A'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

*--To react on oi_custom_events:
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

  DATA: lt_buttons TYPE ui_functions,
        ls_button  TYPE ui_func.

  SET TITLEBAR 'MAIN100'.
  SET PF-STATUS 'MAIN100'.

  IF g_custom_container IS INITIAL.
*... Toolbar Button CHECK
    ls_button = cl_gui_alv_grid=>mc_fc_check.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button REFRESH
    ls_button = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button UNDO
    ls_button = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button PRINT
    ls_button = cl_gui_alv_grid=>mc_fc_print_back.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button Print Preview
    ls_button = cl_gui_alv_grid=>mc_fc_print_prev.
    APPEND ls_button TO lt_buttons.
*... Menu Button PASTE add Menu Item PASTE NEW ROW
    ls_button = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button INSERT ROW
    ls_button = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button DELETE ROW
    ls_button = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button COPY ROW
    ls_button = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button APPEND ROW
    ls_button = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button CUT
    ls_button = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_button TO lt_buttons.
*... Toolbar Button INFORMATION
    ls_button = cl_gui_alv_grid=>mc_fc_info.
    APPEND ls_button TO lt_buttons.
*--- Menu Button SUBTOTAL
    ls_button = cl_gui_alv_grid=>mc_mb_subtot.
    APPEND ls_button TO lt_buttons.
*... Menu Button SUM add Menu Item SUM
    ls_button = cl_gui_alv_grid=>mc_fc_sum.
    APPEND ls_button TO lt_buttons.
    ls_button = cl_gui_alv_grid=>mc_mb_view.
    APPEND ls_button TO lt_buttons.
    ls_button = cl_gui_alv_grid=>mc_mb_sum.
    APPEND ls_button TO lt_buttons.
    ls_button = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND ls_button TO lt_buttons.
    ls_button = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND ls_button TO lt_buttons.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = g_custom_container.

    gs_variant-report = sy-repid.
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
        i_save                        = 'A'
        is_layout                     = gs_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZABS_STR_NURSERY'
      CHANGING
        it_outtab                     = gt_matdoc
        it_fieldcatalog               = gt_fieldcat[]
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
FORM clear.

  REFRESH: gt_matdoc, gt_fieldcat.
  CLEAR: gs_layout, gs_variant, gv_error.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit_program.

  SET SCREEN '0'.
  LEAVE SCREEN.

ENDFORM.
