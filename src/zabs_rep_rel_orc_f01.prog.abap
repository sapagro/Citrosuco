*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_STORDENS_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_global_data .

  REFRESH: gt_orcamento,
           gt_message,
           gt_fieldcat,
           gt_outtab.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_data .

  DATA: lt_total TYPE STANDARD TABLE OF
                 zabs_str_orcamento_fcat INITIAL SIZE 0,
        ls_total LIKE LINE OF lt_total.

  FIELD-SYMBOLS: <ls_total> LIKE LINE OF lt_total.

  REFRESH gt_orcamento.

*--Preparing Budget's Data
  SELECT o~acnum, o~extwg, o~matkl, o~rcnum,
         o~matnr, o~period, o~fazenda,
         o~kostl, o~aarea_form, o~aarea_manu, o~passadas,
         o~rcdos, o~custo, o~produtos, r~werks,
         m~bklas, t~bkbez
    FROM zabs_orcamento AS o
    INNER JOIN zfmrchdr AS r
    ON o~rcnum EQ r~rcnum
    LEFT OUTER JOIN mbew AS m
    ON o~matnr EQ m~matnr
    AND r~werks EQ m~bwkey
    LEFT OUTER JOIN t025t AS t
    ON t~spras EQ @sy-langu
    AND m~bklas EQ t~bklas
    INTO TABLE @DATA(lt_orcamento)
   WHERE o~acnum   IN @s_acnum[]
     AND o~extwg   IN @s_extwg[]
     AND o~matkl   IN @s_matkl[]
     AND o~rcnum   IN @s_rcnum[]
     AND o~matnr   IN @s_matnr[]
     AND o~period  IN @s_period[]
     AND o~fazenda IN @s_faz[]
     AND o~versao  EQ @p_versao.

  DATA(lt_budget) = lt_orcamento[].
  SORT lt_budget BY period ASCENDING.
  READ TABLE lt_budget INTO DATA(ls_budget) INDEX 1.
  IF sy-subrc EQ 0.
    DATA(lv_low) = ls_budget-period(4).
    DATA(lv_high) = lv_low + 1.
  ENDIF.

  LOOP AT lt_orcamento INTO DATA(ls_orcamento).
    INSERT INITIAL LINE INTO TABLE gt_orcamento
      ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
    IF sy-subrc EQ 0.
      <ls_orcamento>-versao = p_versao.
      <ls_orcamento>-versao_custo = p_vcusto.
      <ls_orcamento>-acnum = ls_orcamento-acnum.
      <ls_orcamento>-extwg = ls_orcamento-extwg.
      <ls_orcamento>-matkl = ls_orcamento-matkl.
      <ls_orcamento>-rcnum = ls_orcamento-rcnum.
      <ls_orcamento>-matnr = ls_orcamento-matnr.
      <ls_orcamento>-matnr18 = ls_orcamento-matnr.
      <ls_orcamento>-bklas = ls_orcamento-bklas.
      <ls_orcamento>-bkbez = ls_orcamento-bkbez.
      <ls_orcamento>-period = ls_orcamento-period.
      <ls_orcamento>-period6 = <ls_orcamento>-period.
      <ls_orcamento>-budget = lv_low && lv_high.
      <ls_orcamento>-fazenda = ls_orcamento-fazenda.
      <ls_orcamento>-kostl = ls_orcamento-kostl.
      <ls_orcamento>-kostl10 = ls_orcamento-kostl.
      <ls_orcamento>-aarea_form = ls_orcamento-aarea_form.
      <ls_orcamento>-aarea_manu = ls_orcamento-aarea_manu.
      <ls_orcamento>-passadas = ls_orcamento-passadas.
      <ls_orcamento>-rcdos = ls_orcamento-rcdos.
      <ls_orcamento>-custo = ls_orcamento-custo.
      <ls_orcamento>-produtos = ls_orcamento-produtos.

      ls_total-versao = p_versao.
      ls_total-versao_custo = p_vcusto.
      ls_total-acnum = <ls_orcamento>-acnum.
      ls_total-versao = <ls_orcamento>-versao.
      ls_total-bklas = <ls_orcamento>-bklas.
      ls_total-bkbez = <ls_orcamento>-bkbez.
      ls_total-kostl = <ls_orcamento>-kostl.
      ls_total-period = <ls_orcamento>-period.
      ls_total-period6 = <ls_orcamento>-period6.
      ls_total-matnr18 = <ls_orcamento>-matnr18.
      ls_total-kostl10 = <ls_orcamento>-kostl10.
      ls_total-budget = <ls_orcamento>-budget.
      ls_total-matnr = <ls_orcamento>-matnr.
      ls_total-aarea_form = <ls_orcamento>-aarea_form.
      ls_total-aarea_manu = <ls_orcamento>-aarea_manu.
      ls_total-custo = <ls_orcamento>-custo.
      ls_total-produtos = <ls_orcamento>-produtos.
      COLLECT ls_total INTO lt_total.
    ENDIF.
  ENDLOOP.

  IF p_agr EQ abap_true.
    gt_orcamento[] = lt_total[].
  ENDIF.

  IF gt_orcamento[] IS INITIAL.
*--Não existem registros os parâmetros informados!
    MESSAGE i209(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_orcamento BY acnum period fazenda matkl matnr.
    gt_outtab[] = gt_orcamento[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  DATA: ls_variant TYPE disvariant.

  IF s_acnum[] IS INITIAL.
*--O parâmetro 'Área de Cultivo' é de preenchimento obrigatório!
    MESSAGE i270(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_period[] IS INITIAL.
*--O parâmetro 'Período' é de preenchimento obrigatório!
    MESSAGE i271(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_agr EQ abap_true
  AND p_vcusto IS INITIAL.
*--O parâmetro 'Versão Impressão' é de preenchimento obrigatório!
    MESSAGE i272(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_versao IS INITIAL.
*--O parâmetro 'Versão Orçamento/Cenário' é de preenchimento obrigatório!
    MESSAGE i313(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_var IS NOT INITIAL.
    ls_variant-variant = p_var.
    ls_variant-report  = sy-repid.

    CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK' "Check for display variant
      EXPORTING
        i_save        = space            "Variants Can be Saved
      CHANGING
        cs_variant    = ls_variant       "Variant information
      EXCEPTIONS
        wrong_input   = 1                "Inconsistent input parameters
        not_found     = 2                "Variant not found
        program_error = 3.               "Program Errors

    IF sy-subrc = 2.
*--O Layout informado &1 não existe!
      MESSAGE i273(zfmfp) WITH p_var.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  SET PF-STATUS 'MAIN100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  SET TITLEBAR 'MAIN100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.

  DATA: lt_fcat    TYPE slis_t_fieldcat_alv,
        lt_buttons TYPE ui_functions,
        ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant.

  PERFORM build_fcat CHANGING lt_fcat.

  PERFORM define_buttons CHANGING lt_buttons.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = g_custom_container.

*--Set ALV attributes FOR LAYOUT
    ls_layout-cwidth_opt = abap_true.
    ls_layout-zebra      = abap_true.
    ls_variant-variant   = p_var.
    ls_variant-report    = sy-repid.

*--Displaying ALV Data
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZABS_STR_ORCAMENTO_FCAT'
      CHANGING
        it_outtab                     = gt_outtab
        it_fieldcatalog               = gt_fieldcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat CHANGING lt_fcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_ORCAMENTO_FCAT'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CASE <ls_fcat>-fieldname.
        WHEN '.INCLUDE'.
          <ls_fcat>-no_out = c_true.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fcat
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat[]
    TABLES
      it_data         = gt_outtab[]
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>)
      WITH KEY fieldname = 'ZBADGE'.
    IF sy-subrc EQ 0.
      <ls_fieldcat>-reptext = 'Número do Crachá'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DEFINE_BUTTONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_BUTTONS
*&---------------------------------------------------------------------*
FORM define_buttons CHANGING lt_buttons TYPE ui_functions.

  DATA: ls_button TYPE ui_func.

*--Toolbar Button CHECK
  ls_button = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button REFRESH
  ls_button = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button UNDO
  ls_button = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button PRINT
*  ls_button = cl_gui_alv_grid=>mc_fc_print_back.
*  APPEND ls_button TO lt_buttons.
*--Toolbar Button Print Preview
*  ls_button = cl_gui_alv_grid=>mc_fc_print_prev.
*  APPEND ls_button TO lt_buttons.
*--Menu Button PASTE add Menu Item PASTE NEW ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button INSERT ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button DELETE ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button COPY ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button APPEND ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button CUT
  ls_button = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button INFORMATION
  ls_button = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_button TO lt_buttons.
*--Menu Button SUBTOTAL
*  ls_button = cl_gui_alv_grid=>mc_mb_subtot.
*  APPEND ls_button TO lt_buttons.
*--Menu Button SUM add Menu Item SUM
*  ls_button = cl_gui_alv_grid=>mc_fc_sum.
*  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_mb_view.
  APPEND ls_button TO lt_buttons.
*  ls_button = cl_gui_alv_grid=>mc_mb_sum.
*  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_button TO lt_buttons.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_BUDGET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_budget_data .

  CALL SCREEN 100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form F_INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_initialize_variant .

  DATA: ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = ls_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_var = ls_variant-variant.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_variant .

  DATA: ls_variant     TYPE disvariant,
        ls_new_variant TYPE disvariant,
        lv_exit        TYPE abap_bool.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = lv_exit
      es_variant = ls_new_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF lv_exit = space.
      p_var = ls_new_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_PERIOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ABAP_TRUE
*&---------------------------------------------------------------------*
FORM f4_for_period USING lv_low TYPE abap_bool.

  IF lv_low EQ abap_true.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = sy-datum(6)
      IMPORTING
        selected_month             = s_period-low
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.
  ELSE.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = sy-datum(6)
      IMPORTING
        selected_month             = s_period-high
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_TERRAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ABAP_TRUE
*&---------------------------------------------------------------------*
FORM f4_for_terrain USING lv_low TYPE abap_bool.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  SELECT tplnr_fl, pltxt
    FROM /agri/glflot
    INTO TABLE @DATA(lt_terrains)
   WHERE fltyp = '2'
     AND tplvl = '2'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'S_FAZ'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_terrains
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    IF sy-subrc = 0.
      READ TABLE lt_terrains INTO DATA(ls_terrain)
        WITH KEY pltxt = ls_return-fieldval.
      IF lv_low EQ abap_true.
        s_faz-low = ls_terrain-tplnr_fl.
      ELSE.
        s_faz-high = ls_terrain-tplnr_fl.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
