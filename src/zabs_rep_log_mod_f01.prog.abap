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
                 zabs_str_yorcamento_fcat INITIAL SIZE 0,
        ls_total LIKE LINE OF lt_total.

  FIELD-SYMBOLS: <ls_total> LIKE LINE OF lt_total.

  REFRESH: gt_orcamento, gt_outtab.

*--Preparing Budget's Data
  SELECT *
    FROM zabs_yorcamento
    INTO TABLE @DATA(lt_yorcamento)
   WHERE acnum  EQ @p_acnum
     AND rcnum  IN @s_rcnum[]
     AND period IN @s_period[]
     AND hora   IN @s_time[].

  IF p_date IS NOT INITIAL.
    DELETE lt_yorcamento WHERE data_log NE p_date.
  ENDIF.

  IF p_user IS NOT INITIAL.
    DELETE lt_yorcamento WHERE usuario NE p_user.
  ENDIF.

  IF lt_yorcamento[] IS INITIAL.
*--Não existem registros os parâmetros informados!
    MESSAGE i209(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_orcamento BY acnum period fazenda matkl matnr.
    gt_orcamento = CORRESPONDING #( lt_yorcamento ).
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

  IF p_acnum IS INITIAL.
*--O parâmetro 'Área de Cultivo' é de preenchimento obrigatório!
    MESSAGE i270(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_rcnum[] IS INITIAL.
*--O parâmetro 'Receita' é de preenchimento obrigatório!
    MESSAGE i312(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_period[] IS INITIAL.
*--O parâmetro 'Período' é de preenchimento obrigatório!
    MESSAGE i271(zfmfp).
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
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat.

*--Field catalog prepare
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_YORCAMENTO_FCAT'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
*& Form F4_FOR_VERSION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f4_for_version USING lv_version TYPE fieldname .

  TYPES: BEGIN OF tty_versao,
           versao TYPE zabs_del_ver_orc,
         END OF tty_versao.

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        lt_versao TYPE TABLE OF tty_versao.

  lt_dynpfields-fieldname = 'P_ACNUM'.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc EQ 0.
    READ TABLE lt_dynpfields WITH KEY fieldname = 'P_ACNUM'.
    IF sy-subrc EQ 0.
      p_acnum = lt_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.

  IF p_acnum IS INITIAL.
*-- Informe a Área de Cultivo para consultar as versões existentes!
    MESSAGE i304(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
*-- Verifica versões orçamento
    IF lv_version EQ 'P_TVER'.
      SELECT DISTINCT versao
        FROM zabs_yorcamento
        INTO TABLE @lt_versao
       WHERE acnum = @p_acnum.
    ELSEIF lv_version EQ 'P_SVER'.
      SELECT DISTINCT versao_origem
        FROM zabs_yorcamento
        INTO TABLE @lt_versao
       WHERE acnum = @p_acnum.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lv_version
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = lt_versao
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
      IF sy-subrc = 0.
        IF lv_version EQ 'P_SVER'.
          p_sver = ls_return-fieldval.
        ELSEIF lv_version EQ 'P_TVER'.
          p_tver = ls_return-fieldval.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_for_user .

  TYPES: BEGIN OF tty_users,
           bname     TYPE xubname,
           name_text TYPE ad_namtext,
         END OF tty_users.

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        lt_users  TYPE TABLE OF tty_users.

*-- Verifica usuários disponíveis
  SELECT DISTINCT u~bname, c~name_text
    FROM usr01 AS u
    INNER JOIN usr21 AS a
    ON u~bname EQ a~bname
    INNER JOIN adrp AS c
    ON a~persnumber = c~persnumber
    INTO TABLE @lt_users.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'P_USER'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_users
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    IF sy-subrc = 0.
      READ TABLE lt_users INTO DATA(ls_user)
        WITH KEY name_text = ls_return-fieldval.
      IF sy-subrc EQ 0.
        p_user = ls_user-bname.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set .
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Display Controls
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
FORM controls_display .

  DATA: lt_fcat    TYPE slis_t_fieldcat_alv,
        lt_buttons TYPE ui_functions,
        ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant.

  PERFORM build_fcat.

*--Set ALV attributes for layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = 'A'.
  ls_layout-smalltitle = abap_true.
  ls_variant-variant   = p_var.
  ls_variant-report    = sy-repid.

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
    ls_variant-report = sy-repid.
    CALL METHOD gobj_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_outtab
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE i035(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
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

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR: ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

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
*& Form FCODE_RECOVER
*&---------------------------------------------------------------------*
*& For Recover Button
*&---------------------------------------------------------------------*
FORM fcode_recover.

  IF sy-uname EQ 'T_H.KABABE'
  OR sy-uname EQ 'T_T.KONNO'.
    BREAK-POINT.
  ENDIF.

ENDFORM.
