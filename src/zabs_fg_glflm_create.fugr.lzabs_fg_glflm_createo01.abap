*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_GLFLM_CREATEO01.
*----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**& Module STATUS_SET OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE status_set OUTPUT.
*  SET PF-STATUS 'S201'.
*ENDMODULE.
**&---------------------------------------------------------------------*
**& Module TITLE_SET OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE title_set OUTPUT.
*  SET TITLEBAR 'T01'.
*ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DROPDOWN_TABLES_DISPLAY_201 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE dropdown_tables_display_201 OUTPUT.

  PERFORM dropdown_tables_display_0201
    IN PROGRAM /agri/saplglflm IF FOUND.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISABLE_BUTTONS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE disable_buttons OUTPUT.

  IF /agri/s_glflot-tplkz EQ 'CROP'.
    DATA(lv_custom) = abap_true.
  ELSE.
    lv_custom = abap_false.
  ENDIF.

*-- Entrada Manual
  IF rb_std EQ abap_true.
    PERFORM f_set_default.
*-- Entrada Automática
  ELSEIF rb_cst EQ abap_true.
    IF lv_custom EQ abap_true.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'GR1'.
          screen-invisible = abap_false.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'GR2'.
          screen-invisible = abap_true.
          screen-active = 0.
        ENDIF.
*-- Ocular/Inibir Opção Talhão
*-- Solicitação Cliente 21/10/20
        IF screen-group1 EQ 'GR3'.
          screen-invisible = abap_true.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ELSE.
      PERFORM f_set_default.
    ENDIF.
  ELSE.
    PERFORM f_set_default.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_HEADER_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_header_data INPUT.

  DATA: lv_tplnr_fl        TYPE /agri/gltplnr_fl,
        lv_fazenda         TYPE char6,
        lv_faz_controle    TYPE char6,
        lv_fazenda_tela    TYPE /agri/gltplma,
        lv_talhao          TYPE char5,
        lv_talhao_controle TYPE char5.

  IF sy-ucomm EQ 'CONT'.
    IF /agri/s_glflot-tplkz IS INITIAL.
*-- Selecione o Código de estrutura de terreno!
      MESSAGE e275(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      IF rb_cst EQ abap_true.
        /agri/s_glflot-strno = gv_faz.
      ENDIF.
      PERFORM header_data_check.
    ENDIF.
  ELSEIF sy-ucomm EQ 'CANC'.
    CLEAR: /agri/s_glflot-tplkz,
           /agri/s_glflot-strno,
           /agri/s_glflscrfields-editm,
           /agri/s_glflscrfields-stufm,
           gv_faz.
  ELSEIF sy-ucomm EQ 'MODE'
     AND rb_std EQ abap_true .
    CLEAR gv_faz.
  ELSEIF sy-ucomm EQ 'TER_CHECK'.
    SELECT MAX( strno )
      FROM /agri/glflot
      INTO @DATA(lv_strno)
     WHERE tplkz EQ @/agri/s_glflot-tplkz
       AND fltyp EQ '1'
       AND tplvl EQ 1
       AND strno NE '999999'
       AND loevm EQ @abap_false.

*    SELECT tplnr_fl, pltxt, strno, loevm
*      FROM /agri/glflot UP TO 50 ROWS
*      INTO TABLE @DATA(lt_glflot)
*     WHERE tplkz EQ @/agri/s_glflot-tplkz
*       AND fltyp EQ '1'
*       AND tplvl EQ 1
*       AND strno GE @lv_strno.
*
*    SORT lt_glflot BY strno ASCENDING.
*    LOOP AT lt_glflot INTO DATA(ls_glflot).
*      IF ls_glflot-loevm EQ abap_false.
*        lv_strno = ls_glflot-strno.
*        ADD 1 TO lv_strno.
*        EXIT.
*      ENDIF.
*    ENDLOOP.

    ADD 1 TO lv_strno.
    CONDENSE lv_strno.
    gv_faz = lv_strno.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = gv_faz
      IMPORTING
        output = gv_faz.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form F_SET_DEFAULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_default .

  LOOP AT SCREEN.
    IF screen-group1 EQ 'GR1'.
      screen-invisible = abap_true.
      screen-active = 0.
    ELSEIF screen-group1 EQ 'GR2'.
      screen-invisible = abap_false.
      screen-active = 1.
    ENDIF.
*-- Ocular/Inibir Opção Talhão
*-- Solicitação Cliente 21/10/20
    IF screen-group1 EQ 'GR3'.
      screen-invisible = abap_true.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VALUE_FAZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_faz INPUT.

  DATA: lt_strno TYPE TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = '/AGRI/S_GLFLOT'
      fieldname         = 'STRNO'
      searchhelp        = '/AGRI/CSH_GLFL'
    TABLES
      return_tab        = lt_strno
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  IF sy-subrc EQ 0.
    READ TABLE lt_strno INTO DATA(ls_strno) INDEX 1.
    IF sy-subrc EQ 0.
      gv_faz = ls_strno-fieldval.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALUE_TAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_tal INPUT.

  DATA: lt_strno2 TYPE TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = '/AGRI/S_GLFLOT'
      fieldname         = 'STRNO'
      searchhelp        = '/AGRI/CSH_GLFL'
    TABLES
      return_tab        = lt_strno2
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

ENDMODULE.
