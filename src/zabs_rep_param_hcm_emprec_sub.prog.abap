************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PARAM_HCM_EMPREC                           *
* Tcode          : ZABS_PARAM_HCMEMP                                   *
* Created By     : Adonis Rossato                                      *
* Requested by   : Mauricio J. Pereira                                 *
* Created on     : 12.07.2021                                          *
* Description    : Maitenance for ZABS_EMP_HCM_UPD parameter HCM table *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH gt_outtab.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_DATA
*&---------------------------------------------------------------------*
*& FETCH_DATA
*&---------------------------------------------------------------------*
FORM fetch_data.


  REFRESH: gt_outtab.
  CLEAR gv_upd.
  SELECT * FROM zabs_emp_hcm_upd
    INTO CORRESPONDING FIELDS OF TABLE gt_outtab
    WHERE fazenda_farm IN s_farm
      AND arbpl    IN s_arbpl
      AND kostl    IN s_kostl
      AND orgeh    IN s_orgeh
      AND stell    IN s_stell.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*& DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM display_output.


*... §2 create an ALV table
*    §2.2 just create an instance and do not set LIST_DISPLAY for
*         displaying the data as a Fullscreen Grid
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_outtab ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*... §3 Functions
*... §3.1 activate ALV generic Functions
*... §3.2 include own functions by setting own status
  gr_table->set_screen_status(
    pfstatus      =  'ZSALV_STANDARD'
    report        =  sy-repid
    set_functions = gr_table->c_functions_all ).

*... set the columns technical
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column.
*        lr_column  TYPE REF TO cl_salv_column_table.


  DATA: lo_columns TYPE REF TO cl_salv_columns,
        lo_column  TYPE REF TO cl_salv_column_list.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( gc_true ).

  TRY.
      lr_column = lr_columns->get_column( 'MANDT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  lo_columns = gr_table->get_columns( ).
  TRY.
      lo_column ?= lo_columns->get_column( 'STATUS' ).
      lo_column->set_long_text( 'Status' ).
      lo_column->set_medium_text( 'Status' ).
      lo_column->set_short_text( 'Status' ).
      lo_column->set_icon( 'X' ).
      lo_column->set_output_length( 10 ).

    CATCH cx_salv_not_found.
  ENDTRY.
*... §6 register to the events of cl_salv_table
  DATA: lr_events TYPE REF TO cl_salv_events_table.

  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.
**... §6.2 register to the event DOUBLE_CLICK
*  set handler gr_events->on_double_click for lr_events.
**... §6.3 register to the event LINK_CLICK
*  set handler gr_events->on_link_click for lr_events.
  DATA(event_handler) = NEW lcl_event_handler( ).
  SET HANDLER event_handler->on_before_command FOR ALL INSTANCES.

*... §7 selections
  DATA: lr_selections TYPE REF TO cl_salv_selections,
        lt_rows       TYPE salv_t_row,
        lt_column     TYPE salv_t_column,
        ls_cell       TYPE salv_s_cell.

  lr_selections = gr_table->get_selections( ).

*... §7.1 set selection mode
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).


  gr_table->display( ).

ENDFORM.  "display_output

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  DATA: lv_answer.

  CASE i_ucomm.
    WHEN 'SAVE'.
      CLEAR lv_answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Salvar'
          diagnose_object       = ' '
          text_question         = 'Salvar Alterações?'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF lv_answer = '1'."yes

        PERFORM save_changes.
        PERFORM fetch_data.

      ENDIF.


    WHEN 'INSERT'.
      PERFORM new_rows.
    WHEN 'DELETE'.
      PERFORM delete_selected USING '1'.
    WHEN 'UNDO_DEL'.
      PERFORM delete_selected USING '2'.
  ENDCASE.


  gr_table->refresh( ).

ENDFORM.                    " handle_user_command


*&---------------------------------------------------------------------*
*& Form SAVE_CHANGES
*&---------------------------------------------------------------------*
*& SAVE_CHANGES
*&---------------------------------------------------------------------*
FORM save_changes.


  DATA lt_outtab_db TYPE STANDARD TABLE OF zabs_emp_hcm_upd.
  DATA lt_outtab    LIKE gt_outtab.

  gv_upd = abap_true.

*delete
  lt_outtab = gt_outtab.
  DELETE lt_outtab WHERE status <> icon_delete_row.
  lt_outtab_db = CORRESPONDING #( lt_outtab ).

  IF lt_outtab_db[] IS NOT INITIAL.

    DELETE zabs_emp_hcm_upd FROM TABLE lt_outtab_db.
*    IF sy-subrc NE 0.
*      MESSAGE e000(0k) WITH 'Erro ao excluir registros'.
*      EXIT.
*    ENDIF.
    COMMIT WORK.
  ENDIF.


*Update
  lt_outtab = gt_outtab.
  DELETE lt_outtab WHERE status <> icon_insert_row.
  lt_outtab_db = CORRESPONDING #( lt_outtab ).

  IF lt_outtab_db[] IS NOT INITIAL.

    MODIFY zabs_emp_hcm_upd FROM TABLE lt_outtab_db.
    IF sy-subrc NE 0.
      MESSAGE e000(0k) WITH 'Erro ao incluir registros'.
      EXIT.
    ENDIF.
    COMMIT WORK.
  ENDIF.

  MESSAGE s000(0k) WITH 'Registros Atualizados com sucesso'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form NEW_ROWS
*&---------------------------------------------------------------------*
*& NEW_ROWS
*&---------------------------------------------------------------------*
FORM new_rows.


  DATA: lt_hcm_upd TYPE STANDARD TABLE OF zabs_emp_hcm_upd,
        lw_outtab  LIKE LINE OF gt_outtab.

  gv_upd = abap_true.

  " Call of the function module
  CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
    EXPORTING
      header            = 'Novas Entradas'
      tabname           = 'ZABS_EMP_HCM_UPD'
      no_button         = abap_false
      "X_END             = 55
    TABLES
      table             = lt_hcm_upd
    EXCEPTIONS
      no_more_tables    = 1
      too_many_fields   = 2
      nametab_not_valid = 3
      handle_not_valid  = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT lt_hcm_upd INTO DATA(lw_hcm_upd).

    CHECK lw_hcm_upd IS NOT INITIAL.

    MOVE-CORRESPONDING lw_hcm_upd TO lw_outtab.
    lw_outtab-status = icon_insert_row.
    APPEND lw_outtab TO gt_outtab.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_SELECTED
*&---------------------------------------------------------------------*
*& DELETE_SELECTED
*&---------------------------------------------------------------------*
FORM delete_selected USING p_mod.


  DATA: lr_selections TYPE REF TO cl_salv_selections,
        lt_rows       TYPE salv_t_row,
        l_row         TYPE i,
        lw_outtab     LIKE LINE OF gt_outtab.

  lr_selections = gr_table->get_selections( ).
  lt_rows = lr_selections->get_selected_rows( ).

  gv_upd = abap_true.

  LOOP AT lt_rows INTO l_row.


    READ TABLE gt_outtab INTO DATA(lw_out) INDEX l_row.
    IF sy-subrc NE 0.
      CLEAR lw_out.
    ELSEIF lw_out-status = icon_insert_row AND p_mod NE 1.
      CONTINUE.
    ENDIF.

    "Linha Nova
    IF lw_out-status = icon_insert_row AND p_mod = 1.
      lw_outtab-status = 'EXCLUIR'.
*delete
    ELSEIF p_mod = 1.
      lw_outtab-status = icon_delete_row.
*Undo delete
    ELSEIF lw_out-status = icon_delete_row.
      lw_outtab-status = ''.
    ENDIF.

    MODIFY gt_outtab FROM lw_outtab INDEX l_row TRANSPORTING status.

  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE i000(0k) WITH 'Selecione ao menos uma Linha'.
  ENDIF.

  DELETE gt_outtab WHERE status = 'EXCLUIR'.

ENDFORM.
