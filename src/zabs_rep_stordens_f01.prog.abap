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

  REFRESH: gt_fmfphdr,
           gt_fmfpitm,
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

  DATA: lv_tecom TYPE /agri/fmtecom.

  IF p_tecom EQ abap_true.
    lv_tecom = abap_false.
  ELSEIF p_canc EQ abap_true.
    lv_tecom = abap_true.
  ENDIF.

*--Fetching process order header data
  SELECT aufnr, auart, autyp, tplnr_fl,
         tplma, matnr, cstat, class,
         iwerk, tecom, ernam, erdat
    FROM /agri/fmfphdr
    INTO TABLE @gt_fmfphdr
   WHERE aufnr IN @s_aufnr[]
     AND autyp EQ @c_document_category-task_order
     AND auart IN @s_auart[]
     AND class EQ @c_application-farming
     AND matnr IN @s_matnr[]
     AND iwerk IN @s_iwerk[]
     AND tecom EQ @lv_tecom
     AND ernam IN @s_ernam[]
     AND erdat IN @s_erdat[].

  IF sy-subrc EQ 0.
    gt_outtab[] = gt_fmfphdr[].
*--Process order items
    SELECT *
      FROM /agri/fmfpitm
      INTO TABLE @gt_fmfpitm
      FOR ALL ENTRIES IN @gt_fmfphdr
     WHERE aufnr EQ @gt_fmfphdr-aufnr.
  ENDIF.

  IF gt_fmfpitm[] IS INITIAL.
*--Não existem Ordens para os parâmetros informados!
    MESSAGE i181(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT: gt_fmfphdr BY aufnr,
          gt_fmfpitm BY aufnr posnr.
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

  IF s_iwerk[] IS INITIAL.
*--O parâmetro 'Centro' é de preenchimento obrigatório!
    MESSAGE i178(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_auart[] IS INITIAL.
*--O parâmetro 'Tipo de Ordem' é de preenchimento obrigatório!
    MESSAGE i179(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_matnr[] IS INITIAL.
*--O parâmetro 'Tarefa' é de preenchimento obrigatório!
    MESSAGE i180(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TECHNICALLY_COMPLETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM technically_complete .

  DATA: lt_order      TYPE STANDARD TABLE OF bapi_order_key INITIAL SIZE 0,
        lt_det_return TYPE TABLE OF bapi_order_return,
        ls_return     TYPE bapiret2.

  LOOP AT gt_fmfphdr INTO DATA(ls_fmfphdr).
    REFRESH lt_order.
    INSERT INITIAL LINE INTO TABLE lt_order
      ASSIGNING FIELD-SYMBOL(<ls_order>).
    IF sy-subrc EQ 0.
      <ls_order>-order_number = ls_fmfphdr-aufnr.
      CALL FUNCTION 'ZABS_PRODORD_COMPLETE_TECH'
        IMPORTING
          return        = ls_return
        TABLES
          orders        = lt_order
          detail_return = lt_det_return.

      READ TABLE lt_det_return TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

*--Mensagem Encerramento Técnico Ordem &1
      IF lt_det_return[] IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgty = 'I'.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '184'.
          <ls_message>-msgv1 = ls_fmfphdr-aufnr.
        ENDIF.
      ENDIF.

      LOOP AT lt_det_return INTO DATA(ls_det_return).
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgty = ls_det_return-type.
          <ls_message>-msgid = ls_det_return-id.
          <ls_message>-msgno = ls_det_return-number.
          <ls_message>-msgv1 = ls_det_return-message_v1.
          <ls_message>-msgv2 = ls_det_return-message_v2.
          <ls_message>-msgv3 = ls_det_return-message_v3.
          <ls_message>-msgv4 = ls_det_return-message_v4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZREVOKE_TECHNICAL_COMPLETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zrevoke_technical_complete .

  DATA: lt_order      TYPE STANDARD TABLE OF bapi_order_key INITIAL SIZE 0,
        lt_det_return TYPE TABLE OF bapi_order_return,
        ls_return     TYPE bapiret2.

  LOOP AT gt_fmfphdr INTO DATA(ls_fmfphdr).
    REFRESH lt_order.
    INSERT INITIAL LINE INTO TABLE lt_order
      ASSIGNING FIELD-SYMBOL(<ls_order>).
    IF sy-subrc EQ 0.
      <ls_order>-order_number = ls_fmfphdr-aufnr.
      CALL FUNCTION 'ZABS_PRODORD_REVOKE_TECH'
        IMPORTING
          return        = ls_return
        TABLES
          orders        = lt_order
          detail_return = lt_det_return.

      READ TABLE lt_det_return TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

*--Mensagem Anular Encerramento Técnico Ordem &1
      IF lt_det_return[] IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgty = 'I'.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '185'.
          <ls_message>-msgv1 = ls_fmfphdr-aufnr.
        ENDIF.
      ENDIF.

      LOOP AT lt_det_return INTO DATA(ls_det_return).
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgty = ls_det_return-type.
          <ls_message>-msgid = ls_det_return-id.
          <ls_message>-msgno = ls_det_return-number.
          <ls_message>-msgv1 = ls_det_return-message_v1.
          <ls_message>-msgv2 = ls_det_return-message_v2.
          <ls_message>-msgv3 = ls_det_return-message_v3.
          <ls_message>-msgv4 = ls_det_return-message_v4.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

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

*--Displaying ALV Data

    ls_variant-report = sy-repid.
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZABS_STR_STORDENS'
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
      i_structure_name       = 'ZABS_STR_STORDENS'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
  ls_button = cl_gui_alv_grid=>mc_fc_print_back.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button Print Preview
  ls_button = cl_gui_alv_grid=>mc_fc_print_prev.
  APPEND ls_button TO lt_buttons.
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
  ls_button = cl_gui_alv_grid=>mc_mb_subtot.
  APPEND ls_button TO lt_buttons.
*--Menu Button SUM add Menu Item SUM
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

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ORDER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_order_data .
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
