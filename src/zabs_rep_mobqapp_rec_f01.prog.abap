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

  REFRESH: gt_mobqapp_rec,
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

*--Preparing Mobility's Inspection Lot Data
  SELECT *
    FROM zabs_mobqapp_rec
    INTO TABLE @gt_mobqapp_rec
   WHERE erdat IN @s_erdat[]
     AND erzet IN @s_erzet[].

  DELETE gt_mobqapp_rec WHERE ernam NE p_ernam.

  IF p_imei IS NOT INITIAL.
    DELETE gt_mobqapp_rec WHERE zzimei1 NE p_imei.
  ENDIF.

  IF p_badge IS NOT INITIAL.
    DELETE gt_mobqapp_rec WHERE zbadge NE p_badge.
  ENDIF.

  IF gt_mobqapp_rec[] IS INITIAL.
*--Não existem registros os parâmetros informados!
    MESSAGE i209(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_mobqapp_rec BY prueflos vorglfnr probenr.
    gt_outtab[] = gt_mobqapp_rec[].
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

  IF p_ernam IS INITIAL.
*--O parâmetro 'Criado por' é de preenchimento obrigatório!
    MESSAGE i207(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_erdat[] IS INITIAL.
*--O parâmetro 'Data de Criação' é de preenchimento obrigatório!
    MESSAGE i208(zfmfp).
    LEAVE LIST-PROCESSING.
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

*--Displaying ALV Data

    ls_variant-report = sy-repid.
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZABS_MOBQAPP_REC'
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
      i_structure_name       = 'ZABS_MOBQAPP_REC'
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
*& Form DISPLAY_INSPECTION_LOT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_inspection_lot_data .
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
