*----------------------------------------------------------------------*
***INCLUDE LZFMRCMF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form PREPARE_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_catalog .

  DATA: lt_fieldcat_slis TYPE slis_t_fieldcat_alv.

  REFRESH: gt_fieldcat_sim.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSC_ZFMRCSIM'
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
      et_fieldcat_lvc = gt_fieldcat_sim[]
    TABLES
      it_data         = gt_similares[]
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

  LOOP AT gt_fieldcat_sim ASSIGNING FIELD-SYMBOL(<lwa_fieldcat_sim>).
    <lwa_fieldcat_sim>-edit = abap_true.
    CASE <lwa_fieldcat_sim>-fieldname.
      WHEN 'STLAL'.
        <lwa_fieldcat_sim>-edit = abap_false.
      WHEN 'MATNR_INS'.
        <lwa_fieldcat_sim>-edit = abap_true.
        <lwa_fieldcat_sim>-f4availabl = abap_true.
        <lwa_fieldcat_sim>-ref_field = 'MATNR'.
        <lwa_fieldcat_sim>-ref_table = 'MARA'.
      WHEN 'MAKTX_INS'.
        <lwa_fieldcat_sim>-edit = abap_false.
        <lwa_fieldcat_sim>-scrtext_s = 'Descrição Insumo'.
        <lwa_fieldcat_sim>-scrtext_m = 'Descrição Insumo'.
        <lwa_fieldcat_sim>-scrtext_l = 'Descrição Insumo'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
      WHEN 'UNITS_INS'.
        <lwa_fieldcat_sim>-edit = abap_true.
        <lwa_fieldcat_sim>-f4availabl = abap_true.
        <lwa_fieldcat_sim>-ref_field = 'MEINS'.
        <lwa_fieldcat_sim>-ref_table = 'MARA'.
        <lwa_fieldcat_sim>-scrtext_s = 'UM Ins.'.
        <lwa_fieldcat_sim>-scrtext_m = 'UM Ins.'.
        <lwa_fieldcat_sim>-scrtext_l = 'UM Ins.'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
        <lwa_fieldcat_sim>-no_out = abap_true.
      WHEN 'RCDOS_INS'.
        <lwa_fieldcat_sim>-scrtext_s = 'Dose Ins.'.
        <lwa_fieldcat_sim>-scrtext_m = 'Dose Ins.'.
        <lwa_fieldcat_sim>-scrtext_l = 'Dose Ins.'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
        <lwa_fieldcat_sim>-no_out = abap_true.
      WHEN 'MATNR_SIM'.
        <lwa_fieldcat_sim>-edit = abap_true.
        <lwa_fieldcat_sim>-f4availabl = abap_true.
        <lwa_fieldcat_sim>-ref_field = 'MATNR'.
        <lwa_fieldcat_sim>-ref_table = 'MARA'.
        <lwa_fieldcat_sim>-scrtext_s = 'Produto Similar'.
        <lwa_fieldcat_sim>-scrtext_m = 'Produto Similar'.
        <lwa_fieldcat_sim>-scrtext_l = 'Produto Similar'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
      WHEN 'MAKTX_SIM'.
        <lwa_fieldcat_sim>-edit = abap_false.
        <lwa_fieldcat_sim>-scrtext_s = 'Descr. Produto Similar'.
        <lwa_fieldcat_sim>-scrtext_m = 'Descr. Produto Similar'.
        <lwa_fieldcat_sim>-scrtext_l = 'Descr. Produto Similar'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
      WHEN 'UNITS_SIM'.
        <lwa_fieldcat_sim>-edit = abap_true.
        <lwa_fieldcat_sim>-f4availabl = abap_true.
        <lwa_fieldcat_sim>-ref_field = 'MEINS'.
        <lwa_fieldcat_sim>-ref_table = 'MARA'.
        <lwa_fieldcat_sim>-scrtext_s = 'UM Prd.Sim.'.
        <lwa_fieldcat_sim>-scrtext_m = 'UM Prd.Sim.'.
        <lwa_fieldcat_sim>-scrtext_l = 'UM Prd.Sim.'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
      WHEN 'RCDOS_SIM'.
        <lwa_fieldcat_sim>-scrtext_s = 'Dose Prd.Sim.'.
        <lwa_fieldcat_sim>-scrtext_m = 'Dose Prd.Sim.'.
        <lwa_fieldcat_sim>-scrtext_l = 'Dose Prd.Sim.'.
        <lwa_fieldcat_sim>-reptext = <lwa_fieldcat_sim>-scrtext_s.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_layout .

  gs_layout_sim-zebra = abap_false.
  gs_layout_sim-smalltitle = abap_true.
  gs_layout_sim-cwidth_opt = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  DATA: lt_f4      TYPE lvc_t_f4,
        lwa_f4     TYPE lvc_s_f4,
        lt_buttons TYPE ui_functions,
        lwa_button TYPE ui_func,
        lv_input   TYPE i.

  IF g_custom_container_sim IS INITIAL.
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
*... Toolbar Button INFORMATION
    lwa_button = cl_gui_alv_grid=>mc_fc_info.
    APPEND lwa_button TO lt_buttons.
*... Menu Button SUBTOTAL
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

    CREATE OBJECT g_custom_container_sim
      EXPORTING
        container_name = g_container_sim.

    CREATE OBJECT gr_grid_sim
      EXPORTING
        i_parent = g_custom_container_sim.

    CALL METHOD gr_grid_sim->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout_sim
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZSC_ZFMRCSIM'
      CHANGING
        it_outtab                     = gt_similares
        it_fieldcatalog               = gt_fieldcat_sim[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT gr_event_handler_sim.
    SET HANDLER:
     gr_event_handler_sim->handle_data_changed_finished FOR gr_grid_sim,
     gr_event_handler_sim->handle_user_command FOR gr_grid_sim,
     gr_event_handler_sim->handle_toolbar FOR gr_grid_sim.

    CALL METHOD gr_grid_sim->set_toolbar_interactive.

    CALL METHOD gr_grid_sim->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD gr_grid_sim->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD cl_gui_control=>set_focus EXPORTING control = gr_grid_sim.

    lwa_f4-fieldname  = 'MATNR_INS'.
    lwa_f4-register   = abap_true.
    lwa_f4-getbefore  = abap_true.
    lwa_f4-chngeafter = abap_true.
    INSERT lwa_f4 INTO TABLE lt_f4.
    lwa_f4-fieldname  = 'MATNR_SIM'.
    INSERT lwa_f4 INTO TABLE lt_f4.

    CALL METHOD gr_grid_sim->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER gr_event_handler_sim->handle_on_f4 FOR gr_grid_sim.
  ELSE.
    CALL METHOD gr_grid_sim->refresh_table_display( ).
  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

* 0: lock edit enabled cells against input
* 1: set edit enabled cells ready for input
  CALL METHOD gr_grid_sim->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_GRID_LINES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_grid_lines .

  DATA: lt_screen      LIKE gt_similares,
        lwa_similar_bd TYPE zfmrcsim,
        lt_empty_lines LIKE gt_similares.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  IF gs_fmrchdr_previous-stlal EQ zsc_fmrchdr-stlal
  AND gs_fmrchdr_previous-rcnum EQ zsc_fmrchdr-rcnum.
    lt_screen[] = gt_similares[].
  ELSE.
    REFRESH gt_similares.
  ENDIF.

  IF gs_rcdoc_infocus-rcnum IS NOT INITIAL.
    DATA(lv_rcnum) = gs_rcdoc_infocus-rcnum.
  ELSE.
    lv_rcnum = zsc_fmrchdr-rcnum.
  ENDIF.

  IF lv_rcnum IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfmrchdr
      INTO @DATA(lwa_zfmrchdr)
     WHERE rcnum = @lv_rcnum.

    SELECT *
      INTO TABLE @DATA(lt_similares_bd)
      FROM zfmrcsim
     WHERE rcnum = @lv_rcnum
       AND stlal = @zsc_fmrchdr-stlal.

    SORT lt_similares_bd BY matnr_ins matnr_sim.

    LOOP AT lt_screen INTO DATA(lwa_screen).
      READ TABLE lt_similares_bd TRANSPORTING NO FIELDS
        WITH KEY rcnum     = zsc_fmrchdr-rcnum
                 stlal     = lwa_screen-stlal
                 matnr_ins = lwa_screen-matnr_ins
                 matnr_sim = lwa_screen-matnr_sim.
      IF sy-subrc NE 0.
        MOVE-CORRESPONDING lwa_screen TO lwa_similar_bd.
        lwa_similar_bd-rcnum = zsc_fmrchdr-rcnum.
        lwa_similar_bd-werks = zsc_fmrchdr-werks.
        lwa_similar_bd-matnr = zsc_fmrchdr-matnr.
        APPEND lwa_similar_bd TO lt_similares_bd.
      ENDIF.
    ENDLOOP.

    gt_similares[] = CORRESPONDING #( lt_similares_bd[] ).
    DELETE gt_similares WHERE stlal NE zsc_fmrchdr-stlal.
    SORT gt_similares BY stlal matnr_ins matnr_sim.
    DELETE ADJACENT DUPLICATES FROM gt_similares COMPARING ALL FIELDS.
  ENDIF.
*  IF gs_variables-document_mode NE c_mode_display.
*  IF gt_similares[] IS INITIAL.
*    lt_empty_lines[] = gt_similares[].
*    LOOP AT lt_empty_lines INTO DATA(lwa_line).
*      IF lwa_line IS NOT INITIAL.
*        DELETE lt_empty_lines INDEX sy-tabix.
*      ENDIF.
*    ENDLOOP.
*
*    IF lines( lt_empty_lines ) LT 10.
*      DATA(lv_add_empty) = 10 - lines( lt_empty_lines ).
*      DO lv_add_empty TIMES.
*        INSERT INITIAL LINE INTO TABLE gt_similares
*          ASSIGNING FIELD-SYMBOL(<lwa_similar>).
*      ENDDO.
*    ENDIF.
*  ENDIF.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_SIMILAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_similar USING lt_rcdoc TYPE zt_fmrc_doc.

  DATA: lt_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 0,
        lwa_dynpfield LIKE LINE OF lt_dynpfields,
        lt_zfmrcsim   TYPE STANDARD TABLE OF zfmrcsim INITIAL SIZE 0,
        lv_matnr      TYPE matnr.

  DATA(lv_save) = abap_true.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  lwa_dynpfield-fieldname = 'ZSC_FMRCHDR-STLAL'.
  APPEND lwa_dynpfield TO lt_dynpfields.

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

  READ TABLE lt_dynpfields INTO lwa_dynpfield
    WITH KEY fieldname = 'ZSC_FMRCHDR-STLAL'.
  IF sy-subrc EQ 0
  AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
    zsc_fmrchdr-stlal = lwa_dynpfield-fieldvalue.
  ENDIF.

  IF zsc_fmrchdr-stlal IS INITIAL.
    zsc_fmrchdr-stlal = gs_rcdoc_infocus-x-rchdr-stlal.
  ENDIF.

  IF gs_rcdoc_infocus-rcnum EQ TEXT-004.
*...Grave a Receita antes de atualizar os produtos similares!
    MESSAGE i038(zfmfp).
  ELSE.
*    IF zsc_fmrchdr-stlal IS INITIAL.
**...Selecione a Lista Técnica Alter. antes de gravar os produtos similares!
*      MESSAGE i040(zfmfp).
*    ELSE.
    lt_zfmrcsim[] = CORRESPONDING #( gt_similares[] ).
    LOOP AT lt_zfmrcsim ASSIGNING FIELD-SYMBOL(<lwa_zfmrcsim>).
      IF <lwa_zfmrcsim> IS INITIAL.
        DELETE lt_zfmrcsim INDEX sy-tabix.
        CONTINUE.
      ENDIF.
      <lwa_zfmrcsim>-rcnum = gs_rcdoc_infocus-rcnum.
      <lwa_zfmrcsim>-matnr = gs_rcdoc_infocus-matnr.
      <lwa_zfmrcsim>-werks = gs_rcdoc_infocus-werks.
      IF <lwa_zfmrcsim>-matnr_ins IS NOT INITIAL.
        <lwa_zfmrcsim>-stlal = zsc_fmrchdr-stlal.
      ENDIF.

      DATA(lv_matnr_error) = abap_false.
      READ TABLE gs_rcdoc_infocus-x-rclst TRANSPORTING NO FIELDS
        WITH KEY matnr_ins = <lwa_zfmrcsim>-matnr_ins.
      IF sy-subrc NE 0.
        IF gs_rcdoc_infocus-x-rclst[] IS INITIAL.
          READ TABLE lt_rcdoc INTO DATA(lwa_rcdoc) INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE lwa_rcdoc-x-rclst TRANSPORTING NO FIELDS
              WITH KEY matnr_ins = <lwa_zfmrcsim>-matnr_ins.
            IF sy-subrc NE 0.
              lv_save = abap_false.
              lv_matnr_error = abap_true.
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                EXPORTING
                  input  = <lwa_zfmrcsim>-matnr_ins
                IMPORTING
                  output = lv_matnr.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          lv_save = abap_false.
          lv_matnr_error = abap_true.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = <lwa_zfmrcsim>-matnr_ins
            IMPORTING
              output = lv_matnr.
          EXIT.
        ENDIF.
      ENDIF.

      DATA(lv_um_error) = abap_false.
*      IF <lwa_zfmrcsim>-rcdos_ins IS INITIAL
*      OR <lwa_zfmrcsim>-units_ins IS INITIAL
*      OR <lwa_zfmrcsim>-rcdos_sim IS INITIAL
*      OR <lwa_zfmrcsim>-units_sim IS INITIAL.
      IF <lwa_zfmrcsim>-rcdos_sim IS INITIAL
      OR <lwa_zfmrcsim>-units_sim IS INITIAL.
        lv_save = abap_false.
        lv_um_error = abap_true.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = <lwa_zfmrcsim>-matnr_ins
          IMPORTING
            output = lv_matnr.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_save EQ abap_true.
      MODIFY zfmrcsim FROM TABLE lt_zfmrcsim.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
*.....Os dados foram atualizados com sucesso!
      MESSAGE s023(zfmfp).
    ELSE.
      IF lv_matnr_error EQ abap_true.
*.......Insumo &1 não utilizado na Receita &2! Revise os dados!
        MESSAGE i039(zfmfp) WITH lv_matnr gs_rcdoc_infocus-rcnum.
      ELSEIF lv_um_error EQ abap_true.
*.......Informe a Dose e a Unidade de Medida para &1 antes de gravar!
        MESSAGE i037(zfmfp) WITH lv_matnr.
      ENDIF.
    ENDIF.
*    ENDIF.
  ENDIF.

ENDFORM.
