
*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display .

  IF sy-uname NE 'T_H.KABABE'
  AND sy-uname NE 'T_T.KONNO'.
    LEAVE TO TRANSACTION 'MM03'.
  ENDIF.

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program_acm) IF FOUND.

ENDFORM.                    " CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  controls_display_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0100.

  DATA: lv_view.

  IF ref_worklist_container IS INITIAL.

    CREATE OBJECT ref_worklist_container
      EXPORTING
        repid                       = c_program_acm
        dynnr                       = sy-dynnr
        side                        = cl_gui_docking_container=>dock_at_left
        extension                   = 300
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM worklist_build.

  ENDIF.

  IF gs_variables-refresh_worklist EQ c_true.

    PERFORM worklist_refresh USING lv_view.

    CLEAR gs_variables-refresh_worklist.

  ENDIF.

ENDFORM.                    "controls_display_0100
*&---------------------------------------------------------------------*
*&      Form  CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register .

  DATA : lv_routine(28) VALUE 'CONTROL_EVENTS_REGISTER_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

  PERFORM (lv_routine) IN PROGRAM (c_program_acm) IF FOUND.

ENDFORM.                    " CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0100 .

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  SET HANDLER: ref_event_handler->on_toolbar_wl
               FOR ref_worklist,
               ref_event_handler->on_user_command_wl
               FOR ref_worklist,
               ref_event_handler->on_hotspot_click_wl
               FOR ref_worklist,
               ref_event_handler->on_view_changed
               FOR ref_worklist.

ENDFORM.                    "control_events_register_0100
*&---------------------------------------------------------------------*
*&      Form  CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM changes_confirm  CHANGING lv_answer.

  DATA: lv_data_changed.

  CHECK gs_variables-document_mode NE c_mode_display
    AND NOT gs_acdoc_infocus IS INITIAL.

  IF gs_variables-manual_changes   EQ c_true
  OR gs_variables-data_changed EQ c_true.
    lv_data_changed = c_true.
  ENDIF.

  IF lv_data_changed IS INITIAL.
    CALL FUNCTION 'ZFMAC_CHECKCHANGES_SINGLE'
      EXPORTING
        is_acdoc  = gs_acdoc_infocus
      CHANGING
        c_changed = lv_data_changed
      EXCEPTIONS
        no_change = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  CHECK lv_data_changed EQ c_true OR sy-datar EQ c_true.

  popup_to_confirm TEXT-010 TEXT-011 c_true lv_answer.

  CASE lv_answer.
    WHEN '1'.
      lv_answer = 'A'.
      ok_code   = c_fcode-save.
  ENDCASE.

ENDFORM.                    " CHANGES_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  controls_display_0202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  controls_display_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0301.

  DATA: ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo,
        lv_input   TYPE i,
        lv_title   TYPE lvc_title.

  DATA: ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_items IS INITIAL.

    CREATE OBJECT ref_container_items
      EXPORTING
        container_name              = 'SAPLFMACM_0301_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_items IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_items
      EXPORTING
        i_parent           = ref_container_items
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.


    PERFORM field_catalog_prepare USING c_structure_name-items
                               CHANGING lt_fcat.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_items->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_fmacitm_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR gs_variables-refresh_items_grid.

  ELSEIF gs_variables-refresh_items_grid EQ c_true.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_items
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set  USING ref_grid_items
                        CHANGING gt_fmacitm_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_items->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0301

FORM controls_display_0203.

  DATA: ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo,
        lv_input   TYPE i,
        lv_title   TYPE lvc_title.

  DATA: ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort.

  IF ref_container_vlcl IS INITIAL.

    CREATE OBJECT ref_container_vlcl
      EXPORTING
        container_name              = 'SAPLFMACM_0203_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_vlcl IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_vlcl
      EXPORTING
        i_parent           = ref_container_vlcl
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.


    PERFORM field_catalog_prepare USING c_structure_name-vlcl
                               CHANGING lt_fcat.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-volumen_calda.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare
                  TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_vlcl->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_fmacvlc_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR: gs_variables-refresh_vlc_grid.

  ELSEIF gs_variables-refresh_vlc_grid EQ c_true.

    CLEAR gs_variables-refresh_vlc_grid.

    PERFORM grid_data_get_vlc USING ref_grid_vlcl
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set_vlc  USING ref_grid_vlcl
                        CHANGING gt_fmacvlc_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_vlcl->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0203

*&---------------------------------------------------------------------*
*&      Form  controls_display_0308
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0308.

  DATA: lt_dates            TYPE /scwm/tt_lm_dates,
        lt_sort             TYPE lvc_t_sort,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_orcamento        TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lrt_period          TYPE RANGE OF spmon,
        lt_period           TYPE zabstc_orca_period,
        lt_resumo_orc       TYPE zcl_zfarm_agriplan_mpc=>tt_getresumoorc,
        lrs_period          LIKE LINE OF lrt_period,
        ls_resumo_orc       LIKE LINE OF lt_resumo_orc,
        ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        ls_variant          TYPE disvariant,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i,
        lv_tabixc           TYPE char2,
        lv_field            TYPE fieldname,
        lv_title            TYPE lvc_title,
        lv_matnr_x          TYPE matnr,
        lv_tipo             TYPE char10,
        lv_tfor             TYPE string,
        lv_tman             TYPE string,
        lv_period           TYPE char6.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  REFRESH: gt_resumo_orc_fcat.

  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = gs_acdoc_infocus-x-achdr-datab
      iv_endda = gs_acdoc_infocus-x-achdr-datbi
    IMPORTING
      et_dates = lt_dates.

  lrs_period = 'IEQ'.
  LOOP AT lt_dates INTO DATA(ls_date).
    IF lv_period NE ls_date(6).
      INSERT INITIAL LINE INTO TABLE lt_period
       ASSIGNING FIELD-SYMBOL(<ls_period>).
      IF sy-subrc EQ 0.
        lrs_period-low = <ls_period> = lv_period = ls_date(6).
        APPEND lrs_period  TO lrt_period.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT *
    INTO TABLE @lt_orcamento
    FROM zabs_orcamento
   WHERE acnum  EQ @gs_acdoc_infocus-acnum
     AND extwg  EQ @gv_processo
     AND period IN @lrt_period[].

*-- GETRESUMOORCSET_GET_ENTITYSET
  DO 3 TIMES.
    INSERT INITIAL LINE INTO TABLE gt_resumo_orc_fcat
      ASSIGNING FIELD-SYMBOL(<ls_resumo_orc_fcat>).
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    CASE sy-index.
      WHEN 1.
        ls_resumo_orc =
        zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                            im_ajahr     = gs_acdoc_infocus-x-achdr-ajahr
                                                            im_period    = lt_period
                                                            im_orcamento = lt_orcamento
                                                            im_zfmaitm   = gs_acdoc_infocus-x-acitm ).
        <ls_resumo_orc_fcat>-resumo = ls_resumo_orc-resumo = 'Área Formação'.
      WHEN 2.
        ls_resumo_orc =
        zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                            im_ajahr     = gs_acdoc_infocus-x-achdr-ajahr
                                                            im_period    = lt_period
                                                            im_orcamento = lt_orcamento
                                                            im_zfmaitm   = gs_acdoc_infocus-x-acitm ).
        <ls_resumo_orc_fcat>-resumo = ls_resumo_orc-resumo = 'Área Manutenção'.
      WHEN 3.
        ls_resumo_orc =
        zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                            im_ajahr     = gs_acdoc_infocus-x-achdr-ajahr
                                                            im_period    = lt_period
                                                            im_orcamento = lt_orcamento
                                                            im_zfmaitm   = gs_acdoc_infocus-x-acitm ).
        <ls_resumo_orc_fcat>-resumo = ls_resumo_orc-resumo = 'Custo Total'.
    ENDCASE.

    LOOP AT lt_period INTO DATA(ls_period).
      DATA(lv_tabix) = sy-tabix.
      lv_tabixc = lv_tabix.
      CONCATENATE 'M' lv_tabixc INTO lv_field.
      ASSIGN COMPONENT lv_field OF STRUCTURE <ls_resumo_orc_fcat>
        TO FIELD-SYMBOL(<lv_target>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_resumo_orc
          TO FIELD-SYMBOL(<lv_source>).
        IF sy-subrc EQ 0.
          <lv_target> = <lv_source>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    APPEND ls_resumo_orc TO lt_resumo_orc.
  ENDDO.

  IF ref_container_resumo IS INITIAL.

    CREATE OBJECT ref_container_resumo
      EXPORTING
        container_name              = 'SAPLFMACM_0308_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_resumo IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_resumo
      EXPORTING
        i_parent           = ref_container_resumo
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.

    PERFORM field_catalog_prepare USING c_structure_name-resumo
                               CHANGING lt_fcat.

    LOOP AT lt_period INTO ls_period.
      lv_tabix = sy-tabix.
      lv_tabixc = lv_tabix.
      CONCATENATE 'M' lv_tabixc INTO lv_field.
      READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        <ls_fcat>-reptext   = ls_period+4(2) && '.' && ls_period+0(4).
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_m = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_s = <ls_fcat>-reptext.
      ENDIF.
    ENDLOOP.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_resumo->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_resumo_orc_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CLEAR gs_variables-refresh_items_grid.

  ELSE.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_resumo
                        CHANGING lt_sort[]
                                 lt_fcat[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

    PERFORM grid_data_set  USING ref_grid_resumo
                        CHANGING gt_resumo_orc_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_resumo->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0308

*&---------------------------------------------------------------------*
*&      Form  controls_display_0309
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0309.

  TYPES: BEGIN OF ty_collect_vc,
           period     TYPE spmon,
           aareaform  TYPE zfmacqtb,
           aareamanu  TYPE zfmacqtb,
           bombasform TYPE zfmacqtb,
           bombasmanu TYPE zfmacqtb,
           tipo       TYPE char10,
         END OF ty_collect_vc.

  DATA: lv_tipo TYPE char10.

  TYPES: BEGIN OF ty_collect,
           period TYPE spmon,
           custo  TYPE zfmacqtb,
           tipo   LIKE lv_tipo,
         END OF ty_collect.

  DATA: lt_dates            TYPE /scwm/tt_lm_dates,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort,
        lt_collect          TYPE STANDARD TABLE OF ty_collect INITIAL SIZE 0,
        lt_collect_vc       TYPE TABLE OF ty_collect_vc,
        lt_custoreceita     TYPE zcl_zfarm_agriplan_mpc=>tt_custoreceita,
        lt_period           TYPE zabstc_orca_period,
        lrt_period          TYPE RANGE OF spmon,
        lrt_matnr           TYPE RANGE OF matnr,
        lrs_period          LIKE LINE OF lrt_period,
        ls_collect_vc       TYPE ty_collect_vc,
        ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        ls_collect          LIKE LINE OF lt_collect,
        ls_period           LIKE LINE OF lt_period,
        ls_variant          TYPE disvariant,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i,
        lv_tabixc           TYPE char2,
        lv_field            TYPE fieldname,
        lv_title            TYPE lvc_title,
        lv_total            TYPE zfmacren,
        lv_begda_bis        TYPE begda,
        lv_matnr_x          TYPE matnr,
        lv_months           TYPE string,
        lv_tfor             TYPE string,
        lv_tman             TYPE string,
        lv_period           TYPE char6,
        lv_string           TYPE string.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  REFRESH gt_custo_fcat.

  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = gs_acdoc_infocus-x-achdr-datab
      iv_endda = gs_acdoc_infocus-x-achdr-datbi
    IMPORTING
      et_dates = lt_dates.

  lrs_period = 'IEQ'.
  LOOP AT lt_dates INTO DATA(ls_date).
    IF lv_period NE ls_date(6).
      INSERT INITIAL LINE INTO TABLE lt_period
       ASSIGNING FIELD-SYMBOL(<ls_period>).
      IF sy-subrc EQ 0.
        lrs_period-low = <ls_period> = lv_period = ls_date(6).
        APPEND lrs_period  TO lrt_period.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-- CUSTORECEITASET_GET_ENTITYSET
  SELECT rctyp, orcamento
    INTO TABLE @DATA(lt_rctyp)
    FROM ztfmrctyp
   WHERE orcamento EQ @abap_true.

  IF sy-subrc EQ 0.
    lv_matnr_x = 'TMAN' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr
      ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    lv_matnr_x = 'TFOR' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_rchdr)
      FROM zfmrchdr
      FOR ALL ENTRIES IN @lt_rctyp
     WHERE werks EQ @gs_acdoc_infocus-x-achdr-werks
       AND matnr IN @lrt_matnr[]
       AND datuv GE @gs_acdoc_infocus-x-achdr-datab
       AND rctyp EQ @lt_rctyp-rctyp
       AND datbi LE @gs_acdoc_infocus-x-achdr-datbi.

    DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                      AND matnr(4) NE 'TMAN'.

    IF lt_rchdr[] IS NOT INITIAL.
      SORT lt_rchdr BY rcnum.

      LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
        IF <ls_rchdr>-matnr(4) EQ 'TFOR'.
          <ls_rchdr>-text1 = lc_tipo-formacao.
        ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
          <ls_rchdr>-text1 = lc_tipo-manutencao.
        ENDIF.
      ENDLOOP.

      SELECT *
        INTO TABLE @DATA(lt_orcamento)
        FROM zabs_orcamento
        FOR ALL ENTRIES IN @lt_rchdr
       WHERE rcnum  EQ @lt_rchdr-rcnum
         AND period IN @lrt_period[].

      SORT lt_orcamento BY acnum rcnum period.
    ENDIF.
  ENDIF.

  IF lt_orcamento[] IS NOT INITIAL
  AND lt_rchdr[] IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(lt_zfmacvlcl)
      FROM zfmacvlcl
     WHERE acnum EQ @gs_acdoc_infocus-x-achdr-acnum.

    SORT lt_zfmacvlcl BY tplnr_fl matnr.

    SELECT *
      INTO TABLE @DATA(lt_glflcma)
      FROM /agri/glflcma
      FOR ALL ENTRIES IN @lt_zfmacvlcl
     WHERE tplnr_fl EQ @lt_zfmacvlcl-tplnr_fl.

    IF sy-subrc EQ 0.
      DELETE lt_glflcma WHERE astat NE c_crop_season_status-active
                           OR loevm EQ abap_true.
      SORT lt_glflcma BY tplnr_fl ASCENDING
                         contr    DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING tplnr_fl.

      lv_tman = 'TMAN' && gv_tarefa+4(36).
      lv_tfor = 'TFOR' && gv_tarefa+4(36).

      LOOP AT lt_period INTO ls_period.
        CONCATENATE ls_period '01' INTO lv_begda_bis.

        LOOP AT lt_glflcma INTO DATA(ls_glflcma).
          CLEAR lv_months.
          IF ls_glflcma-datab_ref IS NOT INITIAL.
            CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
              EXPORTING
                i_datum_bis = lv_begda_bis
                i_datum_von = ls_glflcma-datab_ref
              IMPORTING
                e_monate    = lv_months.

            CONDENSE lv_months NO-GAPS.
          ENDIF.

          lv_string = lv_tman.
          READ TABLE lt_zfmacvlcl INTO DATA(ls_zfmacvlcl)
            WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                     matnr    = lv_string BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_collect_vc-aareamanu  = ls_zfmacvlcl-acarx.
            ls_collect_vc-bombasmanu = ls_zfmacvlcl-acqtb.
          ENDIF.

          lv_string = lv_tfor.
          READ TABLE lt_zfmacvlcl INTO ls_zfmacvlcl
            WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                     matnr    = lv_string BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_collect_vc-aareaform  = ls_zfmacvlcl-acarx.
            ls_collect_vc-bombasform = ls_zfmacvlcl-acqtb.
          ENDIF.

          ls_collect_vc-period = ls_period.
          COLLECT ls_collect_vc INTO lt_collect_vc.
          CLEAR ls_collect_vc.
        ENDLOOP.
      ENDLOOP.
      SORT lt_collect_vc BY period.
    ENDIF.

    LOOP AT lt_orcamento ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
      READ TABLE lt_rchdr INTO DATA(ls_rchdr)
        WITH KEY rcnum = <ls_orcamento>-rcnum BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_collect-period = <ls_orcamento>-period.
        IF ls_rchdr-matnr(4) EQ 'TFOR'.
          ls_collect-tipo = lc_tipo-formacao.
          READ TABLE lt_collect_vc INTO ls_collect_vc
            WITH KEY period = <ls_orcamento>-period BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR <ls_orcamento>-custo.
          ENDIF.
        ELSEIF ls_rchdr-matnr(4) EQ 'TMAN'.
          ls_collect-tipo = lc_tipo-manutencao.
        ENDIF.
        ls_collect-custo  = <ls_orcamento>-custo.
        COLLECT ls_collect INTO lt_collect.
      ENDIF.
    ENDLOOP.

    SORT lt_collect BY period tipo.
  ENDIF.

  DO 2 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
      WHEN 1.
        lv_tipo = lc_tipo-formacao.
      WHEN 2.
        lv_tipo = lc_tipo-manutencao.
    ENDCASE.

    INSERT INITIAL LINE INTO TABLE gt_custo_fcat
      ASSIGNING FIELD-SYMBOL(<ls_custo_fcat>).
    IF sy-subrc NE 0.
      CONTINUE.
    ELSE.
      <ls_custo_fcat>-resumo = lv_tipo.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE lt_custoreceita
      ASSIGNING FIELD-SYMBOL(<ls_custoreceita>).
    IF sy-subrc EQ 0.
      LOOP AT lrt_period INTO lrs_period.
        DATA(lv_tabix) = sy-tabix.
        lv_tabixc = lv_tabix.
        CLEAR lv_total.
        READ TABLE lt_collect INTO ls_collect
          WITH KEY period = lrs_period-low
                   tipo   = lv_tipo BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_total = ls_collect-custo.
        ENDIF.
        CONCATENATE 'M' lv_tabixc INTO lv_field.
        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_custoreceita>
          TO FIELD-SYMBOL(<lv_field>).
        IF sy-subrc EQ 0.
          <lv_field> = lv_total.
        ENDIF.
        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_custo_fcat>
          TO <lv_field>.
        IF sy-subrc EQ 0.
          <lv_field> = lv_total.
        ENDIF.
      ENDLOOP.
      <ls_custoreceita>-custo = lv_tipo.
    ENDIF.
  ENDDO.

  IF ref_container_cost IS INITIAL.

    CREATE OBJECT ref_container_cost
      EXPORTING
        container_name              = 'SAPLFMACM_0309_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF ref_grid_cost IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_cost
      EXPORTING
        i_parent           = ref_container_cost
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.

    PERFORM field_catalog_prepare USING c_structure_name-custo
                               CHANGING lt_fcat.

    LOOP AT lt_period INTO ls_period.
      lv_tabix = sy-tabix.
      lv_tabixc = lv_tabix.
      CONCATENATE 'M' lv_tabixc INTO lv_field.
      READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        <ls_fcat>-reptext   = ls_period+4(2) && '.' && ls_period+0(4).
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_m = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_s = <ls_fcat>-reptext.
      ENDIF.
    ENDLOOP.

    lv_field = 'RESUMO'.
    READ TABLE lt_fcat ASSIGNING <ls_fcat>
      WITH KEY fieldname = lv_field.
    IF sy-subrc EQ 0.
      <ls_fcat>-reptext   = 'Custo'.
      <ls_fcat>-scrtext_l = 'Custo'.
      <ls_fcat>-scrtext_m = 'Custo'.
      <ls_fcat>-scrtext_s = 'Custo'.
    ENDIF.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_cost->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_custo_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CLEAR gs_variables-refresh_items_grid.

  ELSE.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_cost
                       CHANGING lt_sort[]
                                lt_fcat[]
                                ls_row_no
                                ls_col_no
                                ls_row_info
                                ls_col_info
                                ls_layout.

    PERFORM grid_data_set  USING ref_grid_cost
                        CHANGING gt_custo_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_cost->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0309

*&---------------------------------------------------------------------*
*&      Form  controls_display_0313
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0313.

  DATA: lt_dates            TYPE /scwm/tt_lm_dates,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort,
        lt_orc_full         TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_getprodutos      TYPE zcl_zfarm_agriplan_mpc=>tt_getprodutos,
        ls_getproduto       LIKE LINE OF lt_getprodutos,
        ls_variant          TYPE disvariant,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i,
        lv_tabixc           TYPE char2,
        lv_field            TYPE fieldname,
        lv_title            TYPE lvc_title,
        ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        ls_orc_full         LIKE LINE OF lt_orc_full,
        lrt_mtart           TYPE /iwbep/t_cod_select_options,
        lrt_period          TYPE RANGE OF spmon,
        lrt_matnr           TYPE RANGE OF matnr,
        lt_period           TYPE zabstc_orca_period,
        lrs_period          LIKE LINE OF lrt_period,
        lrs_mtart           LIKE LINE OF lrt_mtart,
        lrs_matnr           LIKE LINE OF lrt_matnr,
        ls_period           LIKE LINE OF lt_period,
        lv_matnr_x          TYPE matnr,
        lv_tfor             TYPE string,
        lv_tman             TYPE string,
        lv_string           TYPE string,
        lv_tipo             TYPE char10,
        lv_period           TYPE char6.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  REFRESH gt_receita_fcat.

*-- GETPRODUTOSSET_GET_ENTITYSET
  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = gs_acdoc_infocus-x-achdr-datab
      iv_endda = gs_acdoc_infocus-x-achdr-datbi
    IMPORTING
      et_dates = lt_dates.

  lrs_period = 'IEQ'.
  LOOP AT lt_dates INTO DATA(ls_date).
    IF lv_period NE ls_date(6).
      INSERT INITIAL LINE INTO TABLE lt_period
       ASSIGNING FIELD-SYMBOL(<ls_period>).
      IF sy-subrc EQ 0.
        lrs_period-low = <ls_period> = lv_period = ls_date(6).
        APPEND lrs_period  TO lrt_period.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT rctyp, orcamento
    INTO TABLE @DATA(lt_rctyp)
    FROM ztfmrctyp
   WHERE orcamento EQ @abap_true.

  IF sy-subrc EQ 0.
    lv_matnr_x = 'TMAN' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr
      ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    lv_matnr_x = 'TFOR' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_rchdr)
      FROM zfmrchdr
      FOR ALL ENTRIES IN @lt_rctyp
     WHERE werks EQ @gs_acdoc_infocus-x-achdr-werks
       AND matnr IN @lrt_matnr[]
       AND datuv GE @gs_acdoc_infocus-x-achdr-datab
       AND rctyp EQ @lt_rctyp-rctyp
       AND datbi LE @gs_acdoc_infocus-x-achdr-datbi.

    DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                      AND matnr(4) NE 'TMAN'.

    IF lt_rchdr[] IS NOT INITIAL.
      SORT lt_rchdr BY rcnum.

      LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
        IF <ls_rchdr>-matnr(4) EQ 'TFOR'.
          <ls_rchdr>-text1 = lc_tipo-formacao.
        ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
          <ls_rchdr>-text1 = lc_tipo-manutencao.
        ENDIF.
      ENDLOOP.

      IF gv_tarefa IS NOT INITIAL.
        SELECT SINGLE matnr, matkl, extwg
          INTO @DATA(ls_mara)
          FROM mara
         WHERE matnr EQ @gv_tarefa.

        IF sy-subrc EQ 0.
          SELECT *
            INTO TABLE @DATA(lt_orcamento)
            FROM zabs_orcamento
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE acnum  EQ @gs_acdoc_infocus-acnum
             AND extwg  EQ @ls_mara-extwg
             AND matkl  EQ @ls_mara-matkl
             AND rcnum  EQ @lt_rchdr-rcnum
             AND period IN @lrt_period[].

          LOOP AT lt_orcamento INTO DATA(ls_orcamento).
            ls_orc_full = ls_orcamento.
            CLEAR: ls_orc_full-fazenda,
                   ls_orc_full-kostl.
            COLLECT ls_orc_full INTO lt_orc_full.
          ENDLOOP.

          lt_orcamento[] = lt_orc_full[].

          SORT lt_orcamento BY rcnum period matnr.

          SELECT *
            INTO TABLE @DATA(lt_zfmrclst)
            FROM zfmrclst
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE rcnum EQ @lt_rchdr-rcnum
             AND werks EQ @lt_rchdr-werks
             AND matnr EQ @lt_rchdr-matnr.

          IF sy-subrc EQ 0.
            SORT lt_zfmrclst BY rcnum werks matnr.

            DATA(lt_rchdrx) = lt_rchdr[].
            LOOP AT lt_rchdr INTO DATA(ls_rchdr).
              READ TABLE lt_zfmrclst TRANSPORTING NO FIELDS
                WITH KEY rcnum = ls_rchdr-rcnum
                         werks = ls_rchdr-werks
                         matnr = ls_rchdr-matnr BINARY SEARCH.

              IF sy-subrc EQ 0.
                LOOP AT lt_zfmrclst INTO DATA(ls_zfmrclst) FROM sy-tabix.
                  IF ls_zfmrclst-rcnum NE ls_rchdr-rcnum
                  OR ls_zfmrclst-werks NE ls_rchdr-werks
                  OR ls_zfmrclst-matnr NE ls_rchdr-matnr.
                    EXIT.
                  ENDIF.

                  CASE ls_zfmrclst-matnr(4).
                    WHEN 'TFOR'.
                      ls_getproduto-tipo = lc_tipo-formacao.
                    WHEN 'TMAN'.
                      ls_getproduto-tipo = lc_tipo-manutencao.
                  ENDCASE.

                  LOOP AT lt_period INTO ls_period.
                    DATA(lv_tabix) = sy-tabix.

                    READ TABLE lt_orcamento INTO ls_orcamento
                      WITH KEY rcnum  = ls_rchdr-rcnum
                               period = ls_period
                               matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                    IF sy-subrc NE 0.
                      CLEAR ls_orcamento.
                    ENDIF.

                    CASE lv_tabix.
                      WHEN '1'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m1d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m1d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m1p = ls_orcamento-passadas.
                        ls_getproduto-p1 = ls_orcamento-produtos.
                      WHEN '2'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m2d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m2d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m2p = ls_orcamento-passadas.
                        ls_getproduto-p2 = ls_orcamento-produtos.
                      WHEN '3'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m3d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m3d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m3p = ls_orcamento-passadas.
                        ls_getproduto-p3 = ls_orcamento-produtos.
                      WHEN '4'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m4d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m4d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m4p = ls_orcamento-passadas.
                        ls_getproduto-p4 = ls_orcamento-produtos.
                      WHEN '5'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m5d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m5d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m5p = ls_orcamento-passadas.
                        ls_getproduto-p5 = ls_orcamento-produtos.
                      WHEN '6'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m6d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m6d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m6p = ls_orcamento-passadas.
                        ls_getproduto-p6 = ls_orcamento-produtos.
                      WHEN '7'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m7d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m7d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m7p = ls_orcamento-passadas.
                        ls_getproduto-p7 = ls_orcamento-produtos.
                      WHEN '8'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m8d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m8d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m8p = ls_orcamento-passadas.
                        ls_getproduto-p8 = ls_orcamento-produtos.
                      WHEN '9'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m9d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m9d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m9p = ls_orcamento-passadas.
                        ls_getproduto-p9 = ls_orcamento-produtos.
                      WHEN '10'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m10d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m10d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m10p = ls_orcamento-passadas.
                        ls_getproduto-p10 = ls_orcamento-produtos.
                      WHEN '11'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m11d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m11d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m11p = ls_orcamento-passadas.
                        ls_getproduto-p11 = ls_orcamento-produtos.
                      WHEN '12'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m12d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m12d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m12p = ls_orcamento-passadas.
                        ls_getproduto-p12 = ls_orcamento-produtos.
                    ENDCASE.
                  ENDLOOP.

                  ls_getproduto-maktx     = ls_zfmrclst-maktx.
                  ls_getproduto-matnrins  = ls_zfmrclst-matnr_ins.
                  ls_getproduto-matnr     = ls_zfmrclst-matnr.
                  ls_getproduto-principal = ls_zfmrclst-rcinp_check.

                  INSERT INITIAL LINE INTO TABLE gt_receita_fcat
                    ASSIGNING FIELD-SYMBOL(<ls_receita_fcat>).
                  IF sy-subrc EQ 0.
                    <ls_receita_fcat>-resumo    = ls_getproduto-tipo.
                    <ls_receita_fcat>-principal = ls_getproduto-principal.
                    <ls_receita_fcat>-maktx     = ls_getproduto-maktx.
                    <ls_receita_fcat>-d1        = ls_getproduto-m1d.
                    <ls_receita_fcat>-p1        = ls_getproduto-m1p.
                    <ls_receita_fcat>-d2        = ls_getproduto-m2d.
                    <ls_receita_fcat>-p2        = ls_getproduto-m2p.
                    <ls_receita_fcat>-d3        = ls_getproduto-m3d.
                    <ls_receita_fcat>-p3        = ls_getproduto-m3p.
                    <ls_receita_fcat>-d4        = ls_getproduto-m4d.
                    <ls_receita_fcat>-p4        = ls_getproduto-m4p.
                    <ls_receita_fcat>-d5        = ls_getproduto-m5d.
                    <ls_receita_fcat>-p5        = ls_getproduto-m5p.
                    <ls_receita_fcat>-d6        = ls_getproduto-m6d.
                    <ls_receita_fcat>-p6        = ls_getproduto-m6p.
                    <ls_receita_fcat>-d7        = ls_getproduto-m7d.
                    <ls_receita_fcat>-p7        = ls_getproduto-m7p.
                    <ls_receita_fcat>-d8        = ls_getproduto-m8d.
                    <ls_receita_fcat>-p8        = ls_getproduto-m8p.
                    <ls_receita_fcat>-d9        = ls_getproduto-m9d.
                    <ls_receita_fcat>-p9        = ls_getproduto-m9p.
                    <ls_receita_fcat>-d10       = ls_getproduto-m10d.
                    <ls_receita_fcat>-p10       = ls_getproduto-m10p.
                    <ls_receita_fcat>-d11       = ls_getproduto-m11d.
                    <ls_receita_fcat>-p11       = ls_getproduto-m11p.
                    <ls_receita_fcat>-d12       = ls_getproduto-m12d.
                    <ls_receita_fcat>-p12       = ls_getproduto-m12p.
                  ENDIF.

                  APPEND ls_getproduto TO lt_getprodutos.
                  CLEAR ls_getproduto.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ref_container_dose IS INITIAL.

    CREATE OBJECT ref_container_dose
      EXPORTING
        container_name              = 'SAPLFMACM_0313_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF lines( gt_receita_fcat ) LT 10.
    DATA(lv_times) = 10 - lines( gt_receita_fcat ).
    DO lv_times TIMES.
      INSERT INITIAL LINE INTO TABLE gt_receita_fcat
        ASSIGNING <ls_receita_fcat>.
    ENDDO.
  ENDIF.

  IF ref_grid_dose IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_dose
      EXPORTING
        i_parent           = ref_container_dose
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.

    PERFORM field_catalog_prepare USING c_structure_name-receita
                               CHANGING lt_fcat.

    LOOP AT lt_period INTO ls_period.
      lv_tabix = sy-tabix.
      lv_tabixc = lv_tabix.
      CONCATENATE 'D' lv_tabixc INTO lv_field.
      READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        <ls_fcat>-reptext   = '(D)' && space && ls_period+4(2) && '.' && ls_period+0(4).
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_m = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_s = <ls_fcat>-reptext.
      ENDIF.
      CONCATENATE 'P' lv_tabixc INTO lv_field.
      READ TABLE lt_fcat ASSIGNING <ls_fcat>
        WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        <ls_fcat>-reptext   = '(P)' && space && ls_period+4(2) && '.' && ls_period+0(4).
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_m = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_s = <ls_fcat>-reptext.
      ENDIF.
    ENDLOOP.

    lv_field = 'Tipo'.
    READ TABLE lt_fcat ASSIGNING <ls_fcat>
      WITH KEY fieldname = lv_field.
    IF sy-subrc EQ 0.
      <ls_fcat>-reptext   = 'Tipo'.
      <ls_fcat>-scrtext_l = 'Tipo'.
      <ls_fcat>-scrtext_m = 'Tipo'.
      <ls_fcat>-scrtext_s = 'Tipo'.
    ENDIF.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_dose->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_receita_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CLEAR gs_variables-refresh_items_grid.

  ELSE.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_dose
                       CHANGING lt_sort[]
                                lt_fcat[]
                                ls_row_no
                                ls_col_no
                                ls_row_info
                                ls_col_info
                                ls_layout.

    PERFORM grid_data_set  USING ref_grid_dose
                        CHANGING gt_receita_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_dose->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0313

*&---------------------------------------------------------------------*
*&      Form  controls_display_0314
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0314.

  DATA: lt_dates            TYPE /scwm/tt_lm_dates,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort             TYPE lvc_t_sort,
        lt_orc_full         TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_getprodutos      TYPE zcl_zfarm_agriplan_mpc=>tt_getprodutos,
        ls_getproduto       LIKE LINE OF lt_getprodutos,
        ls_variant          TYPE disvariant,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i,
        lv_tabixc           TYPE char2,
        lv_field            TYPE fieldname,
        lv_title            TYPE lvc_title,
        ls_row_no           TYPE lvc_s_roid,
        ls_col_no           TYPE lvc_s_col,
        ls_row_info         TYPE lvc_s_row,
        ls_col_info         TYPE lvc_s_col,
        ls_environment      TYPE /agri/s_glvc_environment,
        ls_orc_full         LIKE LINE OF lt_orc_full,
        lrt_mtart           TYPE /iwbep/t_cod_select_options,
        lrt_period          TYPE RANGE OF spmon,
        lrt_matnr           TYPE RANGE OF matnr,
        lt_period           TYPE zabstc_orca_period,
        lrs_period          LIKE LINE OF lrt_period,
        lrs_mtart           LIKE LINE OF lrt_mtart,
        lrs_matnr           LIKE LINE OF lrt_matnr,
        ls_period           LIKE LINE OF lt_period,
        lv_matnr_x          TYPE matnr,
        lv_tfor             TYPE string,
        lv_tman             TYPE string,
        lv_string           TYPE string,
        lv_tipo             TYPE char10,
        lv_period           TYPE char6.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  REFRESH gt_qtde_fcat.

*-- GETPRODUTOSSET_GET_ENTITYSET
  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = gs_acdoc_infocus-x-achdr-datab
      iv_endda = gs_acdoc_infocus-x-achdr-datbi
    IMPORTING
      et_dates = lt_dates.

  lrs_period = 'IEQ'.
  LOOP AT lt_dates INTO DATA(ls_date).
    IF lv_period NE ls_date(6).
      INSERT INITIAL LINE INTO TABLE lt_period
       ASSIGNING FIELD-SYMBOL(<ls_period>).
      IF sy-subrc EQ 0.
        lrs_period-low = <ls_period> = lv_period = ls_date(6).
        APPEND lrs_period  TO lrt_period.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT rctyp, orcamento
    INTO TABLE @DATA(lt_rctyp)
    FROM ztfmrctyp
   WHERE orcamento EQ @abap_true.

  IF sy-subrc EQ 0.
    lv_matnr_x = 'TMAN' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr
      ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    lv_matnr_x = 'TFOR' && gv_tarefa+4(36).
    INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = lv_matnr_x.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_rchdr)
      FROM zfmrchdr
      FOR ALL ENTRIES IN @lt_rctyp
     WHERE werks EQ @gs_acdoc_infocus-x-achdr-werks
       AND matnr IN @lrt_matnr[]
       AND datuv GE @gs_acdoc_infocus-x-achdr-datab
       AND rctyp EQ @lt_rctyp-rctyp
       AND datbi LE @gs_acdoc_infocus-x-achdr-datbi.

    DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                      AND matnr(4) NE 'TMAN'.

    IF lt_rchdr[] IS NOT INITIAL.
      SORT lt_rchdr BY rcnum.

      LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
        IF <ls_rchdr>-matnr(4) EQ 'TFOR'.
          <ls_rchdr>-text1 = lc_tipo-formacao.
        ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
          <ls_rchdr>-text1 = lc_tipo-manutencao.
        ENDIF.
      ENDLOOP.

      IF gv_tarefa IS NOT INITIAL.
        SELECT SINGLE matnr, matkl, extwg
          INTO @DATA(ls_mara)
          FROM mara
         WHERE matnr EQ @gv_tarefa.

        IF sy-subrc EQ 0.
          SELECT *
            INTO TABLE @DATA(lt_orcamento)
            FROM zabs_orcamento
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE acnum  EQ @gs_acdoc_infocus-acnum
             AND extwg  EQ @ls_mara-extwg
             AND matkl  EQ @ls_mara-matkl
             AND rcnum  EQ @lt_rchdr-rcnum
             AND period IN @lrt_period[].

          LOOP AT lt_orcamento INTO DATA(ls_orcamento).
            ls_orc_full = ls_orcamento.
            CLEAR: ls_orc_full-fazenda,
                   ls_orc_full-kostl.
            COLLECT ls_orc_full INTO lt_orc_full.
          ENDLOOP.

          lt_orcamento[] = lt_orc_full[].

          SORT lt_orcamento BY rcnum period matnr.

          SELECT *
            INTO TABLE @DATA(lt_zfmrclst)
            FROM zfmrclst
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE rcnum EQ @lt_rchdr-rcnum
             AND werks EQ @lt_rchdr-werks
             AND matnr EQ @lt_rchdr-matnr.

          IF sy-subrc EQ 0.
            SORT lt_zfmrclst BY rcnum werks matnr.

            DATA(lt_rchdrx) = lt_rchdr[].
            LOOP AT lt_rchdr INTO DATA(ls_rchdr).
              READ TABLE lt_zfmrclst TRANSPORTING NO FIELDS
                WITH KEY rcnum = ls_rchdr-rcnum
                         werks = ls_rchdr-werks
                         matnr = ls_rchdr-matnr BINARY SEARCH.

              IF sy-subrc EQ 0.
                LOOP AT lt_zfmrclst INTO DATA(ls_zfmrclst) FROM sy-tabix.
                  IF ls_zfmrclst-rcnum NE ls_rchdr-rcnum
                  OR ls_zfmrclst-werks NE ls_rchdr-werks
                  OR ls_zfmrclst-matnr NE ls_rchdr-matnr.
                    EXIT.
                  ENDIF.

                  CASE ls_zfmrclst-matnr(4).
                    WHEN 'TFOR'.
                      ls_getproduto-tipo = lc_tipo-formacao.
                    WHEN 'TMAN'.
                      ls_getproduto-tipo = lc_tipo-manutencao.
                  ENDCASE.

                  LOOP AT lt_period INTO ls_period.
                    DATA(lv_tabix) = sy-tabix.

                    READ TABLE lt_orcamento INTO ls_orcamento
                      WITH KEY rcnum  = ls_rchdr-rcnum
                               period = ls_period
                               matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                    IF sy-subrc NE 0.
                      CLEAR ls_orcamento.
                    ENDIF.

                    CASE lv_tabix.
                      WHEN '1'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m1d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m1d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m1p = ls_orcamento-passadas.
                        ls_getproduto-p1 = ls_orcamento-produtos.
                      WHEN '2'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m2d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m2d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m2p = ls_orcamento-passadas.
                        ls_getproduto-p2 = ls_orcamento-produtos.
                      WHEN '3'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m3d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m3d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m3p = ls_orcamento-passadas.
                        ls_getproduto-p3 = ls_orcamento-produtos.
                      WHEN '4'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m4d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m4d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m4p = ls_orcamento-passadas.
                        ls_getproduto-p4 = ls_orcamento-produtos.
                      WHEN '5'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m5d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m5d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m5p = ls_orcamento-passadas.
                        ls_getproduto-p5 = ls_orcamento-produtos.
                      WHEN '6'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m6d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m6d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m6p = ls_orcamento-passadas.
                        ls_getproduto-p6 = ls_orcamento-produtos.
                      WHEN '7'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m7d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m7d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m7p = ls_orcamento-passadas.
                        ls_getproduto-p7 = ls_orcamento-produtos.
                      WHEN '8'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m8d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m8d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m8p = ls_orcamento-passadas.
                        ls_getproduto-p8 = ls_orcamento-produtos.
                      WHEN '9'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m9d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m9d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m9p = ls_orcamento-passadas.
                        ls_getproduto-p9 = ls_orcamento-produtos.
                      WHEN '10'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m10d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m10d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m10p = ls_orcamento-passadas.
                        ls_getproduto-p10 = ls_orcamento-produtos.
                      WHEN '11'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m11d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m11d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m11p = ls_orcamento-passadas.
                        ls_getproduto-p11 = ls_orcamento-produtos.
                      WHEN '12'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_getproduto-m12d = ls_orcamento-rcdos.
                        ELSE.
                          ls_getproduto-m12d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_getproduto-m12p = ls_orcamento-passadas.
                        ls_getproduto-p12 = ls_orcamento-produtos.
                    ENDCASE.
                  ENDLOOP.

                  ls_getproduto-maktx     = ls_zfmrclst-maktx.
                  ls_getproduto-matnrins  = ls_zfmrclst-matnr_ins.
                  ls_getproduto-matnr     = ls_zfmrclst-matnr.
                  ls_getproduto-principal = ls_zfmrclst-rcinp_check.

                  INSERT INITIAL LINE INTO TABLE gt_qtde_fcat
                    ASSIGNING FIELD-SYMBOL(<ls_qtde_fcat>).
                  IF sy-subrc EQ 0.
                    <ls_qtde_fcat>-resumo    = ls_getproduto-tipo.
                    <ls_qtde_fcat>-principal = ls_getproduto-principal.
                    <ls_qtde_fcat>-maktx     = ls_getproduto-maktx.
                    <ls_qtde_fcat>-d1        = ls_getproduto-p1.
                    <ls_qtde_fcat>-d2        = ls_getproduto-p2.
                    <ls_qtde_fcat>-d3        = ls_getproduto-p3.
                    <ls_qtde_fcat>-d4        = ls_getproduto-p4.
                    <ls_qtde_fcat>-d5        = ls_getproduto-p5.
                    <ls_qtde_fcat>-d6        = ls_getproduto-p6.
                    <ls_qtde_fcat>-d7        = ls_getproduto-p7.
                    <ls_qtde_fcat>-d8        = ls_getproduto-p8.
                    <ls_qtde_fcat>-d9        = ls_getproduto-p9.
                    <ls_qtde_fcat>-d10       = ls_getproduto-p10.
                    <ls_qtde_fcat>-d11       = ls_getproduto-p11.
                    <ls_qtde_fcat>-d12       = ls_getproduto-p12.
                  ENDIF.

                  APPEND ls_getproduto TO lt_getprodutos.
                  CLEAR ls_getproduto.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ref_container_qtde IS INITIAL.

    CREATE OBJECT ref_container_qtde
      EXPORTING
        container_name              = 'SAPLFMACM_0314_CC'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

  ENDIF.

  IF lines( gt_qtde_fcat ) LT 10.
    DATA(lv_times) = 10 - lines( gt_qtde_fcat ).
    DO lv_times TIMES.
      INSERT INITIAL LINE INTO TABLE gt_qtde_fcat
        ASSIGNING <ls_qtde_fcat>.
    ENDDO.
  ENDIF.

  IF ref_grid_qtde IS INITIAL.

    ls_environment-switchoff_performance = c_true.
    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.

    CREATE OBJECT ref_grid_qtde
      EXPORTING
        i_parent           = ref_container_qtde
        is_lvc_environment = ls_environment
        i_no_auto_details  = c_true
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-no_rowins = c_true.

    PERFORM field_catalog_prepare USING c_structure_name-qtde
                               CHANGING lt_fcat.

    LOOP AT lt_period INTO ls_period.
      lv_tabix = sy-tabix.
      lv_tabixc = lv_tabix.
      CONCATENATE 'D' lv_tabixc INTO lv_field.
      READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        <ls_fcat>-reptext   = ls_period+4(2) && '.' && ls_period+0(4).
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_m = <ls_fcat>-reptext.
        <ls_fcat>-scrtext_s = <ls_fcat>-reptext.
      ENDIF.
    ENDLOOP.

    lv_field = 'Tipo'.
    READ TABLE lt_fcat ASSIGNING <ls_fcat>
      WITH KEY fieldname = lv_field.
    IF sy-subrc EQ 0.
      <ls_fcat>-reptext   = 'Tipo'.
      <ls_fcat>-scrtext_l = 'Tipo'.
      <ls_fcat>-scrtext_m = 'Tipo'.
      <ls_fcat>-scrtext_s = 'Tipo'.
    ENDIF.

    ls_variant-report = sy-repid.
    ls_variant-handle = c_variant_handle-items.

    PERFORM control_events_register.

    PERFORM toolbar_excludes_prepare TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_qtde->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes
      CHANGING
        it_outtab                     = gt_qtde_fcat
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CLEAR gs_variables-refresh_items_grid.

  ELSE.

    CLEAR gs_variables-refresh_items_grid.

    PERFORM grid_data_get USING ref_grid_qtde
                       CHANGING lt_sort[]
                                lt_fcat[]
                                ls_row_no
                                ls_col_no
                                ls_row_info
                                ls_col_info
                                ls_layout.

    PERFORM grid_data_set  USING ref_grid_qtde
                        CHANGING gt_qtde_fcat[]
                                 lt_fcat[]
                                 lt_sort[]
                                 ls_row_no
                                 ls_col_no
                                 ls_row_info
                                 ls_col_info
                                 ls_layout.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_dose->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0314

*&---------------------------------------------------------------------*
*&      Form  control_events_register_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0301.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4     TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4    TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_items->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

****Register the F4 event for grid
  lwa_f4-fieldname = 'TPLNR_FL'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_grid_items->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_f4_request_items
               FOR ref_grid_items,
               ref_event_handler->on_hotspot_click_items
               FOR ref_grid_items,
               ref_event_handler->on_data_changed_items
               FOR ref_grid_items,
               ref_event_handler->on_toolbar_grid
               FOR ref_grid_items,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_items.

ENDFORM.                    "control_events_register_0301
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0203.

  DATA: lt_events TYPE cntl_simple_events,
        lt_f4     TYPE lvc_t_f4,

        lwa_event TYPE cntl_simple_event,
        lwa_f4    TYPE lvc_s_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  lwa_event-appl_event = c_true.
  lwa_event-eventid = /agri/cl_gui_alv_grid=>mc_evt_enter.
  APPEND lwa_event TO lt_events.

  CALL METHOD ref_grid_vlcl->register_edit_event
    EXPORTING
      i_event_id = lwa_event-eventid
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

****Register the F4 event for grid
*  lwa_f4-fieldname = 'TPLNR_FL'.
*  lwa_f4-register = c_true.
*  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_grid_vlcl->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_f4_request_vlcl
               FOR ref_grid_vlcl,
*               ref_event_handler->on_hotspot_click_items
*               FOR ref_grid_vlcl,
               ref_event_handler->on_data_changed_vlcl
               FOR ref_grid_vlcl,
               ref_event_handler->on_toolbar_grid_vlcl
               FOR ref_grid_vlcl,
               ref_event_handler->on_user_command_grid_vlcl
               FOR ref_grid_vlcl.

ENDFORM.                    "control_events_register_0301
*&---------------------------------------------------------------------*
*& Form CROP_AREA_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM crop_area_check .

  DATA: lv_acnum TYPE zfmacnum,
        lv_subrc TYPE sy-subrc.

  CHECK zsc_fmachdr-acnum IS NOT INITIAL.
  SELECT SINGLE acnum
           INTO lv_acnum
           FROM zfmachdr
          WHERE acnum = zsc_fmachdr-acnum.
  IF sy-subrc = 0.
    MESSAGE ID '/AGRI/GLCM' TYPE 'E' NUMBER '004' WITH lv_acnum
                                                INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
  ELSE.
    PERFORM document_infocus_lock USING zsc_fmachdr-acnum
                                        zsc_fmachdr-ajahr
                               CHANGING lv_subrc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CROP_AREA_VALIDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ACITM
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM crop_area_validate  CHANGING lt_acitm TYPE zt_fmacitm
*...BOC-T_T.KONNO
                                  lt_acitm_old TYPE zt_fmacitm
*...EOC-T_T.KONNO
                                  lv_subrc TYPE sy-subrc
                                  lv_ajahr TYPE ajahr.

  DATA: lt_rmg_tplnr_fl TYPE zfmac_terrain_range_t,
        lt_tempfmacitm  TYPE zt_fmacitm.

  CONSTANTS: BEGIN OF c_status,
               criado    TYPE zfmacitst VALUE ' ',
               agendado  TYPE zfmacitst VALUE 'A',
               planejado TYPE zfmacitst VALUE 'B',
             END OF c_status.

*...BOC-T_T.KONNO
*  SELECT * FROM zfmaitm AS m INNER JOIN zfmachdr AS r
*                       ON  m~acnum EQ r~acnum
*                INTO CORRESPONDING FIELDS OF TABLE lt_tempfmacitm
*                 FOR ALL ENTRIES IN lt_acitm[]
*            WHERE m~tplnr_fl = lt_acitm-tplnr_fl
*            AND  r~ajahr = lv_ajahr
*            AND m~acetm  NE 'B'.

  IF lt_acitm[] IS NOT INITIAL.
    SELECT * FROM zfmaitm AS m
      INNER JOIN zfmachdr AS r
      ON m~acnum EQ r~acnum
      INTO CORRESPONDING FIELDS OF TABLE @lt_tempfmacitm
      FOR ALL ENTRIES IN @lt_acitm[]
     WHERE m~tplnr_fl EQ @lt_acitm-tplnr_fl
       AND m~acetm    NE @c_status-planejado.

    DELETE lt_tempfmacitm WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                            AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                            AND NOT ( datab LE p_datab AND
                                      datbi GE p_datbi ).

    lt_acitm_old[] = lt_tempfmacitm[].
  ENDIF.
*...EOC-T_T.KONNO

  IF lt_tempfmacitm[] IS NOT INITIAL.
    PERFORM range_terrain_create USING lt_tempfmacitm
                              CHANGING lt_rmg_tplnr_fl.
    DELETE lt_acitm WHERE tplnr_fl IN lt_rmg_tplnr_fl.
  ENDIF.

  IF lt_acitm[] IS NOT INITIAL.
    lv_subrc = 0.
  ELSE.
    lv_subrc = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALL_MODAL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> C_SCREEN_UPDATE_MASS
*&---------------------------------------------------------------------*
FORM call_modal_screen.
  CALL SCREEN 310 STARTING AT 35 5.
ENDFORM.
