*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0R
*&---------------------------------------------------------------------*
FORM refresh_items_grid.

  DATA: ls_layout              TYPE lvc_s_layo,
        ls_stable              TYPE lvc_s_stbl,
        lwa_row_info           TYPE lvc_s_row,
        lwa_col_info           TYPE lvc_s_col,
        lwa_rowid_current_cell TYPE lvc_s_row,
        lwa_colid_current_cell TYPE lvc_s_col..

  CLEAR: gs_variables-refresh_items_grid.
*  ls_stable-row = c_true.
*  ls_stable-col = c_true.
*  CLEAR: ls_layout.
*
  CALL METHOD ref_grid_items->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.
  ls_layout-cwidth_opt = c_true.
*  ls_layout-sel_mode = 'A'.
*  ls_layout-stylefname = 'STYLES'.
  ls_layout-info_fname = 'ROWCOLOR'.


  CALL METHOD ref_grid_items->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

****Get the scroll info
  CALL METHOD ref_grid_items->get_scroll_info_via_id
    IMPORTING
      es_row_info = lwa_row_info
      es_col_info = lwa_col_info.

  CALL METHOD ref_grid_items->get_current_cell
    IMPORTING
      es_row_id = lwa_rowid_current_cell
      es_col_id = lwa_colid_current_cell.

  CALL METHOD ref_grid_items->tables_display_refresh
    CHANGING
      it_outtab = gt_items_layout.

*  CALL METHOD ref_grid_items->frontend_fieldcatalog_set
*    EXPORTING
*      it_fieldcatalog = gt_fcat.

  IF NOT lwa_row_info IS INITIAL AND NOT lwa_col_info IS INITIAL.
****Set the scroll info
    CALL METHOD ref_grid_items->set_scroll_info_via_id
      EXPORTING
        is_row_info = lwa_row_info
        is_col_info = lwa_col_info.
  ENDIF.

  IF NOT lwa_rowid_current_cell IS INITIAL
  AND NOT lwa_colid_current_cell IS INITIAL.
    CALL METHOD ref_grid_items->set_current_cell_via_id
      EXPORTING
        is_row_id    = lwa_rowid_current_cell
        is_column_id = lwa_colid_current_cell.
  ENDIF.

ENDFORM.                    " REFRESH_ITEMS_GRID
*&---------------------------------------------------------------------*
*&      Form  refresh_details_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_details_grid.

  DATA: ls_layout              TYPE lvc_s_layo,
        ls_stable              TYPE lvc_s_stbl,
        lwa_row_info           TYPE lvc_s_row,
        lwa_col_info           TYPE lvc_s_col,
        lwa_rowid_current_cell TYPE lvc_s_row,
        lwa_colid_current_cell TYPE lvc_s_col..


  CALL METHOD ref_grid_details->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.
  ls_layout-cwidth_opt = c_true.
  ls_layout-info_fname = 'ROWCOLOR'.

  CALL METHOD ref_grid_details->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = gt_fcat.

  PERFORM field_catalog_prepare USING c_structure_name-details.

  CALL METHOD ref_grid_details->frontend_fieldcatalog_set
    EXPORTING
      it_fieldcatalog = gt_fcat
      is_layout       = ls_layout.

*  PERFORM control_events_register.

  CALL METHOD ref_grid_details->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.




ENDFORM.                    " REFRESH_DETAILS_GRID
*&---------------------------------------------------------------------*
*&      Form  REVERSED_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_TRUE  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM reversed_infocus_save  USING lv_post
                                CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.
    PERFORM reversed_process CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text         = ref_text
    CHANGING
      ct_acdoc          = lt_acdoc
      ct_messages       = lt_messages
    EXCEPTIONS
      no_change         = 1
      OTHERS            = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                       INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                    INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '055'
                            WITH gs_acdoc_infocus-x-achdr-accom
                            INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " REVERSED_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  REVERSED_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM reversed_process  CHANGING lv_subrc.
  FIELD-SYMBOLS: <lwa_acitm> TYPE /agri/s_fmacitm.

  gs_acdoc_infocus-x-achdr-status = c_process_status-cnf .
  IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
    gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
  ENDIF.
  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>.
    <lwa_acitm>-status = c_process_status-cnf.
    CLEAR <lwa_acitm>-distr.
    IF <lwa_acitm>-updkz NE c_updkz_new.
      <lwa_acitm>-updkz = c_updkz_update.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " REVERSED_PROCESS
*&---------------------------------------------------------------------*
*&      Form  REVERSE_INFOCUS_SAVE
*&---------------------------------------------------------------------*
FORM reverse_infocus_save  USING lv_post
                           CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        lwa_items   LIKE LINE OF gt_items_layout,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.

    READ TABLE gt_items_layout INTO lwa_items
                               WITH KEY intext = c_true.
    IF sy-subrc EQ 0.
*--- Third party services MIGO 101 Cancel.
      PERFORM good_receipt_reverse.
    ENDIF.

    READ TABLE gt_items_layout INTO lwa_items
                                WITH KEY intext = c_false.
    IF sy-subrc EQ 0.
*--- Normal CO11
      PERFORM reverse_confirmations CHANGING lv_subrc.
    ENDIF.

    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text         = ref_text
    CHANGING
      ct_acdoc          = lt_acdoc
      ct_messages       = lt_messages
    EXCEPTIONS
      no_change         = 1
      OTHERS            = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success
                                           NUMBER '322'
                                           INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                        NUMBER '011'
                                        INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '055'
                            WITH gs_acdoc_infocus-x-achdr-accom
                            INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " REVERSE_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  REVERSE_CONFIRMATIONS
*&---------------------------------------------------------------------*
FORM reverse_confirmations  CHANGING lv_subrc.

  DATA:ls_selected_rows   TYPE lvc_s_row,
         lwa_items_layout TYPE /agri/s_fmacitm_layout,
         lt_fmfp_doc      TYPE /agri/t_fmfp_doc,
         lt_afru          TYPE TABLE OF afru,
         ls_afru          TYPE afru,
         ls_bapiret       TYPE bapiret1,
         lwa_message      TYPE /agri/s_gprolog,
         lt_messages      TYPE /agri/t_gprolog,
         lwa_context      TYPE /agri/s_fmfp_context,
         lt_fpitm         TYPE /agri/t_fmfpitm,
         lwa_fmacitm      TYPE /agri/s_fmacitm,
         lt_activity      TYPE TABLE OF /agri/fmacact,
         lwa_activity     TYPE /agri/fmacact.

  FIELD-SYMBOLS: <lwa_selected_rows> TYPE lvc_s_row,
                 <lwa_fmacitm>       TYPE /agri/s_fmacitm.

  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.
  IF gt_selected_rows IS NOT INITIAL.
    DELETE gt_selected_rows WHERE rowtype IS NOT INITIAL.
*    SORT gt_items_layout BY posnr.
****Desc Layout Row Delete
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-desc_delete.

  ELSEIF gt_selected_rows IS INITIAL.
****Please select a row
    MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-error
            NUMBER 321 INTO sy-msgli.
    message_simple space.
  ENDIF.
  SELECT idactv bill  "#EC CI_NOWHERE
    INTO CORRESPONDING FIELDS OF TABLE lt_activity
    FROM /agri/fmacact.

  LOOP AT gt_selected_rows ASSIGNING <lwa_selected_rows>.

    READ TABLE gt_items_layout INTO lwa_items_layout
                    INDEX <lwa_selected_rows>-index.
*---Check type resources
    CHECK lwa_items_layout-intext NE c_true.

    CHECK lwa_items_layout-status EQ c_process_status-cnf
          AND lwa_items_layout-intext EQ c_false.
    READ TABLE lt_activity INTO lwa_activity
                    WITH KEY idactv =  lwa_items_layout-idactvl.
    IF sy-subrc NE 0.
      READ TABLE lt_activity INTO lwa_activity
                    WITH KEY idactv =  lwa_items_layout-idactve.
    ENDIF.

    IF lwa_activity-bill NE 'YES'.
      READ TABLE gs_acdoc_infocus-x-acitm[] ASSIGNING <lwa_fmacitm>
      WITH KEY posnr = lwa_items_layout-posnr.
*      WITH KEY rmzhl = lwa_items_layout-rmzhl.
      "CLEAR: <lwa_fmacitm>-menge, <lwa_fmacitm>-qmein.
      <lwa_fmacitm>-status = c_process_status-ctd.
      IF <lwa_fmacitm>-updkz NE c_updkz_new.
        <lwa_fmacitm>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
        EXPORTING
          confirmation        = lwa_items_layout-rueck
          confirmationcounter = lwa_items_layout-rmzhl
        IMPORTING
          return              = ls_bapiret.
      COMMIT WORK.
      lv_subrc = sy-subrc.

      IF lv_subrc = 0.
        WAIT UP TO 2 SECONDS.
        READ TABLE gs_acdoc_infocus-x-acitm[] ASSIGNING <lwa_fmacitm>
        WITH KEY rueck = lwa_items_layout-rueck
                 rmzhl = lwa_items_layout-rmzhl.
        CLEAR: <lwa_fmacitm>-rmzhl. "<lwa_fmacitm>-menge, <lwa_fmacitm>-qmein>.
        <lwa_fmacitm>-status = c_process_status-ctd.
        IF <lwa_fmacitm>-updkz NE c_updkz_new.
          <lwa_fmacitm>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_subrc EQ 0.
    READ TABLE gs_acdoc_infocus-x-acitm[] INTO lwa_fmacitm
                    WITH KEY status = c_process_status-cnf.
    CHECK sy-subrc = 4.
    gs_acdoc_infocus-x-achdr-status = c_process_status-ctd.
    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.

ENDFORM.                    " REVERSE_CONFIRMATIONS
*&---------------------------------------------------------------------*
*&      Form  RELEASE_STATUS_DETERMINE
*&---------------------------------------------------------------------*
FORM release_status_determine  USING lv_user_status
                            CHANGING lv_kfrst.

  DATA: lt_tj31 TYPE TABLE OF tj31,
        ls_tj31 TYPE tj31.

  SELECT * FROM tj31 INTO TABLE lt_tj31
            WHERE stsma EQ gs_acdoc_infocus-x-achdr-stsma
              AND estat EQ lv_user_status.
  CHECK sy-subrc EQ 0.

  READ TABLE lt_tj31 INTO ls_tj31
       WITH KEY vrgng = c_status_transaction-blocked.
  IF sy-subrc EQ 0 AND ls_tj31-modkz CA '12'.
    lv_kfrst = 'A'.
  ENDIF.

  READ TABLE lt_tj31 INTO ls_tj31
       WITH KEY vrgng = c_status_transaction-released.
  IF sy-subrc EQ 0 AND ls_tj31-modkz CA '12'.
    lv_kfrst = 'R'.
  ENDIF.

ENDFORM.                    " RELEASE_STATUS_DETERMINE
