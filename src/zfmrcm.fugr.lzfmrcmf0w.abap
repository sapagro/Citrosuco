*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_build .

  DATA : lv_count            TYPE i,
         ls_variant          TYPE disvariant,
         ls_layout           TYPE lvc_s_layo,
         lt_fcat             TYPE lvc_t_fcat,
         lt_toolbar_excludes TYPE ui_functions.

  IF ref_worklist IS INITIAL.
    CREATE OBJECT ref_worklist
      EXPORTING
        i_ref_parent      = ref_worklist_container
        i_objtyp          = c_object-bor
        i_fieldname_obj   = 'RCNUM'
        i_esh_obj         = c_object-esh_object
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDIF.
  CHECK sy-subrc EQ 0.
  PERFORM field_catalog_prepare USING c_structure_name-work_list_header
                                CHANGING lt_fcat.
  PERFORM control_events_register.

  ls_variant-report    = c_program_rcm.
  ls_variant-handle    = c_variant_handle-worklist.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = c_true.

  PERFORM toolbar_excludes_prepare TABLES lt_toolbar_excludes.

  DESCRIBE TABLE gt_search_header LINES lv_count.
  CALL METHOD ref_worklist->worklist_first_display
    EXPORTING
      i_no_items_display   = c_true
      i_count_header       = lv_count
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = ls_layout
      it_toolbar_excluding = lt_toolbar_excludes
    CHANGING
      ct_header            = gt_search_header
      ct_fcat_hdr          = lt_fcat.

  CALL METHOD ref_worklist->set_toolbar_interactive.

ENDFORM.                    " WORKLIST_BUILD
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_refresh USING    lv_view.

  DATA: lt_worklist         TYPE /agri/t_gworklist,
        lv_worklist_changed.

  IF lv_view IS INITIAL.
    lv_view = ref_worklist->view_in_focus_get( ).
  ENDIF.

  IF lv_view EQ 1.
    CALL METHOD ref_worklist->worklist_get
      IMPORTING
        et_worklist_hdr    = lt_worklist
        e_worklist_changed = lv_worklist_changed.

    IF lv_worklist_changed EQ c_true
      OR  gt_worklist_header IS INITIAL.
      PERFORM worklist_data_select USING lt_worklist.
    ENDIF.

    SORT gt_worklist_header BY rcnum.

    DESCRIBE TABLE gt_worklist_header LINES sy-tfill.
    CALL METHOD ref_worklist->worklist_refresh
      EXPORTING
        i_count_header = sy-tfill
      CHANGING
        ct_header      = gt_worklist_header.
  ELSE.
    DESCRIBE TABLE gt_search_header LINES sy-tfill.
    CALL METHOD ref_worklist->worklist_refresh
      EXPORTING
        i_count_header = sy-tfill
      CHANGING
        ct_header      = gt_search_header.

  ENDIF.
  CALL METHOD ref_worklist->set_toolbar_interactive.
  CLEAR gs_variables-refresh_worklist.

ENDFORM.                    " WORKLIST_REFRESH
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_DATA_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_update USING lwa_rcdoc TYPE zsc_fmrc_doc .

  DATA: lwa_search_header   LIKE LINE OF gt_search_header,
        lwa_worklist_header LIKE LINE OF gt_worklist_header,
        lv_view.

  IF gs_variables-document_mode EQ c_mode_create.
    MOVE-CORRESPONDING lwa_rcdoc-x-rchdr TO lwa_search_header.
    APPEND lwa_search_header TO gt_search_header.
  ELSE.
    lv_view = ref_worklist->view_in_focus_get( ).
    IF lv_view EQ 2.
      READ TABLE gt_search_header WITH KEY rcnum = gs_rcdoc_infocus-x-rchdr-rcnum
                                  TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_rcdoc_infocus-x-rchdr TO lwa_search_header.
        MODIFY gt_search_header FROM lwa_search_header INDEX sy-tabix.
      ENDIF.
    ELSEIF lv_view EQ 1.
      READ TABLE gt_worklist_header WITH KEY rcnum = gs_rcdoc_infocus-x-rchdr-rcnum
                                    TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_rcdoc_infocus-x-rchdr TO lwa_worklist_header.
        MODIFY gt_worklist_header FROM lwa_worklist_header INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.

  gs_variables-refresh_worklist = c_true.

ENDFORM.                    " WORKLIST_DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_DATA_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM worklist_data_select  USING lt_worklist TYPE /agri/t_gworklist.

  DATA : lwa_worklist LIKE LINE OF lt_worklist,
         lwa_rchdr    TYPE zfmrchdr,
         lt_rcnum     TYPE zt_fmrcnum.

  FIELD-SYMBOLS : <lwa_rchdr> TYPE zsc_fmrchdr_wl.

  REFRESH gt_worklist_header.

  LOOP AT lt_worklist INTO lwa_worklist.
    APPEND lwa_worklist-objkey TO lt_rcnum.
  ENDLOOP.

  IF NOT lt_rcnum IS INITIAL.
    SELECT * FROM zfmrchdr
      INTO CORRESPONDING FIELDS OF TABLE gt_worklist_header
      FOR ALL ENTRIES IN lt_rcnum WHERE rcnum EQ lt_rcnum-rcnum.
  ENDIF.

  READ TABLE gt_worklist_header INTO lwa_worklist INDEX 1.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING lwa_worklist TO gs_rckey.
  ENDIF.

ENDFORM.                    " WORKLIST_DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_search  CHANGING lt_search_hdr_more TYPE zt_fmrchdr.

  REFRESH: gt_search_header.
  CLEAR:   gt_search_header.

  SUBMIT zfmrc_process WITH p_excall = c_true            "#EC CI_SUBMIT
         VIA SELECTION-SCREEN AND RETURN.

  IMPORT zfmrchdr = gt_search_header[] FROM MEMORY ID 'FMRC_SRCH'.

  IF gt_search_header[] IS INITIAL.
    MESSAGE s751(/agri/global) INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF NOT lt_search_hdr_more IS INITIAL.
    APPEND LINES OF lt_search_hdr_more TO gt_search_header.
  ENDIF.

  PERFORM rcdoc_data_set.

ENDFORM.                    " WORKLIST_SEARCH
