*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0W
*&---------------------------------------------------------------------*

FORM worklist_update .

  DATA: lwa_search_header   LIKE LINE OF gt_search_header,
        lwa_worklist_header LIKE LINE OF gt_worklist_header,
        lv_view.

  IF gs_variables-document_mode = c_mode_create.
    MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO lwa_search_header.
    APPEND lwa_search_header TO gt_search_header.
  ELSE.
    lv_view = ref_worklist->view_in_focus_get( ).
    IF lv_view EQ 2.
      READ TABLE gt_search_header
       WITH KEY accom = gs_acdoc_infocus-x-achdr-accom
                                TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr
                               TO lwa_search_header.
        MODIFY gt_search_header FROM lwa_search_header INDEX sy-tabix.
      ENDIF.
    ELSEIF lv_view EQ 1.
      READ TABLE gt_worklist_header
      WITH KEY accom = gs_acdoc_infocus-x-achdr-accom
                                        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr
                             TO lwa_worklist_header.
        MODIFY gt_worklist_header FROM lwa_worklist_header
                                           INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.

  gs_variables-refresh_worklist = c_true.

ENDFORM.                    " WORKLIST_UPDATE
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_SEARCH_HDR_MORE  text
*----------------------------------------------------------------------*
FORM worklist_search  CHANGING lt_search_hdr_more TYPE /agri/t_fmachdr.

  REFRESH: gt_search_header.
  CLEAR:   gt_search_header.

  SUBMIT /agri/fmac_process WITH p_excall = c_true       "#EC CI_SUBMIT
  VIA SELECTION-SCREEN AND RETURN.

  IMPORT /agri/t_fmachdr = gt_search_header[]
                FROM MEMORY ID 'FMACHDR_SRCH'.

  IF gt_search_header[] IS INITIAL.
    MESSAGE s751(/agri/global) INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF NOT lt_search_hdr_more IS INITIAL.
    APPEND LINES OF lt_search_hdr_more TO gt_search_header.
  ENDIF.

  PERFORM accom_data_display.

ENDFORM.                    " WORKLIST_SEARCH
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_BUILD
*&---------------------------------------------------------------------*
FORM worklist_build.
  DATA: lv_count            TYPE i,
        ls_variant          TYPE disvariant,
        ls_layout           TYPE lvc_s_layo,
        lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions.

  IF ref_worklist IS INITIAL.
    CREATE OBJECT ref_worklist
      EXPORTING
        i_ref_parent      = ref_worklist_container
        i_objtyp          = c_object-bor
        i_fieldname_obj   = 'ACCOM'
        i_esh_obj         = c_object-esh_object
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  CHECK sy-subrc EQ 0.

  PERFORM field_catalog_prepare
              USING c_structure_name-worklist_header.
*                                CHANGING lt_fcat.

  PERFORM control_events_register.

  ls_variant-report    = c_program.
  ls_variant-handle    = c_variant_handle-worklist.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = c_true.

  PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

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
      ct_fcat_hdr          = gt_fcat.
  CALL METHOD ref_worklist->set_toolbar_interactive.
ENDFORM.                    " WORKLIST_BUILD
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_REFRESH
*&---------------------------------------------------------------------*
FORM worklist_refresh  USING   lv_view.

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
    SORT gt_worklist_header BY accom.
    DESCRIBE TABLE gt_worklist_header LINES sy-tfill.
    CALL METHOD ref_worklist->worklist_refresh  "#EC CI_ALL_FIELDS_NEEDED
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
FORM worklist_data_select  USING lt_worklist TYPE /agri/t_gworklist.

  DATA: lwa_worklist LIKE LINE OF lt_worklist,
        lwa_achdrt   TYPE /agri/fmachdrt,
        lt_accom     TYPE /agri/t_fmacom,
        lt_achdrt    TYPE TABLE OF /agri/fmachdrt.
  FIELD-SYMBOLS : <lwa_achdr> TYPE /agri/s_fmachdr_wl.

  REFRESH gt_worklist_header.

  LOOP AT lt_worklist INTO lwa_worklist.
    APPEND lwa_worklist-objkey TO lt_accom.
  ENDLOOP.

  IF NOT lt_accom IS INITIAL.
    SELECT * FROM /agri/fmachdr"#EC CI_NOORDER
      INTO CORRESPONDING FIELDS OF TABLE gt_worklist_header
      FOR ALL ENTRIES IN lt_accom WHERE accom EQ lt_accom-accom."#EC CI_NOORDER
  ENDIF.

  SELECT accom descr FROM /agri/fmachdrt
    INTO CORRESPONDING FIELDS OF TABLE lt_achdrt
    FOR ALL ENTRIES IN lt_accom
    WHERE accom EQ lt_accom-accom AND
          spras EQ sy-langu.
  SORT lt_achdrt BY accom.
  LOOP AT gt_worklist_header ASSIGNING <lwa_achdr>."#EC CI_ALL_FIELDS_NEEDED
    READ TABLE lt_achdrt INTO lwa_achdrt"#EC CI_ALL_FIELDS_NEEDED
      WITH KEY accom = <lwa_achdr>-accom BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    <lwa_achdr>-descr = lwa_achdrt-descr."#EC CI_ALL_FIELDS_NEEDED
  ENDLOOP.

ENDFORM.                    " WORKLIST_DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  WONUM_INFOCUS_READ
*&---------------------------------------------------------------------*
FORM wonum_infocus_read  USING    lv_wonum
                                  lv_aufnr
                         CHANGING lt_arbpl TYPE /agri/t_range_arbpl.

  DATA: lt_wonum          TYPE /agri/t_fmwonum,
        lt_fmwo_doc       TYPE /agri/t_fmwoc_doc,
        lwa_wodoc_infocus TYPE /agri/s_fmwoc_doc,
        lt_afko           TYPE STANDARD TABLE OF afko,
*       lt_plpo           TYPE STANDARD TABLE OF plpo,
        lt_afvc           TYPE STANDARD TABLE OF afvc,
        lt_crhd           TYPE STANDARD TABLE OF crhd,
        lwa_crhd          TYPE crhd,
        lwa_arbpl         TYPE /agri/s_range_arbpl,
        lv_subrc          TYPE i.

  IF lv_wonum IS NOT INITIAL.
    APPEND lv_wonum TO lt_wonum.

    CALL FUNCTION '/AGRI/FMWO_VIEW'
      EXPORTING
        it_wonum       = lt_wonum
      IMPORTING
        et_wodoc       = lt_fmwo_doc
      EXCEPTIONS ##FM_SUBRC_OK
        no_data_exists = 1
        OTHERS         = 2.

    READ TABLE lt_fmwo_doc INTO lwa_wodoc_infocus INDEX 1.
    DELETE lwa_wodoc_infocus-x-woitm WHERE aufnr NE lv_aufnr.
    IF lwa_wodoc_infocus-x-woitm[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_afko FROM afko
        FOR ALL ENTRIES IN lwa_wodoc_infocus-x-woitm  "#EC CI_NO_TRANSFORM
        WHERE aufnr = lwa_wodoc_infocus-x-woitm-aufnr.
      lv_subrc = sy-subrc.
    ELSE.
      lv_subrc = 4.
    ENDIF.
  ELSE.
    SELECT * INTO TABLE lt_afko FROM afko
      WHERE aufnr = lv_aufnr.
    lv_subrc = sy-subrc.
  ENDIF.

  CHECK lv_subrc EQ 0.
***Extended additional syntax check 1_3 ATC 1709 PQ
  IF lt_afko IS NOT INITIAL.

    SELECT * INTO TABLE lt_afvc FROM afvc     "#EC CI_FAE_LINES_ENSURED
      FOR ALL ENTRIES IN lt_afko  "#EC CI_NO_TRANSFORM
      WHERE aufpl = lt_afko-aufpl.

  ENDIF.
*  SELECT * INTO TABLE lt_plpo FROM plpo
*    FOR ALL ENTRIES IN lt_afko
*    WHERE plnnr = lt_afko-plnnr.

  CHECK sy-subrc EQ 0 AND NOT lt_afvc[] IS INITIAL.
  SELECT * INTO TABLE lt_crhd FROM crhd            "#EC CI_ALL_FIELDS_NEEDED     "#EC CI_NOORDER
    FOR ALL ENTRIES IN lt_afvc  "#EC CI_NO_TRANSFORM
    WHERE objid = lt_afvc-arbid.

  LOOP AT lt_crhd INTO lwa_crhd.                  "#EC CI_ALL_FIELDS_NEEDED      "#EC CI_NOORDER
    lwa_arbpl-sign = c_sign-include.
    lwa_arbpl-option = c_operator_word-equalto.
    lwa_arbpl-low = lwa_crhd-arbpl.               "#EC CI_ALL_FIELDS_NEEDED      "#EC CI_NOORDER
    APPEND lwa_arbpl TO lt_arbpl.
  ENDLOOP.

  SORT lt_arbpl BY low.
  DELETE ADJACENT DUPLICATES FROM lt_arbpl.

ENDFORM.                    " WONUM_INFOCUS_READ
