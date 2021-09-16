*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0W .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WORKLIST_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_build .

  DATA: lv_count   TYPE i,
        ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant,
        lt_fcat    TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions,
        lt_sort               TYPE lvc_t_sort,
        lt_group_level        TYPE lvc_t_fimg.

  IF ref_worklist IS INITIAL.
    CREATE OBJECT ref_worklist
      EXPORTING
*        i_link_dynnr      =
*        i_link_repid      =
*        i_metric          = CNTL_METRIC_DYNPRO
        i_ref_parent      = ref_worklist_container
*        i_name            =
        i_objtyp          = c_object-bor
        i_fieldname_obj   = 'TPLNR_FL'
*        i_subobjtyp       =
*        i_fieldname_subobj =
*        i_default_view    = 2
*        i_lock_worklist   = SPACE
        i_esh_obj         = c_object-esh_object
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  CHECK sy-subrc = 0.

  PERFORM field_catalog_prepare USING c_structure_name-worklist_header
                            CHANGING lt_fcat.

  PERFORM control_events_register.

  ls_variant-report    = c_program-funloc.
  ls_variant-handle    = c_variant_handle-worklist.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = c_true.

  PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

  DESCRIBE TABLE gt_search_header LINES lv_count.

  PERFORM sort_group_tables_prepare CHANGING lt_sort
                                             lt_group_level.

  CALL METHOD ref_worklist->worklist_first_display
    EXPORTING
*     i_hdr_structure       =
*     i_item_structure      =
*     i_fill_header_on_item = SPACE
      i_no_items_display    = c_true
*     i_show_items          = 'X'
      i_count_header        = lv_count
      is_variant            = ls_variant
      i_save                = 'A'
*     i_default             = 'X'
*     i_disp_profile        =
      is_layout             = ls_layout
      it_toolbar_excluding  = lt_toolbar_excludes
      it_sort               = lt_sort
    CHANGING
      ct_header             = gt_search_header
*     ct_item               =
      ct_fcat_hdr           = lt_fcat.
*     ct_fcat_item          = .

  CALL METHOD ref_worklist->set_toolbar_interactive.

ENDFORM.                     " WORKLIST_BUILD

*&---------------------------------------------------------------------*
*&      Form  worklist_refresh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_refresh  USING    lv_view.

  DATA: lt_worklist TYPE /agri/t_gworklist,
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
    SORT gt_worklist_header BY tplnr_fl.
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
  CLEAR gs_variables-worklist_refresh.

ENDFORM.                    " WORKLIST_REFRESH

*&---------------------------------------------------------------------*
*&      Form  worklist_data_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_data_select USING lt_worklist TYPE /agri/t_gworklist.

  DATA : lwa_worklist LIKE LINE OF lt_worklist,
         lwa_iflotx   TYPE /agri/s_gliflotx,
         lt_tplnr     TYPE tplnr_tab,
         lt_iflotx    TYPE TABLE OF /agri/glflotx.  " Replaced Statndard table IFLOTX with /AGRI/GLFLOTX

  FIELD-SYMBOLS : <lwa_flot> TYPE /agri/s_glflot_wl.

  REFRESH gt_worklist_header.

  LOOP AT lt_worklist INTO lwa_worklist.
    APPEND lwa_worklist-objkey TO lt_tplnr.
  ENDLOOP.

  IF NOT lt_tplnr[] IS INITIAL.
    SELECT * FROM /agri/glflot
      INTO CORRESPONDING FIELDS OF TABLE gt_worklist_header
      FOR ALL ENTRIES IN lt_tplnr WHERE tplnr_fl EQ lt_tplnr-table_line.
  ENDIF.

  SELECT tplnr_fl pltxt FROM /agri/glflotx
    INTO CORRESPONDING FIELDS OF TABLE lt_iflotx
    FOR ALL ENTRIES IN lt_tplnr
    WHERE tplnr_fl EQ lt_tplnr-table_line AND
          spras EQ sy-langu.
  SORT lt_iflotx BY tplnr_fl.
  LOOP AT gt_worklist_header ASSIGNING <lwa_flot>.
    READ TABLE lt_iflotx INTO lwa_iflotx
      WITH KEY tplnr_fl = <lwa_flot>-tplnr_fl BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    <lwa_flot>-pltxt = lwa_iflotx-pltxt.
  ENDLOOP.

ENDFORM.                    " worklist_data_select

*&---------------------------------------------------------------------*
*&      Form  worklist_search
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_search  CHANGING lt_search_hdr_more TYPE /agri/t_glflot.

  REFRESH: gt_search_header, gt_search_text.
  CLEAR:   gt_search_header, gt_search_text.", gs_variables-wl_srch_count.

  SUBMIT /agri/glfl_process WITH p_excall = c_true     "#EC CI_SUBMIT
     VIA SELECTION-SCREEN AND RETURN.

  IMPORT /agri/glflot = gt_search_header[] FROM MEMORY ID 'GLFLOT_SRCH'.
  IMPORT /agri/glflotx = gt_search_text[] FROM MEMORY ID 'GLFLOTX_SRCH'.

  IF gt_search_header[] IS INITIAL.
    MESSAGE s751(/agri/global) INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF NOT lt_search_hdr_more IS INITIAL.
    APPEND LINES OF lt_search_hdr_more TO gt_search_header.
  ENDIF.

  PERFORM fl_data_display.

ENDFORM.                    " WORKLIST_SEARCH

*&---------------------------------------------------------------------*
*&      Form  worklist_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM worklist_update .

  DATA: lwa_iflotx          TYPE /agri/s_gliflotx,
        lwa_search_header   LIKE LINE OF gt_search_header,
        lwa_worklist_header LIKE LINE OF gt_worklist_header,
        lv_view.

  IF gs_variables-document_mode = c_mode_create.
    MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_search_header.

    READ TABLE gs_fldoc_infocus-x-iflotx INTO lwa_iflotx WITH KEY spras = sy-langu.
    IF lwa_iflotx-pltxt IS NOT INITIAL.
      lwa_search_header-pltxt = lwa_iflotx-pltxt.
    ENDIF.
    APPEND lwa_search_header TO gt_search_header.
  ELSE.
    lv_view = ref_worklist->view_in_focus_get( ).
    IF lv_view EQ 2.
      READ TABLE gt_search_header WITH KEY tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl
                                      TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_search_header.
        MODIFY gt_search_header FROM lwa_search_header INDEX sy-tabix.
      ENDIF.
    ELSEIF lv_view EQ 1.
      READ TABLE gt_worklist_header WITH KEY tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl
                                        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_worklist_header.
        MODIFY gt_worklist_header FROM lwa_worklist_header INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.

  gs_variables-worklist_refresh = c_true.

ENDFORM.                    " WORKLIST_UPDATE
