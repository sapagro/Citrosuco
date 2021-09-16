*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0S .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_set .

  DATA : lwa_tglfllvl      TYPE /agri/tglfllvl,
         lt_fcode_excludes TYPE ui_functions.

  IF NOT gs_fldoc_infocus-x-flhdr-fltyp IS INITIAL.
    lwa_tglfllvl-tplkz = gs_fldoc_infocus-x-flhdr-tplkz.
    lwa_tglfllvl-tplvl = gs_fldoc_infocus-x-flhdr-tplvl.
    key_text_get '/AGRI/TGLFLLVL'
                 'TPLVL'
                 lwa_tglfllvl-tplvl
                 lwa_tglfllvl
                 gs_variables-object_text.
  ELSE.
    gs_variables-object_text = TEXT-001.
  ENDIF.

  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN c_screen-overview.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-create_floc     OR c_screen-class_assignment
      OR c_screen-multi_lang_desc OR c_screen-change_superior_fl
      OR c_screen-gis_map OR c_screen-terrain_labels OR c_screen-hierarchy_display
      OR c_screen-new_label.
      SET PF-STATUS 'S201'.
    WHEN c_screen-reference_summary.
      SET PF-STATUS 'S319'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  SUBSCREEN_AREA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set .

  DATA: lv_routine(30) VALUE 'SUBSCREEN_AREA_SET_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program-funloc) IF FOUND.

ENDFORM.                    " SUBSCREEN_AREA_SET
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify .

  DATA: lv_perform(30) TYPE c VALUE 'SCREEN_MODIFY_'.

  CONCATENATE lv_perform sy-dynnr INTO lv_perform.

  LOOP AT SCREEN.
    PERFORM (lv_perform) IN PROGRAM (c_program-funloc)
                         IF FOUND.
    IF gs_variables-document_mode EQ c_mode_display.
      CHECK screen-group1 NE c_screen_group-display_only.

      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0302.

  IF /agri/s_glflot-swerk IS NOT INITIAL.
    IF screen-group1 EQ c_screen_group-company_code.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    "screen_modify_0302
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0301.

  IF gs_variables-owners_assigned IS NOT INITIAL.
    IF screen-name EQ '/AGRI/S_GLFLOT-OWROL' OR
       screen-name EQ '/AGRI/S_GLFLOT-OWNER'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    "screen_modify_0301
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0100.

  IF gs_fldoc_infocus IS INITIAL.
    gs_variables-main_screen = c_screen-dummy.
  ELSE.
    gs_variables-main_screen = c_screen-main_screen.
  ENDIF.

ENDFORM.                    "subscreen_area_set_0100
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0101.

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.

  gs_variables-subs_header = c_screen-dummy.
  gs_variables-subs_details = c_screen-dummy.

  IF gs_fldoc_infocus IS NOT INITIAL.
    IF gs_variables-header_display = c_true.
      gs_variables-subs_header = c_screen-header.
    ELSE.
      gs_variables-subs_header = c_screen-header_compressed.
    ENDIF.
    gs_variables-subs_details = c_screen-items.

    READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
               WITH KEY ts_fcode = ts_items-activetab.
    IF ls_tabstrip-local_fcode EQ c_fcode-tab_assignments.
      gs_variables-subs_details = c_screen-items_fullscr.
    ENDIF.

  ENDIF.

ENDFORM.                    "subscreen_area_set_0101
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0300.

  DATA: lv_active,
        ls_tabstrip TYPE /agri/s_gtabstrip,
        lv_descr    TYPE /agri/gdescr,
        lv_program  TYPE sy-repid,
        lv_screen   TYPE scradnum.

  DATA: lt_filter TYPE badi_filter_bindings,
        ls_filter TYPE badi_filter_binding.

  IF ts_items-activetab IS INITIAL.
    LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
      EXIT.
    ENDLOOP.
    ts_items-activetab = ls_tabstrip-ts_fcode.
  ENDIF.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
               WITH KEY ts_fcode = ts_items-activetab.

  gs_variables-program = c_program-funloc.
  gs_variables-subs_details = c_screen-dummy.

  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_details.
      gs_variables-subs_details = c_screen-general.
    WHEN c_fcode-tab_organization.
      gs_variables-subs_details = c_screen-organization.
    WHEN c_fcode-tab_address.
      gs_variables-subs_details = c_screen-address.
    WHEN c_fcode-tab_plants.
      gs_variables-subs_details = c_screen-plants.
    WHEN c_fcode-tab_partners.
*      gs_variables-subs_details = c_screen-partners.
      PERFORM partners_subscreen_set.
    WHEN c_fcode-tab_owners.
      gs_variables-subs_details = c_screen-owners.
    WHEN c_fcode-tab_classification.
      gs_variables-subs_details = c_screen-classification.
    WHEN c_fcode-tab_assignments.
      gs_variables-subs_details = c_screen-assignments.
*      PERFORM assignments_maintain.
    WHEN c_fcode-tab_texts.
      gs_variables-subs_details = c_screen-texts.
    WHEN c_fcode-tab_status.
      gs_variables-subs_details = c_screen-status.
*      PERFORM status_maintain.
    WHEN c_fcode-tab_notes.
      PERFORM notes_maintain.
    WHEN c_fcode-tab_additional_data.
      IF gt_additional_data IS NOT INITIAL.
        gs_variables-subs_details = c_screen-user_additional_data.
      ELSE.
        gs_variables-subs_details = c_screen-dummy.
      ENDIF.

    WHEN c_fcode-tab_additional_data2.
      ls_filter-name = 'FLTYP'.
      GET REFERENCE OF gs_fldoc_infocus-x-flhdr-fltyp INTO ls_filter-value.
****ESP6 Task#34370 - ATC correction (Post 1709 Corrections)
*    APPEND ls_filter TO lt_filter.
     INSERT  ls_filter INTO TABLE lt_filter.
****
      TRY.
          CALL METHOD cl_enh_badi_runtime_functions=>get_prog_and_dynp_for_subscr
            EXPORTING
              badi_name       = '/AGRI/BADI_GLFL_CS'
              calling_dynpro  = '0318'
              calling_program = c_program-funloc
              subscreen_area  = 'USRSSCR'
              filter_values   = lt_filter
            IMPORTING
              called_dynpro   = lv_screen
              called_program  = lv_program.
        CATCH cx_enh_badi_inconsistent .
        CATCH cx_enh_badi_no_such_extension .
        CATCH cx_enh_badi_not_found .
        CATCH cx_enh_badi_mulitple_impls .
        CATCH cx_enh_badi_filter_missing .
      ENDTRY.
      IF lv_program NE 'SAPLENH_BADI_SORTER_SUBSCREENS' AND
        lv_screen NE '1000' AND
        lv_program IS NOT INITIAL AND
        lv_screen  IS NOT INITIAL.
        gs_variables-customer_screen = lv_screen.
        gs_variables-customer_program = lv_program.
        gs_variables-subs_details = c_screen-adddata_cust_screen.
      ELSE.
        gs_variables-subs_details = c_screen-dummy.
      ENDIF.
    WHEN c_fcode-tab_admin.
      gs_variables-subs_details = c_screen-admin.
    WHEN OTHERS.
      gs_variables-subs_details = c_screen-dummy.
  ENDCASE.

  PERFORM notes_title_prepare.

ENDFORM.                    "subscreen_area_set_0300
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0314
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0314.
  PERFORM subscreen_area_set_0300.
ENDFORM.                    "subscreen_area_set_0314
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0300.

  DATA: lwa_tabstrip_fcodes TYPE /agri/s_gtabstrip.

  IF screen-name CP '/AGRI/S_GTABSTRIP*' .
    READ TABLE gt_tabstrip_fcodes INTO lwa_tabstrip_fcodes
                              WITH KEY tabname = screen-name.
    IF sy-subrc EQ 0 AND
       lwa_tabstrip_fcodes-invisible IS INITIAL.
      screen-invisible = 0.
    ELSE.
      screen-invisible = 1.
    ENDIF.

    IF gs_variables-document_mode = c_mode_create AND
       ( lwa_tabstrip_fcodes-local_fcode EQ c_fcode-tab_admin OR
         lwa_tabstrip_fcodes-local_fcode EQ
                                         c_fcode-tab_classification ).
      screen-active = 0.
    ENDIF.

    IF lwa_tabstrip_fcodes-local_fcode EQ c_fcode-tab_assignments.
      IF gs_variables-document_mode = c_mode_create.
        screen-active = 0.
      ELSE.
        READ TABLE gt_tglcsatl TRANSPORTING NO FIELDS
                           WITH KEY tplkz = /agri/s_glflot-tplkz
                                    tplvl = /agri/s_glflot-tplvl
                         BINARY SEARCH.
        IF sy-subrc NE 0.
          screen-active = 0.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
  MODIFY SCREEN.

ENDFORM.                    "screen_modify_0300
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0314
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0314.
  PERFORM screen_modify_0300.
ENDFORM.                    "screen_modify_0314
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0306.
  PERFORM admin_data_maintain.
ENDFORM.                    "subscreen_area_set_0306
*&---------------------------------------------------------------------*
*&      Form  sort_group_tables_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sort_group_tables_prepare CHANGING lt_sort TYPE lvc_t_sort
                                        lt_group_level TYPE lvc_t_fimg.

  DATA: lv_perform(30) VALUE 'SORT_GROUP_TABLES_PREPARE_'.

  lv_perform+26 = sy-dynnr.

  PERFORM (lv_perform) IN PROGRAM (c_program-funloc) IF FOUND
                        CHANGING lt_sort
                                 lt_group_level.

ENDFORM.                    "sort_group_tables_prepare
*&---------------------------------------------------------------------*
*&      Form  sort_group_tables_prepare_0309
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sort_group_tables_prepare_0309
                           CHANGING lt_sort TYPE lvc_t_sort
                                    lt_group_level TYPE lvc_t_fimg.

  DATA : lwa_sort        TYPE lvc_s_sort,
         lwa_group_level TYPE lvc_s_fimg.

  CLEAR lwa_sort.
  lwa_sort-spos = 1.
  lwa_sort-fieldname = 'CLASS'.
  lwa_sort-up = c_true.
  APPEND lwa_sort TO lt_sort.

ENDFORM.                    "sort_group_tables_prepare

FORM sort_group_tables_prepare_0100
                           CHANGING lt_sort TYPE lvc_t_sort
                                    lt_group_level TYPE lvc_t_fimg.

  DATA : lwa_sort        TYPE lvc_s_sort,
         lwa_group_level TYPE lvc_s_fimg.

  CLEAR lwa_sort.
  lwa_sort-spos = 1.
  lwa_sort-fieldname = 'STRNO'.
  lwa_sort-up = c_true.
  APPEND lwa_sort TO lt_sort.

ENDFORM.                    "sort_group_tables_prepare
*&---------------------------------------------------------------------*
*&      Form  STATUS_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_maintain .
*
*  DATA : lv_objnr TYPE onr00-objnr,
*         lv_mode LIKE t180-trtyp.
*
*  lv_mode = gs_variables-document_mode.
*  lv_objnr = gs_fldoc_infocus-x-iflot-objnr.
*
*****Release 60E Status Changes for BRF
*  IF ref_status_handler IS INITIAL.
*    CREATE OBJECT ref_status_handler.
*  ENDIF.
*****
*
*  CALL FUNCTION '/AGRI/G_STATUS_SUBSCREEN_IMPORT'
*    EXPORTING
*      i_mode                = lv_mode
*      i_objnr               = lv_objnr
*      i_no_user_status      = ' '
*      i_no_system_status    = c_true
*      iref_object           = ref_status_handler
*    CHANGING
*      c_change_flag         = gs_variables-data_changed
*      c_program             = gs_variables-program
*      c_subscreen           = gs_variables-subs_details
*    EXCEPTIONS
*      invalid_objnr         = 1
*      invalid_combination   = 2
*      no_fieldcatalog_found = 3
*      OTHERS                = 4.

ENDFORM.                    " STATUS_MAINTAIN

*&---------------------------------------------------------------------*
*&      Form  status_outcome_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_outcome_set USING lv_objnr.
  DATA:lt_messages TYPE /agri/t_gprolog,
       lwa_message TYPE /agri/s_gprolog.

  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.

  CALL FUNCTION '/AGRI/G_STATUS_FLOWOUTCOME_SET'
    EXPORTING
      i_objnr     = lv_objnr
*     IS_STEP_OUTCOME =
*     iref_object = ref_status_handler.
      iref_object = ref_status_handler
    IMPORTING
      et_messages = lt_messages.

  LOOP AT lt_messages INTO lwa_message.
****Message
    MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty NUMBER lwa_message-msgno
    WITH lwa_message-msgv1 lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
    INTO sy-msgli.
    message_simple space.

  ENDLOOP.

  CALL FUNCTION '/AGRI/G_STATUSPROF_TRIGGER_SET'
    EXPORTING
      i_objnr     = lv_objnr
*     I_TRIGGER   =
      iref_object = ref_status_handler.

ENDFORM.                    "STATUS_OUTCOME_SET
*&---------------------------------------------------------------------*
*&      Form  STATUS_OBJECT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_object_create USING lv_stsma
                       CHANGING lv_objnr.

  TYPES: lv_subrc TYPE sy-subrc,
         t_tj04   TYPE TABLE OF tj04.

  FIELD-SYMBOLS:<lt_tj04_buf> TYPE t_tj04.

  CHECK NOT lv_stsma IS INITIAL.

  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.

  CALL FUNCTION '/AGRI/G_STATUS_OBJECT_CREATE'
    EXPORTING
      i_obtyp             = 'XFL'
      i_stsma             = lv_stsma
*     I_CHGKZ             = 'X'
      iref_object         = ref_status_handler
    IMPORTING
      e_objnr             = lv_objnr
*     E_STONR             =
    EXCEPTIONS
      obtyp_invalid       = 1
      stsma_invalid       = 2
      stsma_obtyp_invalid = 3
      OTHERS              = 4.
  IF sy-subrc <> 0 AND NOT sy-msgid IS INITIAL
  AND NOT sy-msgno IS INITIAL AND NOT sy-msgty IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
  ENDIF.

*  ASSIGN ('(SAPLBSVA)TJ04_BUF[]') TO <lt_tj04_buf>.
*  IF sy-subrc NE 0.
*    PERFORM get_mess_active IN PROGRAM saplbsva IF FOUND.
**** 1610 Task
*    CALL FUNCTION 'STATUS_OBJECT_READ'
*      EXPORTING
*        objnr            = ''
*      EXCEPTIONS
*        object_not_found = 1
*        OTHERS           = 2.
****
*    ASSIGN ('(SAPLBSVA)TJ04_BUF[]') TO <lt_tj04_buf>.
*  ENDIF.
*
*  IF <lt_tj04_buf> IS ASSIGNED.
*    REFRESH: <lt_tj04_buf>.
*    SELECT * FROM tj04 INTO TABLE <lt_tj04_buf>
*                            WHERE obtyp EQ 'IFL'.
*  ENDIF.

ENDFORM.                    " STATUS_OBJECT_CREATE
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0303.

  gs_variables-program = c_program-address.
  gs_variables-subs_items = c_screen-address_fl.

ENDFORM.                    "subscreen_area_set_0303
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0308
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0308.

  gs_variables-program = c_program-partners.
  gs_variables-subs_items = c_screen-partners_fl.

ENDFORM.                    "subscreen_area_set_0308
*&---------------------------------------------------------------------*
*&      Form  STATUS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_check USING    lv_objnr TYPE j_objnr
                  CHANGING lv_cancel_change.

  CHECK lv_objnr IS NOT INITIAL.

  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
      bypass_buffer     = c_true
*     CLIENT            = SY-MANDT
      objnr             = lv_objnr
      status            = 'I0076'
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2
      OTHERS            = 3.
  IF sy-subrc <> 2.
    lv_cancel_change = c_true.
  ENDIF.

ENDFORM.                    " STATUS_CHECK

*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0310
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0310.
  PERFORM assignments_maintain.
ENDFORM.                    "subscreen_area_set_0310

*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0311
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0311.

  gs_variables-subs_class = c_screen-class_assignment.
  gs_variables-subs_items = c_screen-attributes.

ENDFORM.                    "subscreen_area_set_0311

*&---------------------------------------------------------------------*
*&      Form  SUPERIOR_FL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM superior_fl_check .

*  CALL FUNCTION 'FUNC_LOCATION_READ'
*    EXPORTING
**     AUTH_CHECK        = ' '
**     AUTH_TCODE        = 'IL03'
*      buffer_bypass     = c_true
**     DYFIELD           = ' '
**     NO_OTHER_LANGUAGE = ' '
**     SPRAS             = SY-LANGU
*      tplnr             = */agri/s_glflot-tplma
*    IMPORTING
**     IFLO_WA           =
*      pltxt             = /agri/s_glflscrfields-pltxt_ma
*    EXCEPTIONS
*      iflot_not_found   = 1
*      iloa_not_found    = 2
*      no_authority      = 3
*      OTHERS            = 4.
  IF sy-subrc <> 0.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
    MESSAGE e021(/agri/glfl) ."WITH */agri/s_glflot-tplma. "#EC*
*****
  ELSE.
    IF gs_fldoc_infocus-x-flhdr-tplma NE */agri/s_glflot-tplma.
      MESSAGE w133(il).
      gs_variables-data_changed = c_true.
      gs_fldoc_infocus-x-flhdr-tplma = */agri/s_glflot-tplma.
*      gs_fldoc_infocus-x-iflot-tplma = */agri/s_glflot-tplma.
      IF gs_fldoc_infocus-x-flhdr-updkz IS INITIAL.
        gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
      ENDIF.
*      IF gs_fldoc_infocus-x-iflot-updkz IS INITIAL.
*        gs_fldoc_infocus-x-iflot-updkz = c_updkz_update.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " superior_fl_check

*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0304
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0304.
  PERFORM status_subscreen_import.
ENDFORM.                    "subscreen_area_set_0304

*&---------------------------------------------------------------------*
*&      Form  STATUS_SUBSCREEN_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_subscreen_import .

  DATA : lv_objnr TYPE onr00-objnr,
         lv_mode  LIKE t180-trtyp.

  lv_mode  = gs_variables-document_mode.
  lv_objnr = gs_fldoc_infocus-x-flhdr-objnr.

  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.

  CALL FUNCTION '/AGRI/G_STATUS_SUBSCR_IMPORT'
    EXPORTING
      i_mode                = lv_mode
      i_objnr               = lv_objnr
      i_no_user_status      = ' '
      i_no_system_status    = c_true
      iref_object           = ref_status_handler
*     I_NO_SET_DATE         = ' '
*     I_NO_SET_TIME         = ' '
*     I_REFRESH_SYSTEM_STATUS       = 'X'
*     i_call_from_popup     =
*     I_PROGRAM_TOP         =
*     I_SUBS_TOP            =
* TABLES
*     T_SYSTEM_STATUS       =
    CHANGING
      c_change_flag         = gs_variables-data_changed
      c_program             = gs_variables-program
      c_subscreen           = gs_variables-subs_items
    EXCEPTIONS ##FM_SUBRC_OK
      invalid_objnr         = 1
      invalid_combination   = 2
      no_fieldcatalog_found = 3
      OTHERS                = 4.

ENDFORM.                    " STATUS_SUBSCREEN_IMPORT
*&---------------------------------------------------------------------*
*&      Form  STATUS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_update .

  DATA: lv_ustat TYPE j_status,
        lv_kfrst TYPE kfrst,
        lv_subrc TYPE sy-subrc.

  CHECK: gs_variables-data_changed  EQ c_true OR
         gs_variables-document_mode EQ c_mode_create.

  CHECK NOT gs_fldoc_infocus-x-flhdr-objnr IS INITIAL.

  CALL FUNCTION '/AGRI/G_STATUS_ACTIVE_GET'
    EXPORTING
      i_objnr          = gs_fldoc_infocus-x-flhdr-objnr
    IMPORTING
      e_current_status = lv_ustat
    EXCEPTIONS ##FM_SUBRC_OK
      object_not_found = 1
      OTHERS           = 2.

  IF lv_ustat NE gs_fldoc_infocus-x-flhdr-ustat.
    gs_fldoc_infocus-x-flhdr-ustat = lv_ustat.

    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.

    PERFORM release_status_determine USING lv_ustat
                                  CHANGING lv_kfrst.
    CHECK NOT lv_kfrst IS INITIAL.

    IF lv_kfrst EQ 'R'.
      lv_kfrst = space.
    ENDIF.

    CHECK gs_fldoc_infocus-x-flhdr-kfrst NE lv_kfrst.
    PERFORM rel_status_check_n_update USING lv_kfrst space
                                   CHANGING lv_subrc.

  ENDIF.

ENDFORM.                    " STATUS_UPDATE
*&---------------------------------------------------------------------*
*&      Form  SYSTEM_STATUS_TEXT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM system_status_text_get USING lv_objnr TYPE j_objnr
                            CHANGING lv_sstat_txt.

  DATA: lv_line TYPE bsvx-sttxt.

  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
*     CLIENT           = SY-MANDT
*     FLG_USER_STAT    = ' '
      objnr            = lv_objnr
*     ONLY_ACTIVE      = 'X'
      spras            = sy-langu
*     BYPASS_BUFFER    = ' '
    IMPORTING
*     ANW_STAT_EXISTING =
*     E_STSMA          =
      line             = lv_line
*     USER_LINE        =
*     STONR            =
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK lv_line IS NOT INITIAL.
  lv_sstat_txt = lv_line.

ENDFORM.                    " SYSTEM_STATUS_TEXT_GET
*&---------------------------------------------------------------------*
*&      Form  SELECTED_ROWS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selected_rows_get .

  DATA: lv_view          TYPE i,
        lt_selected_rows TYPE lvc_t_row WITH HEADER LINE,
        lwa_terrain      LIKE LINE OF gt_search_header.

  REFRESH gt_selected_terrains.

  IF lv_view IS INITIAL.
    lv_view = ref_worklist->view_in_focus_get( ).
  ENDIF.

  CALL METHOD ref_worklist->selected_rows_get
    IMPORTING
      et_hdr_rows = lt_selected_rows[].

  IF NOT lt_selected_rows[] IS INITIAL.
    LOOP AT lt_selected_rows.
      IF lv_view EQ 1.
        READ TABLE gt_worklist_header
        INTO lwa_terrain INDEX lt_selected_rows-index.
        IF sy-subrc EQ 0.
          APPEND lwa_terrain TO gt_selected_terrains.
        ENDIF.
      ELSE.
        READ TABLE gt_search_header
        INTO lwa_terrain INDEX lt_selected_rows-index.
        IF sy-subrc EQ 0.
          APPEND lwa_terrain TO gt_selected_terrains.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT gt_selected_terrains.
  DELETE ADJACENT DUPLICATES FROM gt_selected_terrains.

ENDFORM.                    " SELECTED_ROWS_GET
*&---------------------------------------------------------------------*
*& Form SORT_TABLE_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SORT
*&      --> LT_GROUP_LEVELS_LAYOUT
*&      --> LT_FCAT
*&---------------------------------------------------------------------*
FORM sort_table_build TABLES lt_sort TYPE lvc_t_sort
                             lt_group_levels_layout TYPE lvc_t_fimg
                             lt_fcat TYPE lvc_t_fcat.

  DATA: lwa_sort  TYPE lvc_s_sort,
        lwa_group TYPE lvc_s_fimg.

  FIELD-SYMBOLS: <lwa_fcat> TYPE lvc_s_fcat.

  DATA:lwa_style TYPE lvc_s_styl.

  lwa_sort-spos      =  1.
  lwa_sort-fieldname = 'TPLMA'.
  lwa_sort-up        = c_true.
  APPEND lwa_sort TO lt_sort.

  lwa_sort-spos      =  2.
  lwa_sort-fieldname = 'TPLNR_FL'.
  lwa_sort-up        = space.
  APPEND lwa_sort TO lt_sort.

  lwa_group-grouplevel = 'TPLMA'.
  lwa_group-n_image    = icon_closed_folder.
  lwa_group-exp_image  = icon_open_folder.
  lwa_group-style      = /agri/cl_alv_tree_simple_hier=>mc_style_link.
  APPEND lwa_group TO lt_group_levels_layout.

  CLEAR lwa_group.
  lwa_group-grouplevel = 'TPLNR_FL'.
  lwa_group-n_image    = icon_closed_folder.
  lwa_group-exp_image  = icon_open_folder.
  lwa_group-style      = /agri/cl_alv_tree_simple_hier=>mc_style_link.
  APPEND lwa_group TO lt_group_levels_layout.

  READ TABLE lt_fcat ASSIGNING <lwa_fcat>
  WITH KEY fieldname = 'TPLMA'.
  IF sy-subrc EQ 0.
    <lwa_fcat>-txt_field = 'TPLMA_TXT'.
  ENDIF.

  READ TABLE lt_fcat ASSIGNING <lwa_fcat>
  WITH KEY fieldname = 'TPLNR_FL'.
  IF sy-subrc EQ 0.
    <lwa_fcat>-txt_field = 'TPLNR_TXT'.
  ENDIF.

ENDFORM.
