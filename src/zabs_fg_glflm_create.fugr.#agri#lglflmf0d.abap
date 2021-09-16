*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0D .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM descriptions_display .

  CASE sy-dynnr.
    WHEN c_screen-create_floc OR
         c_screen-header OR
         c_screen-header_compressed OR
         c_screen-new_label.
      /agri/s_glflscrfields-fltyp_txt = gs_variables-object_text.
      IF /agri/s_glflscrfields-fltyp_txt IS INITIAL.
        /agri/s_glflscrfields-fltyp_txt = TEXT-001.
      ENDIF.
      PERFORM user_status_text_get
                             USING /agri/s_glflot-ustat
                                   /agri/s_glflot-stsma
                          CHANGING /agri/s_glflscrfields-ustat_txt.
*      PERFORM system_status_text_get
*                          USING    /agri/s_glflot-objnr
*                          CHANGING /agri/s_glflscrfields-sstat_txt.

      PERFORM fill_edit_mask.
    WHEN c_screen-general OR
         c_screen-change_superior_fl.
      PERFORM general_descriptions_fill.
  ENDCASE.

ENDFORM.                    " DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_lock  USING lv_tplnr TYPE /agri/gltplnr_fl
                                  lv_strno TYPE /agri/glstrno
                                  lv_message_type
                         CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  IF lv_tplnr IS NOT INITIAL.
    CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFL'
      EXPORTING
*       MODE_/AGRI/GLFLOT       = 'E'
*       MANDT          = SY-MANDT
        tplnr_fl       = lv_tplnr
*       X_TPLNR_FL     = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc IS NOT INITIAL.
      lv_subrc = sy-subrc.
      lv_msgv1 = sy-msgv1.
      MESSAGE ID '/AGRI/GLFL' TYPE lv_message_type
       NUMBER '011' WITH lv_tplnr lv_msgv1
         INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFLS'
    EXPORTING
*     MODE_/AGRI/S_GLFLOS_ENQ       = 'E'
*     MANDT          = SY-MANDT
      strno          = lv_strno
*     X_STRNO        = ' '
*     _SCOPE         = '2'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    MESSAGE ID '/AGRI/GLFL' TYPE lv_message_type
     NUMBER '011' WITH lv_strno lv_msgv1
       INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*&      Form  document_infocus_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock  USING lv_tplnr TYPE /agri/gltplnr_fl
                                    lv_strno TYPE /agri/glstrno.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLFL'
    EXPORTING
*     MODE_/AGRI/GLFLOT       = 'E'
*     MANDT    = SY-MANDT
      tplnr_fl = lv_tplnr
*     X_TPLNR_FL              = ' '
*     _SCOPE   = '3'
*     _SYNCHRON               = ' '
*     _COLLECT = ' '
    .

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLFLS'
    EXPORTING
*     MODE_/AGRI/S_GLFLOS_ENQ       = 'E'
*     MANDT = SY-MANDT
      strno = lv_strno
*     X_STRNO                       = ' '
*     _SCOPE                        = '3'
*     _SYNCHRON                     = ' '
*     _COLLECT                      = ' '
    .

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*&      Form  label_infocus_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM label_infocus_unlock USING lv_strno TYPE /agri/glstrno.

  CHECK: lv_strno IS NOT INITIAL.
*         gs_variables-labeling_active IS NOT INITIAL.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLFLS'
    EXPORTING
*     MODE_/AGRI/S_GLFLOS_ENQ       = 'E'
*     MANDT = SY-MANDT
      strno = lv_strno
*     X_STRNO                       = ' '
*     _SCOPE                        = '3'
*     _SYNCHRON                     = ' '
*     _COLLECT                      = ' '
    .

ENDFORM.                    "label_infocus_unlock
*&---------------------------------------------------------------------*
*&      Form  document_infocus_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_set  USING VALUE(lv_tplnr) TYPE /agri/gltplnr_fl.

  DATA: lv_activity(2)   TYPE c,
        lv_subrc         TYPE sy-subrc,
        lv_not_active    TYPE i,
        lv_cancel_change,
**** ESP6 Task #30035 - Global Text Engine Integration
*        lv_txtgr  TYPE txtgr.
        lv_txtgr         TYPE /agri/gtxtgr.
****

  PERFORM document_data_initialize USING c_true.

  IF lv_tplnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM document_infocus_read USING lv_tplnr.
  CHECK gs_fldoc_infocus IS NOT INITIAL.

  gs_variables-document_mode = gs_variables-overview_mode.

  IF gs_variables-document_mode = c_mode_display.

    lv_activity = c_authorization_activity-display.

  ELSE.

    lv_activity = c_authorization_activity-change.
*    PERFORM status_check USING gs_fldoc_infocus-x-flhdr-objnr
*                      CHANGING lv_cancel_change.
*    IF lv_cancel_change EQ c_true.
    IF gs_fldoc_infocus-x-flhdr-loevm EQ c_true.
      gs_variables-overview_mode = gs_variables-document_mode
                                 = c_mode_display.
      lv_activity = c_authorization_activity-display.
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success
          NUMBER '014' WITH gs_fldoc_infocus-x-flhdr-tplnr_fl
            INTO sy-msgli.
      message_simple space.
    ENDIF.

  ENDIF.

  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                lv_activity
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    IF lv_activity EQ c_authorization_activity-change.
      gs_variables-overview_mode = gs_variables-document_mode
                                 = c_mode_display.
    ELSE.
      gs_variables-errors = c_true.
      PERFORM document_data_initialize USING c_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_variables-document_mode = c_mode_change.
    PERFORM document_infocus_lock USING lv_tplnr
                                        gs_fldoc_infocus-x-flhdr-strno
                                        c_msg_type-info
                               CHANGING lv_subrc.
    IF lv_subrc <> 0.
      gs_variables-document_mode =
      gs_variables-overview_mode = c_mode_display.
    ENDIF.
  ENDIF.
*  PERFORM funloc_category_control_read
*                                 USING gs_fldoc_infocus-x-flhdr-fltyp.
  PERFORM funloc_control_read USING gs_fldoc_infocus-x-flhdr-tplkz
                                    gs_fldoc_infocus-x-flhdr-tplvl.
  PERFORM attr_infocus_set.

  IF gs_fldoc_infocus-x-flhdr-pargr IS INITIAL.
    gs_fldoc_infocus-x-flhdr-pargr = gs_tglfllvl-pargr.
  ENDIF.

  IF gs_fldoc_infocus-x-flhdr-txtgr IS NOT INITIAL.
    lv_txtgr = gs_fldoc_infocus-x-flhdr-txtgr.
  ELSE.
    lv_txtgr = gs_tglfllvl-txtgr.
  ENDIF.
  IF gs_fldoc_infocus-x-flown IS NOT INITIAL.
    gs_variables-owners_assigned = c_true.
  ENDIF.

  object_refresh_all.
  object_publish c_object-bor gs_fldoc_infocus-tplnr_fl.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING lv_txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_fldoc_infocus-x-flhdr-tplnr_fl.

  PERFORM address_init.

  PERFORM partners_init.

  PERFORM assignments_init.

ENDFORM.                    " document_infocus_set

*&---------------------------------------------------------------------*
*&      Form  document_infocus_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_prepare.

  DATA: lv_subrc TYPE sy-subrc,
        lv_msgv1 TYPE sy-msgv1,
        ls_flos  TYPE /agri/s_glflos.

  CHECK gs_variables-document_mode EQ c_mode_create.

  PERFORM document_infocus_lock
                          USING gs_fldoc_infocus-x-flhdr-tplnr_fl
                                gs_fldoc_infocus-x-flhdr-strno
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
*    PERFORM label_infocus_unlock USING gs_fldoc_infocus-x-flhdr-strno.
    CLEAR: gs_variables-document_mode,
           gs_fldoc_infocus, /agri/s_glflot.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                c_authorization_activity-create
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
*    PERFORM label_infocus_unlock USING gs_fldoc_infocus-x-flhdr-strno.
    CLEAR: gs_variables-document_mode,
           gs_fldoc_infocus, /agri/s_glflot.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

  PERFORM funloc_control_read USING gs_fldoc_infocus-x-flhdr-tplkz
                                    gs_fldoc_infocus-x-flhdr-tplvl.

  gs_fldoc_infocus-x-flhdr-owrol = gs_tglfllvl-owrol.
  gs_fldoc_infocus-x-flhdr-fltyp = gs_tglfllvl-fltyp.

  PERFORM funloc_data_prepare CHANGING gs_fldoc_infocus-x-flhdr.
*                                       gs_fldoc_infocus-x-iflot
*                                       gs_fldoc_infocus-x-iloa.

  gs_fldoc_infocus-x-flhdr-stsma = gs_tglfllvl-stsma.
  PERFORM status_object_create USING gs_fldoc_infocus-x-flhdr-stsma
                            CHANGING gs_fldoc_infocus-x-flhdr-objnr.
  PERFORM status_update.
  gs_fldoc_infocus-x-flhdr-pargr = gs_tglfllvl-pargr.

***Label Update
  ls_flos-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
  ls_flos-tplkz = gs_fldoc_infocus-x-flhdr-tplkz.
  ls_flos-strno = gs_fldoc_infocus-x-flhdr-strno.
  ls_flos-updkz = c_updkz_new.
  ls_flos-erdat = sy-datum.
  ls_flos-ernam = sy-uname.
  APPEND ls_flos TO gs_fldoc_infocus-x-flos.

*--Initialize address data.
  PERFORM address_init.

  PERFORM partners_init.

  PERFORM assignments_init.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING gs_tglfllvl-txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_fldoc_infocus-x-flhdr-tplnr_fl.

ENDFORM.                    "document_infocus_prepare
*&---------------------------------------------------------------------*
*&      Form  document_data_initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_data_initialize  USING lv_refresh_messages.

*  DATA: lwa_ilox_tplnr TYPE ilox_tplnr,
*        lt_ilox_tplnr  TYPE ilox_t_tplnr.
*
*  lwa_ilox_tplnr-tplnr = gs_fldoc_infocus-x-flhdr-tplnr_fl.
*  APPEND lwa_ilox_tplnr TO lt_ilox_tplnr.

  CLEAR: gv_address_handle.

  CLEAR: gs_fldoc_infocus,
         gt_csdoc_infocus,
         gs_variables-document_mode,
         gs_variables-data_changed,
         gs_variables-owners_assigned,
         gs_variables-errors.

  REFRESH: gt_athdr,
           gt_owner_mod_rows.

  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

  object_refresh_all.

  CALL FUNCTION '/AGRI/G_STATUS_INIT'.
*  CALL FUNCTION 'ILOX_IFLOS_BUFFER_REFRESH'
*    CHANGING
*      ct_tplnr = lt_ilox_tplnr.

  gs_variables-refresh_attributes  = c_true.
  gs_variables-refresh_plants_grid = c_true.
  gs_variables-refresh_class_grid  = c_true.
  gs_variables-refresh_desc_grid   = c_true.
  gs_variables-refresh_owners_grid = c_true.
  gs_variables-refresh_assignments = c_true.
  gs_variables-refresh_additional_data_grid = c_true.
  gs_variables-refresh_hierarchy_grid = c_true.
  gs_variables-refresh_label_grid = c_true.
  gs_variables-refresh_extdfl_tree = c_true.

ENDFORM.                    "document_data_initialize

*&---------------------------------------------------------------------*
*&      Form  document_infocus_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_read USING lv_tplnr.

  DATA: lt_tplnr  TYPE /agri/t_gltplnr,
        lt_fl_doc TYPE /agri/t_glfl_doc.

  APPEND lv_tplnr TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc EQ 0.
    READ TABLE lt_fl_doc INTO gs_fldoc_infocus INDEX 1.
  ENDIF.

ENDFORM.                    "document_infocus_read

*&---------------------------------------------------------------------*
*&      Form  document_infocus_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_save USING lv_set_infocus.

  DATA: "lt_fldoc      TYPE /agri/t_glfl_doc,
    lt_messages TYPE /agri/t_gprolog,
    ls_message  TYPE /agri/s_gprolog,
    ls_variant  TYPE disvariant,
    lv_subrc    TYPE sysubrc.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    gs_fldoc_infocus-x-flhdr.

  PERFORM funloc_infocus_save CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc = 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    IF lv_set_infocus EQ c_true.
      PERFORM document_infocus_set USING
      gs_fldoc_infocus-x-flhdr-tplnr_fl.
    ENDIF.
  ENDIF.

ENDFORM.                    "document_infocus_save

*&---------------------------------------------------------------------*
*&      Form  document_infocus_clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_CONTINUE  text
*----------------------------------------------------------------------*
FORM document_infocus_clear  CHANGING lv_continue.

  DATA: lv_answer(1).

  IF gs_variables-exit_after_save IS NOT INITIAL.

    PERFORM document_infocus_save USING space.

    IF gs_variables-exit_after_save EQ c_true.
      CLEAR gs_variables-exit_after_save.
      SET SCREEN 0.
      LEAVE SCREEN.
    ELSEIF gs_variables-exit_after_save EQ 'C'.
      CLEAR: gs_variables-exit_after_save.
      PERFORM document_data_initialize USING c_true.
      gs_variables-document_mode = c_mode_display.
    ENDIF.

  ELSE.
    PERFORM changes_confirm   CHANGING lv_answer.
    IF lv_answer EQ 'A'.
      IF ok_code = c_fcode-save.
        PERFORM document_infocus_save USING space.
        PERFORM document_data_initialize USING c_true.
        CLEAR ok_code.
      ELSE.
        CLEAR ok_code.
        lv_continue = c_false.
        EXIT.
      ENDIF.
    ENDIF.
    PERFORM document_infocus_unlock USING gs_fldoc_infocus-tplnr_fl
                                          gs_fldoc_infocus-x-flhdr-strno.
    PERFORM document_data_initialize USING c_true.
    gs_variables-document_mode = c_mode_display.
  ENDIF.

ENDFORM.                    "document_infocus_clear

*&---------------------------------------------------------------------*
*&      Form  display_fl_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_TPLNR   text
*----------------------------------------------------------------------*
FORM display_fl_hierarchy CHANGING lv_no_data_exists.

  DATA: lt_reference_docs TYPE /agri/t_glflot.

*  DATA: lv_selmode TYPE rihea-selmo VALUE '1'.
*
*  CALL FUNCTION 'PM_HIERARCHY_CALL'
*    EXPORTING
**     DATUM                = SY-DATUM
**     EQUNR                = ' '
**     GRAFICS              = ' '
**     LEVDO                = '01'
**     LEVUP                = '00'
**     MATNR                = ' '
**     READ_NEW             = ' '
**     SANIN                = 'X'
**     SELECT_EQUI          = ' '
*      select_iflo          = c_true
**     SELECT_STPO          = ' '
*      selmod               = lv_selmode
**     STKKZ                = ' '
**     TECHV                = ' '
*      tplnr                = lv_tplnr
**     WERKS                = ' '
**     WITH_EQUI            = ' '
**     WITH_EQUI_HIER       = ' '
*      with_iflo_hier       = c_true
**     WITH_BTYP            = 'X'
**     WITH_MARA            = ' '
**     WITH_IBASE_HIER      = ' '
**     CAPID                = ' '
**     EMENG                = 0
**     ARBPL_INT            = ' '
**     AUTHORITY_CHECK      = 'X'
**     I_GRBOM              =
**   IMPORTING
**     ET_HIER              =
**   TABLES
**     EQUI_TAB             =
**     IFLO_TAB             =
**     STPO_TAB             =
*    EXCEPTIONS
*      no_hierarchy         = 1
*      no_object_defined    = 2
*      no_selection         = 3
*      no_valid_equnr       = 4
*      no_valid_matnr       = 5
*      no_valid_selmod      = 6
*      no_valid_tplnr       = 7
*      no_valid_grbom_werks = 8
*      OTHERS               = 9.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

  PERFORM exthier_tree_prepare USING lt_reference_docs[].

  IF gt_glflot_buffer[] IS INITIAL.
    lv_no_data_exists = c_true.
  ENDIF.

ENDFORM.                    "display_fl_hierarchy
*&---------------------------------------------------------------------*
*&      Form  DESC_STYLES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_styles_prepare  CHANGING lwa_fldesc_layout
                              TYPE /agri/s_gliflotx_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'SPRAS'.
  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lwa_style INTO TABLE lwa_fldesc_layout-styles.

ENDFORM.                    " DESC_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  DESC_GRID_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_grid_update .

  DATA: lv_modified,
        lv_subrc,
        lv_valid.

  IF ok_code EQ c_fcode-desc_delete.
    PERFORM fcode_desc_delete.
    CLEAR ok_code.
  ENDIF.

  lv_modified = ref_multi_lang_desc_grid->data_modified_check( ).
  IF lv_modified EQ c_true.
    CALL METHOD ref_multi_lang_desc_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_desc_grid = c_true.
    ENDIF.

  ENDIF.

  PERFORM desc_update CHANGING lv_subrc.

ENDFORM.                    " DESC_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  DESC_DUPLICATES_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_duplicates_check
              USING lwa_fldesc TYPE /agri/s_gliflotx
                    lv_subrc.

  DATA: lwa_fldesc_tmp TYPE /agri/s_gliflotx,
        lv_cnt         TYPE i,
        lv_error.

  IF lwa_fldesc-spras IS INITIAL.
    lv_error = c_true.
    MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '004' INTO sy-msgli.
    message_simple space.
  ENDIF.

  LOOP AT gs_fldoc_infocus-x-iflotx INTO lwa_fldesc_tmp
                           WHERE spras EQ lwa_fldesc-spras.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_error = c_true.
      MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '005' INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDLOOP.

  IF lv_error IS NOT INITIAL.
    lv_subrc = 4.
  ENDIF.

ENDFORM.                    " DESC_DUPLICATES_CHECK
*&---------------------------------------------------------------------*
*&      Form  DESC_DATA_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_data_fill .

  FIELD-SYMBOLS: <lwa_fldesc> TYPE /agri/s_gliflotx.
  DATA: lwa_fldesc TYPE /agri/s_gliflotx.

  CHECK gs_fldoc_infocus IS NOT INITIAL.

  gs_variables-refresh_desc_grid = c_true.

  IF /agri/s_glflot-pltxt IS INITIAL.
    READ TABLE gs_fldoc_infocus-y-iflotx ASSIGNING <lwa_fldesc>
                                      WITH KEY spras = sy-langu
                                               tplnr_fl = gs_fldoc_infocus-tplnr_fl.
    IF sy-subrc EQ 0.
      <lwa_fldesc>-updkz = c_updkz_delete.
    ENDIF.
    DELETE gs_fldoc_infocus-x-iflotx WHERE spras EQ sy-langu.
  ELSE.

    READ TABLE gs_fldoc_infocus-x-iflotx ASSIGNING <lwa_fldesc>
                                        WITH KEY spras = sy-langu.

    IF sy-subrc EQ 0.
      <lwa_fldesc>-pltxt = /agri/s_glflot-pltxt.
      IF <lwa_fldesc>-updkz NE c_updkz_new.
        <lwa_fldesc>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      lwa_fldesc-spras = sy-langu.
      lwa_fldesc-pltxt = /agri/s_glflot-pltxt.
      lwa_fldesc-tplnr_fl = /agri/s_glflot-tplnr_fl.
      lwa_fldesc-updkz = c_updkz_new.
      APPEND lwa_fldesc TO gs_fldoc_infocus-x-iflotx.
    ENDIF.
  ENDIF.

  gs_variables-refresh_desc_grid = c_true.

ENDFORM.                    " DESC_DATA_FILL
*&---------------------------------------------------------------------*
*&      Form  dropdown_tables_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_tables_display .

  DATA lv_perform(30) VALUE 'DROPDOWN_TABLES_DISPLAY_'.

  CONCATENATE lv_perform sy-dynnr INTO lv_perform.
  PERFORM (lv_perform) IN PROGRAM (c_program-funloc) IF FOUND.

ENDFORM.                    " dropdown_tables_display
*&---------------------------------------------------------------------*
*&      Form  dropdown_tables_display_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_tables_display_0200.
  PERFORM dropdown_tables_display_0201.
ENDFORM.                    "dropdown_tables_display_0200
*&---------------------------------------------------------------------*
*&      Form  dropdown_tables_display_0201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_tables_display_0201.

  DATA: lt_dropdown_values TYPE vrm_values,
        ls_dropdown_value  TYPE vrm_value,
        lwa_tgl370s        TYPE /agri/tgl370s,
        lwa_t370s_t        TYPE t370s_t,
        lwa_tglfllvl       TYPE /agri/tglfllvl,
        lwa_tglfllvl_t     TYPE /agri/tglfllvlt.

  STATICS: lv_tplkz      TYPE tplkz,
           lt_t370s_t    TYPE TABLE OF t370s_t,
           lt_tgl370s    TYPE TABLE OF /agri/tgl370s,
           lt_tglfllvl   TYPE TABLE OF /agri/tglfllvl,
           lt_tglfllvl_t TYPE TABLE OF /agri/tglfllvlt.
*
*  IF lt_tgl370s[] IS INITIAL.
*    SELECT * FROM /agri/tgl370s
*           INTO TABLE lt_tgl370s.
*    IF sy-subrc EQ 0.
*      SORT lt_tgl370s BY tplkz.
*      SELECT * FROM t370s_t
*               INTO TABLE lt_t370s_t
*                FOR ALL ENTRIES IN lt_tgl370s
*              WHERE spras = sy-langu
*                AND tplkz = lt_tgl370s-tplkz.
*      IF sy-subrc = 0.
*        SORT lt_t370s_t BY tplkz.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  IF lv_tplkz NE /agri/s_glflot-tplkz.
    lv_tplkz = /agri/s_glflot-tplkz.
    SELECT * FROM /agri/tglfllvl    "#EC CI_GENBUFF
           INTO TABLE lt_tglfllvl
          WHERE tplkz EQ lv_tplkz.
    IF sy-subrc EQ 0.
      SORT lt_tglfllvl BY tplvl.
      SELECT * FROM /agri/tglfllvlt
               INTO TABLE lt_tglfllvl_t
                FOR ALL ENTRIES IN lt_tglfllvl
              WHERE spras = sy-langu
                AND tplkz = lt_tglfllvl-tplkz
                AND tplvl = lt_tglfllvl-tplvl.
      IF sy-subrc = 0.
        SORT lt_tglfllvl_t BY tplkz tplvl.
      ENDIF.
    ENDIF.
  ENDIF.

*  LOOP AT lt_tgl370s INTO lwa_tgl370s.
*
*    CLEAR: lwa_t370s_t, ls_dropdown_value.
*    READ TABLE lt_t370s_t INTO lwa_t370s_t
*                      WITH KEY tplkz = lwa_tgl370s-tplkz
*                        BINARY SEARCH.
*    ls_dropdown_value-key = lwa_tgl370s-tplkz.
*    ls_dropdown_value-text = lwa_t370s_t-tplxt.
*    APPEND ls_dropdown_value TO lt_dropdown_values.
*
*  ENDLOOP.
*
*  dropdowns_fill lt_dropdown_values[] c_dropdown_id-struct_ind.

  REFRESH: lt_dropdown_values[].
  LOOP AT lt_tglfllvl INTO lwa_tglfllvl.

    CLEAR: lwa_t370s_t, ls_dropdown_value.
    READ TABLE lt_tglfllvl_t INTO lwa_tglfllvl_t
                      WITH KEY tplkz = lv_tplkz
                               tplvl = lwa_tglfllvl-tplvl
                        BINARY SEARCH.
    ls_dropdown_value-key = lwa_tglfllvl-tplvl.
    ls_dropdown_value-text = lwa_tglfllvl_t-descr.
    APPEND ls_dropdown_value TO lt_dropdown_values.

  ENDLOOP.
  dropdowns_fill lt_dropdown_values[] c_dropdown_id-terrain_level.

ENDFORM.                    "dropdown_tables_display_0201
*&---------------------------------------------------------------------*
*&      Form  dropdown_tables_display_0309
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_tables_display_0309.

  DATA: lt_dropdown_values TYPE vrm_values,
        ls_dropdown_value  TYPE vrm_value,
        lwa_glagha         TYPE /agri/s_glagha.

  FIELD-SYMBOLS: <lwa_glagha> TYPE /agri/s_glagha.

  IF gt_atgrp IS INITIAL.
    SELECT * FROM /agri/glagha "#EC CI_ALL_FIELDS_NEEDED
      INTO CORRESPONDING FIELDS OF TABLE gt_atgrp
     WHERE klart EQ c_agtyp-functional_location.
    IF sy-subrc EQ 0.
      SORT gt_atgrp BY class.
    ENDIF.
    LOOP AT gt_atgrp ASSIGNING <lwa_glagha>.
      CALL FUNCTION 'CLMA_CLASS_EXIST'
        EXPORTING
          class             = <lwa_glagha>-class
*         CLASSIFY_ACTIVITY = ' '
*         CLASSNUMBER       = ' '
          classtype         = <lwa_glagha>-klart
*         DATE              = SY-DATUM
          description_only  = c_true
*         LANGUAGE          = SY-LANGU
*         MODE              = ' '
*         NO_DESCRIPTION    = ' '
*         CLASS_ACTIVITY    =
*         BYPASSING_BUFFER  = ' '
        IMPORTING
          class_description = <lwa_glagha>-klbez
*         CLASS_LANGUAGE    =
*         NOT_VALID         =
*         NO_ACTIVE_STATUS  =
*         NO_AUTHORITY_CLASSIFY =
*         NO_AUTHORITY_MAINTAIN =
*         NO_AUTHORITY_SELECT   =
*         RET_CODE          =
*         XKLAH             =
*         E_BUFFER_COUNT    =
        EXCEPTIONS
          no_valid_sign     = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDLOOP.
  ENDIF.

  LOOP AT gt_atgrp INTO lwa_glagha.
    ls_dropdown_value-key = lwa_glagha-class.
    ls_dropdown_value-text = lwa_glagha-klbez.
    APPEND ls_dropdown_value TO lt_dropdown_values.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DESCRIBE TABLE gt_atgrp.
    IF sy-tfill GT 1.
      CLEAR ls_dropdown_value.
      ls_dropdown_value-key = 'ALL'.
      APPEND ls_dropdown_value TO lt_dropdown_values.
    ENDIF.
  ENDIF.

  dropdowns_fill lt_dropdown_values[] c_dropdown_id-class.

ENDFORM.                    "dropdown_tables_display_0309
*&---------------------------------------------------------------------*
*&      Form  DESC_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_update CHANGING lv_subrc.

  DATA: lwa_fldesc        TYPE /agri/s_gliflotx,
        lwa_fldesc_layout TYPE /agri/s_gliflotx_fcat,
        lwa_mod_row       TYPE lvc_s_modi.

  FIELD-SYMBOLS: <lwa_fldesc> TYPE /agri/s_gliflotx.

  LOOP AT gt_desc_mod_rows INTO lwa_mod_row.

    READ TABLE gt_fl_desc_layout INTO lwa_fldesc_layout INDEX lwa_mod_row-row_id.
    CHECK lwa_fldesc_layout IS NOT INITIAL.

    MOVE-CORRESPONDING lwa_fldesc_layout TO lwa_fldesc.
    PERFORM desc_duplicates_check USING lwa_fldesc lv_subrc.

    CHECK lv_subrc EQ 0.

    READ TABLE gs_fldoc_infocus-x-iflotx ASSIGNING <lwa_fldesc>
                                         WITH KEY spras = lwa_fldesc_layout-spras
                                                  tplnr_fl = lwa_fldesc_layout-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_fldesc NE <lwa_fldesc>.
        MOVE lwa_fldesc TO <lwa_fldesc>.
        IF <lwa_fldesc>-updkz NE c_updkz_new.
          gs_variables-data_changed = c_true.
          <lwa_fldesc>-updkz = c_updkz_update.
        ENDIF.
**** updating header simultaneously
        IF sy-langu = lwa_fldesc-spras.
          gs_fldoc_infocus-x-flhdr-pltxt = lwa_fldesc-pltxt.
        ENDIF.
      ENDIF.
    ELSE.
**** updating header simultaneously
      IF sy-langu = lwa_fldesc-spras.
        gs_fldoc_infocus-x-flhdr-pltxt = lwa_fldesc-pltxt.
      ENDIF.
      gs_variables-refresh_desc_grid = c_true.
      gs_variables-data_changed = c_true.
      lwa_fldesc_layout-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
      lwa_fldesc_layout-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_fldesc_layout TO lwa_fldesc.
      APPEND lwa_fldesc TO gs_fldoc_infocus-x-iflotx.
    ENDIF.

    DELETE gt_desc_mod_rows WHERE row_id EQ lwa_mod_row-row_id.

  ENDLOOP.

ENDFORM.                    " DESC_UPDATE
