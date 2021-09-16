*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0A .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM admin_data_maintain .

  DATA: lv_subobj LIKE dd03p-fieldname,
        ls_screenfields TYPE /agri/gadminscrfields.

  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO ls_screenfields.
  CALL FUNCTION '/AGRI/GADMIN_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp              = c_object-bor
      i_objkey              = gs_fldoc_infocus-x-flhdr-tplnr_fl
      i_subobj              = lv_subobj
      is_scrfields          = ls_screenfields
      i_suppress_activities = c_true
      i_suppress_notes      = c_false
    CHANGING
      c_program             = gs_variables-admin_program
      c_subscreen           = gs_variables-subscr_admin.

ENDFORM.                    " ADMIN_DATA_MAINTAIN

*&---------------------------------------------------------------------*
*&      Form  authority_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check USING lwa_flhdr TYPE /agri/s_glflot
                           lv_activity
                          lv_display_messages
                  CHANGING lv_subrc  TYPE sy-subrc.

  PERFORM fl_authority_check USING lwa_flhdr
                                   lv_activity
                          CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    CASE lv_activity.
      WHEN c_authorization_activity-create.
        MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '032'
        INTO sy-msgli.
      WHEN c_authorization_activity-change.
        MESSAGE ID '/AGRI/GLFL' TYPE 'S' NUMBER '033'
        INTO sy-msgli.
      WHEN c_authorization_activity-display.
        MESSAGE ID '/AGRI/GLFL' TYPE 'S' NUMBER '034'
        INTO sy-msgli.
      WHEN c_authorization_activity-delete.
        MESSAGE ID '/AGRI/GLFL' TYPE 'S' NUMBER '038'
        INTO sy-msgli.
    ENDCASE.
    message_simple space.
  ENDIF.

  CHECK lv_subrc EQ 0.
*--Call Terrian authority check.
  PERFORM badi_authority_check USING lwa_flhdr
                                     lv_activity
                                     lv_display_messages
                            CHANGING lv_subrc.

ENDFORM.                    "authority_check
*&---------------------------------------------------------------------*
*&      Form  ATTR_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attr_grid_data_prepare .

  DATA: lwa_athdr TYPE /agri/s_gathdr,
        lwa_attr_vals TYPE /agri/s_glflatv_fcat.

  CHECK ref_attributes_grid IS INITIAL OR
        gs_variables-refresh_attributes IS NOT INITIAL.

  REFRESH: gt_attr_vals.
  LOOP AT gt_athdr INTO lwa_athdr.
    PERFORM attribute_data_prepare USING lwa_athdr
                                CHANGING lwa_attr_vals.
    APPEND lwa_attr_vals TO gt_attr_vals.
  ENDLOOP.

ENDFORM.                    " ATTR_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  attr_infocus_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attr_infocus_set.

  DATA: lwa_flatg TYPE /agri/s_glflatg.

  REFRESH: gt_athdr.
  LOOP AT gs_fldoc_infocus-x-flatg INTO lwa_flatg.
    PERFORM class_attributes_read USING c_agtyp-functional_location
                                        lwa_flatg-clint
                                        lwa_flatg-class
                               CHANGING gt_athdr.
  ENDLOOP.

ENDFORM.                    "attr_infocus_set
*&---------------------------------------------------------------------*
*&      Form  ATTR_VALUES_F4_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attr_values_f4_get  USING    lv_tabix
                         CHANGING lv_value
                                  lv_change.

  DATA: lv_display_only,
        lt_values TYPE TABLE OF cawn,
        lwa_attr_vals TYPE /agri/s_glflatv_fcat.

  READ TABLE gt_attr_vals INTO lwa_attr_vals INDEX lv_tabix.
  CHECK sy-subrc = 0.

  IF gs_variables-document_mode = c_mode_display.
    lv_display_only = c_true.
  ENDIF.

  CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
    EXPORTING
      i_atinn               = lwa_attr_vals-atinn
*     I_ATNAM               =
*     I_DISPLAY_ONLY        = ' '
*     I_ONLY_ALLOWED_VALUES = ' '
*     I_SHOW_DESCRIPTION    = 'X'
*     I_MULTIPLE_VALUES     = ' '
*     IT_SEL_VALUE          =
    IMPORTING
      e_value               = lv_value
    TABLES
      t_values              = lt_values
    EXCEPTIONS
      charact_not_found     = 1
      no_values_found       = 2
      OTHERS                = 3.
  IF sy-subrc EQ 0.
    lv_change = c_true.
  ENDIF.

ENDFORM.                    " ATTR_VALUES_F4_GET
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTES_GRID_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attributes_grid_update .

  DATA: lv_valid,
        lv_modified,
        lv_continue,
        lv_subrc TYPE sy-subrc,
        lv_error.

  CHECK gs_variables-document_mode NE c_mode_display
  AND gs_variables-document_mode NE space.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_fldoc_infocus-x-flhdr.


  CALL METHOD ref_attributes_grid->data_modified_check
    RECEIVING
      e_modified = lv_modified.

  IF lv_modified EQ c_true OR
     gs_variables-attr_manual_changes EQ c_true.

    CALL METHOD ref_attributes_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.

    CHECK lv_valid EQ c_true.

    PERFORM attributes_data_update CHANGING gt_attr_vals[]
                                            lv_subrc.

    IF lv_subrc EQ 0.
      gs_variables-refresh_attributes = c_true.
      CLEAR: gs_variables-attr_manual_changes.
    ENDIF.
  ENDIF.

  IF gs_variables-errors EQ c_true.
    CLEAR ok_code.
  ENDIF.
  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.                    " ATTRIBUTES_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTES_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attributes_data_update
                   CHANGING lt_attributes TYPE /agri/t_glflatv_fcat
                            lv_subrc TYPE sy-subrc.

  DATA: lv_tabix TYPE sy-tabix,
        lwa_flatv_old TYPE /agri/s_glflatv,
        lwa_flatv TYPE /agri/s_glflatv.

  FIELD-SYMBOLS: <lwa_flatv>      TYPE /agri/s_glflatv,
                 <lwa_flatg>      TYPE /agri/s_glflatg,
                 <lwa_attributes> TYPE /agri/s_glflatv_fcat.

  LOOP AT lt_attributes ASSIGNING <lwa_attributes>.

    CLEAR: lwa_flatv_old, lwa_flatv.
    CHECK <lwa_attributes>-atwrt NE <lwa_attributes>-atwrt_old.

    READ TABLE gs_fldoc_infocus-x-flatv INTO lwa_flatv_old
                               WITH KEY clint = <lwa_attributes>-clint
                                        atinn = <lwa_attributes>-atinn.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      lwa_flatv = lwa_flatv_old.
    ENDIF.

    MOVE-CORRESPONDING <lwa_attributes> TO lwa_flatv.

    PERFORM attribute_update USING lwa_flatv_old
                          CHANGING lwa_flatv
                                   lv_subrc.

    IF lv_subrc NE 0.
      CONTINUE.
    ELSE.
      IF lv_tabix IS NOT INITIAL.
        READ TABLE gs_fldoc_infocus-x-flatv ASSIGNING <lwa_flatv>
                              INDEX lv_tabix.
        MOVE-CORRESPONDING lwa_flatv TO <lwa_flatv>.
        IF <lwa_flatv>-updkz NE c_updkz_new.
          <lwa_flatv>-updkz = c_updkz_update.
          gs_variables-data_changed = c_true.
        ENDIF.
      ELSE.
        lwa_flatv-updkz = c_updkz_new.
        lwa_flatv-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
        gs_variables-data_changed = c_true.
        APPEND lwa_flatv TO gs_fldoc_infocus-x-flatv.
      ENDIF.
      READ TABLE gs_fldoc_infocus-x-flatg ASSIGNING <lwa_flatg>
                                     WITH KEY clint = lwa_flatv-clint.
      IF sy-subrc EQ 0 AND <lwa_flatg>-updkz NE c_updkz_new.
        <lwa_flatg>-updkz = c_updkz_update.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ATTRIBUTES_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  attribute_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attribute_check USING lwa_athdr     TYPE /agri/s_gathdr
                           lwa_flatv_old TYPE /agri/s_glflatv
                  CHANGING lwa_flatv     TYPE /agri/s_glflatv
                           lv_subrc.

  DATA: lv_valid,
        lwa_message TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog,
        lwa_attr_val TYPE auspdata.

  CLEAR lv_subrc.
  CALL METHOD /agri/cl_gattr_utils=>attribute_value_check
    EXPORTING
      i_agtyp              = lwa_athdr-agtyp
      i_atinn              = lwa_flatv-atinn
      i_atwrt              = lwa_flatv-atwrt
*     i_sdpde              =
*     i_no_conv_exit_apply =
    IMPORTING
      et_messages          = lt_messages
    CHANGING
      cs_attr_val          = lwa_attr_val
      c_valid              = lv_valid.

  IF lv_valid EQ c_true.
    MOVE-CORRESPONDING lwa_attr_val TO lwa_flatv.
    IF lwa_flatv-atflv IS NOT INITIAL.
      CLEAR lwa_flatv-atwrt.
    ENDIF.
  ELSE.
    LOOP AT lt_messages INTO lwa_message.
      MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
         NUMBER lwa_message-msgno WITH lwa_message-msgv1
         lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
                    INTO sy-msgli.
      message_simple space.
    ENDLOOP.
    gs_variables-errors = c_true.
    lv_subrc = 4.
  ENDIF.

ENDFORM.                    "attribute_check
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTE_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attribute_data_prepare
                         USING lwa_athdr TYPE /agri/s_gathdr
                      CHANGING lwa_attr_vals TYPE /agri/s_glflatv_fcat.

  DATA: lwa_flatv TYPE /agri/s_glflatv.

  CLEAR lwa_attr_vals.
  MOVE-CORRESPONDING lwa_athdr TO lwa_attr_vals.

  READ TABLE gs_fldoc_infocus-x-flatv INTO lwa_flatv
                                  WITH KEY atinn = lwa_attr_vals-atinn
                                           clint = lwa_attr_vals-clint.
  IF sy-subrc = 0.
    PERFORM attribute_display_prepare USING lwa_athdr
                                            lwa_flatv
                                   CHANGING lwa_attr_vals.

  ENDIF.

ENDFORM.                    " ATTRIBUTE_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTE_DISPLAY_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attribute_display_prepare
                      USING    lwa_athdr TYPE /agri/s_gathdr
                               lwa_flatv TYPE /agri/s_glflatv
                      CHANGING lwa_attr_vals TYPE /agri/s_glflatv_fcat.

  DATA: lwa_glflatv TYPE /agri/glflatv.

  IF lwa_flatv IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_flatv TO lwa_glflatv.
    MOVE-CORRESPONDING lwa_glflatv TO lwa_attr_vals.
  ENDIF.

  IF NOT lwa_flatv-atzhl IS INITIAL
  OR NOT lwa_flatv-atwrt IS INITIAL
  OR NOT lwa_flatv-atflv IS INITIAL
  OR NOT lwa_athdr-inval IS INITIAL.

    CALL METHOD /agri/cl_gattr_utils=>attr_value_for_display_prepare
      EXPORTING
        i_agtyp       = lwa_athdr-agtyp
*       i_value_descr = 'X'
        is_athdr      = lwa_athdr
      CHANGING
        c_atwrt       = lwa_attr_vals-atwrt
        c_atflv       = lwa_attr_vals-atflv
        c_atwtb       = lwa_attr_vals-atwtb.
    IF lwa_attr_vals-atwtb IS INITIAL.
      lwa_attr_vals-atwtb = lwa_attr_vals-atwrt. "#EC CI_FLDEXT_OK
    ENDIF.
  ENDIF.

  lwa_attr_vals-atwrt_old = lwa_attr_vals-atwrt.

ENDFORM.                    " ATTRIBUTE_DISPLAY_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attribute_update  USING    lwa_flatv_old TYPE /agri/s_glflatv
                       CHANGING lwa_flatv     TYPE /agri/s_glflatv
                                lv_subrc      TYPE sy-subrc.

  DATA: lwa_athdr TYPE /agri/s_gathdr.

  CHECK lwa_flatv NE lwa_flatv_old.

  READ TABLE gt_athdr INTO lwa_athdr
        WITH KEY atinn = lwa_flatv-atinn.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  PERFORM attribute_check USING lwa_athdr
                                lwa_flatv_old
                       CHANGING lwa_flatv
                                lv_subrc.

ENDFORM.                    " ATTRIBUTE_UPDATE

*&---------------------------------------------------------------------*
*&      Form  address_handle_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_handle_prepare.

  DATA: BEGIN OF ls_handle_str,         "Struktur für temporäre Adresse:
          table LIKE dd02d-tabname,     "Wurzeltabelle des Adreßobjekts
          objnr LIKE iflot-objnr,       "(temporäre) Id des Adreßobjekts
          filler(88),                   "Wenn es nur ein adreßfähiges
        END OF ls_handle_str.

  CHECK gs_fldoc_infocus-x-flhdr-adrnr IS INITIAL.

  ls_handle_str-table = 'IFLOT'. "'/AGRI/GLFLOT'.
  ls_handle_str-objnr = gs_fldoc_infocus-x-flhdr-objnr.
  gv_address_handle = ls_handle_str.

ENDFORM.                    " address_handle_prepare
*&---------------------------------------------------------------------*
*&      Form  address_data_import
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_data_import.

  DATA: ls_addr LIKE addr1_data,
        lv_data_changed.

  CHECK sy-ucomm+0(1) NE '$' AND
        gs_variables-document_mode NE c_mode_display.

  CALL FUNCTION 'ADDR_IMP_SUBSCREEN'
    IMPORTING
      values                 = ls_addr
      error_in_address_data  = gs_variables-errors
      data_has_changed       = lv_data_changed
*     CURSOR_FIELD           =
*     INPUT_FROM_FRONTEND    =
*     DUPLICATE_RECORD_FOUND =
*     DUPL_REC_OBJECT_TYPE   =
*     DUPL_REC_OBJECT_KEY    =
*     DUPLICATE_CHECK_STATUS =
*   TABLES
*     ERROR_TABLE            =
    EXCEPTIONS ##FM_SUBRC_OK
      internal_error         = 1
      OTHERS                 = 2.

  IF lv_data_changed IS NOT INITIAL.
    gs_variables-data_changed = gs_variables-addr_data_changed
                              = c_true.
    MOVE-CORRESPONDING ls_addr TO gs_fldoc_infocus-x-adrc.
    gs_fldoc_infocus-x-adrc-updkz = c_updkz_update.
  ENDIF.

ENDFORM.                    " address_data_import
*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM attributes_display  USING lv_agtyp TYPE /agri/gagtyp
                               lv_atgrp TYPE /agri/gatgrp.

  CALL FUNCTION '/AGRI/GL_ATTRGROUP_DISPLAY'
    EXPORTING
      i_agtyp               = lv_agtyp
      i_atgrp               = lv_atgrp
    EXCEPTIONS
      incomplete_parameters = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ATTRIBUTES_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ADDRESS_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_data_display .

  DATA: lv_subrc TYPE sy-subrc.

  PERFORM address_handle_prepare.

  PERFORM address_data_prepare CHANGING lv_subrc.
  IF lv_subrc NE 0.
    gs_variables-program = c_program-funloc.
    gs_variables-subs_items = c_screen-dummy.
  ENDIF.

ENDFORM.                    " ADDRESS_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  address_data_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_data_prepare CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_dialog_mode TYPE szad_field-maint_mode.

  CLEAR lv_subrc.
  IF gs_fldoc_infocus-x-flhdr-adrnr IS INITIAL AND
     gs_variables-document_mode EQ c_mode_display.
    lv_dialog_mode = c_cam_mode-empty.
  ELSEIF gs_variables-document_mode EQ c_mode_display.
    lv_dialog_mode = c_cam_mode-display.
  ELSEIF gs_fldoc_infocus-x-flhdr-adrnr IS NOT INITIAL.
    lv_dialog_mode = c_cam_mode-change.
  ELSE.
    lv_dialog_mode = c_cam_mode-create.
  ENDIF.

  CALL FUNCTION 'ADDR_DIALOG_PREPARE'
   EXPORTING
*     FIELD_SELECTION                  = ' '
*     FIELD_SELECTION_FOR_NATION       = ' '
*     USE_FS_FOR_NATION                = ' '
*     KEYWORDS                         = ' '
*     TITLEBAR                         = ' '
*     CHANGE_DEFAULT_COMM_TYPES        = ' '
*     FRAME_TEXT                       = ' '
*     DEFAULT_URI_TYPE                 = ' '
*     SCREEN_VARIANT                   = ' '
      show_pushbottons_at_top          = c_true
*     USE_PSEUDO_REQUIRED_FIELDS       = ' '
*     IV_TIME_DEPENDENCE               = ' '
*   TABLES
*     EXCLUDED_FUNCTIONS               =
*     INCLUDED_FUNCTIONS               =
*     ERROR_TABLE                      =
   EXCEPTIONS
     internal_error                   = 1
     OTHERS                           = 2.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ADDR_EXP_SUBSCREEN'
    EXPORTING
      address_number              = gs_fldoc_infocus-x-flhdr-adrnr
      address_handle              = gv_address_handle
      address_group               = c_cam_group
      dialog_mode                 = lv_dialog_mode
*     SUGGESTED_VALUES            =
*     SUGGESTED_COMM_VALUES       =
*     ADDRESS_IS_OPTIONAL         = ' '
*     CHECK_ADDRESS               = 'X'
*     CURSOR_FIELD                =
*     SET_CURSOR_MODE             = '1'
*     SPA_GPA_IN_OPTIONAL_ADDRESS = 'X'
*     IV_VALID_FROM               = ' '
*     IV_VALID_TO                 = ' '
*     IV_SUGGESTED_VALID_FROM     =
*     IV_SUGGESTED_VALID_TO       =
    EXCEPTIONS
      address_not_exist           = 1
      group_not_valid             = 2
      parameter_error             = 3
      internal_error              = 4
      OTHERS                      = 5.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
  ENDIF.

ENDFORM.                    " address_data_prepare

**&---------------------------------------------------------------------*
**&      Form  ADDRESS_CREATE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM address_create .
*
*  DATA BEGIN OF lt_addr1 OCCURS 1.      "Übergabetab. für FB ADDR_DIALOG
*          INCLUDE STRUCTURE addr1_dia.
*  DATA END OF lt_addr1.
*
*  ad_handle_str-table = 'IFLOT'.
*  ad_handle_str-objnr = gs_fldoc_infocus-x-flhdr-objnr.
*  lt_addr1-handle = ad_handle_str.
*  MOVE: 'PM01'   TO lt_addr1-addr_group,
*        'CREATE' TO lt_addr1-maint_mode.
*  APPEND lt_addr1.
*
*  PERFORM address_dialog_call TABLES lt_addr1.
*
*ENDFORM.                    " ADDRESS_CREATE
**&---------------------------------------------------------------------*
**&      Form  ADDRESS_DIALOG_CALL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LT_ADDR1  text
**----------------------------------------------------------------------*
*FORM address_dialog_call  TABLES lt_addr1.
*
*  CALL FUNCTION 'ADDR_DIALOG_PREPARE'
*    EXCEPTIONS
*      internal_error = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  CALL FUNCTION 'ADDR_DIALOG'
** EXPORTING
**   CHECK_ADDRESS                     = 'X'
**   SUPPRESS_TAXJURCODE_CHECK         = ' '
**   IV_TIME_DEPENDENT_COMM_DATA       = ' '
** IMPORTING
**   OK_CODE                           =
*    TABLES
*      number_handle_tab                 = lt_addr1[]
**   VALUES                            =
*   EXCEPTIONS
*     address_not_exist                 = 1
*     group_not_valid                   = 2
*     parameter_error                   = 3
*     internal_error                    = 4
*     OTHERS                            = 5.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*ENDFORM.                    " ADDRESS_DIALOG_CALL
*&---------------------------------------------------------------------*
*&      Form  ADDRESS_NUMBER_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_number_get .

  DATA: lwa_ad_ref TYPE addr_ref.
  DATA: BEGIN OF lwa_ad_refkey,             "Hilfsstruktur zur Aufnahme des
        mandt LIKE sy-mandt,          "führenden Mandanten im Feld
        id    LIKE /agri/glflot-tplnr_fl,"iflot-tplnr,       "AD_REF-APPL_KEY
      END OF lwa_ad_refkey.

  CHECK gs_variables-addr_data_changed EQ c_true AND
        gs_fldoc_infocus-x-flhdr-adrnr IS INITIAL.
*        gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.

  lwa_ad_refkey-mandt = sy-mandt.
  lwa_ad_refkey-id = gs_fldoc_infocus-x-flhdr-tplnr_fl.
  lwa_ad_ref-appl_table = 'IFLOT'."'/AGRI/GLFLOT'.
  lwa_ad_ref-appl_field = 'TPLNR'."'TPLNR_FL'.
  lwa_ad_ref-appl_key = lwa_ad_refkey.
  lwa_ad_ref-owner = c_true.

****ESP6 Task #29917 -Partner engine changes
*  CALL FUNCTION '/AGRI/G_SD_ADDRESS_NUMBER_GET'
*    EXPORTING
*      i_address_handle         = gv_address_handle
**     I_ADDRESS_GROUP          = 'SD01'
*      i_appl_table             = lwa_ad_ref-appl_table
*      i_appl_field             = lwa_ad_ref-appl_field
*      i_appl_key               = lwa_ad_ref-appl_key
**     I_EXECUTE_IN_UPDATE_TASK = ' '
**     I_TRTYP                  =
*    IMPORTING
*      e_address_number         = gs_fldoc_infocus-x-flhdr-adrnr
*    EXCEPTIONS
*      number_not_found         = 1
*      OTHERS                   = 2.
  CALL FUNCTION '/AGRI/G_PAR_ADDRESS_NUMBER_GET'
    EXPORTING
      i_address_handle         = gv_address_handle
*     I_ADDRESS_GROUP          = 'SD01'
      i_appl_table             = lwa_ad_ref-appl_table
      i_appl_field             = lwa_ad_ref-appl_field
      i_appl_key               = lwa_ad_ref-appl_key
*     I_EXECUTE_IN_UPDATE_TASK = ' '
*     I_TRTYP                  =
    IMPORTING
      e_address_number         = gs_fldoc_infocus-x-flhdr-adrnr
    EXCEPTIONS ##FM_SUBRC_OK
      number_not_found         = 1
      OTHERS                   = 2.
****

*  CALL FUNCTION 'ADDR_NUMBER_GET'
*    EXPORTING
*      address_handle    = gv_address_handle
*      address_reference = lwa_ad_ref
*    IMPORTING
*      address_number    = gs_fldoc_infocus-x-flhdr-adrnr
*    EXCEPTIONS
*      OTHERS            = 1.
*  gs_fldoc_infocus-x-iloa-adrnr = gs_fldoc_infocus-x-flhdr-adrnr.

ENDFORM.                    " ADDRESS_NUMBER_GET
*&---------------------------------------------------------------------*
*&      Form  ADDRESS_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_init .

  CALL FUNCTION 'ADDR_MEMORY_CLEAR'
    EXPORTING
      force              = c_true
    EXCEPTIONS
      unsaved_data_exist = 1
      internal_error     = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'ADDR_SUBSCREEN_INITIALIZE'.

ENDFORM.                    " ADDRESS_INIT
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_maintain .

  DATA: lv_objtyp TYPE char1 VALUE 'F',
        lv_mode   LIKE t180-trtyp,
        lv_tplnr  TYPE /agri/gltplnr_fl.

  lv_mode = gs_variables-document_mode.
  lv_tplnr = gs_fldoc_infocus-x-flhdr-tplnr_fl.
**S4H
 gs_variables-subs_items = c_screen-dummy.
**
  CALL FUNCTION '/AGRI/GLCS_SUBSCREEN_IMPORT'
    EXPORTING
      i_mode      = lv_mode
      i_refresh   = gs_variables-refresh_assignments
      i_objtyp    = lv_objtyp
      i_objkey    = lv_tplnr
    CHANGING
      c_program   = gs_variables-program
      c_subscreen = gs_variables-subs_items.

  CLEAR: gs_variables-refresh_assignments.

ENDFORM.                    " ASSIGNMENTS_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_DATA_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_data_import .

  DATA: lv_subrc TYPE sy-subrc,
        lwa_message TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog.

  CHECK gs_variables-document_mode NE c_mode_display AND
        ok_code(10) NE '/AGRI/CS_'.

  CALL FUNCTION '/AGRI/GLCS_DATA_IMPORT'
    EXPORTING
      i_objkey    = gs_fldoc_infocus-x-flhdr-tplnr_fl
    IMPORTING
      e_changed   = gs_variables-data_changed
      e_subrc     = lv_subrc
      et_messages = lt_messages
      et_csdoc    = gt_csdoc_infocus.
  IF lv_subrc NE 0.
    CLEAR: ok_code.
    LOOP AT lt_messages INTO lwa_message.
   MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
      NUMBER lwa_message-msgno WITH lwa_message-msgv1
      lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
                 INTO sy-msgli.
   message_simple space.
 ENDLOOP.
  ENDIF.

 PERFORM messages_display USING gs_variables-initiator.
ENDFORM.                    " ASSIGNMENTS_DATA_IMPORT
*&---------------------------------------------------------------------*
*&      Form  assignments_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_init.
  CALL FUNCTION '/AGRI/GLCS_INIT'.
ENDFORM.                    "assignments_init
*&---------------------------------------------------------------------*
*&      Form  ADDITIONAL_FIELDS_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM additional_fields_prepare .

  DATA: lv_user_structure TYPE ddobjname VALUE '/AGRI/S_GLFLOTCA',
        lt_customer_fields TYPE TABLE OF /agri/s_abgl_user_scrfields,
        lt_cstru_fields TYPE TABLE OF /agri/s_abgl_user_scrfields,
        ls_cstru_field TYPE /agri/s_abgl_user_scrfields,
        lt_additional_data TYPE TABLE OF /agri/s_abgl_user_scrfields.

  CHECK gs_tglfllvl-hdrstr IS NOT INITIAL.

  PERFORM additional_data_fields_get USING lv_user_structure.
  lt_customer_fields = gt_additional_data.

  PERFORM additional_data_fields_get USING gs_tglfllvl-hdrstr.
  lt_cstru_fields = gt_additional_data.
  REFRESH gt_additional_data.

  LOOP AT lt_cstru_fields INTO ls_cstru_field.
    READ TABLE lt_customer_fields
                WITH KEY fieldname = ls_cstru_field-fieldname
                  TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      APPEND ls_cstru_field TO gt_additional_data.
    ENDIF.
  ENDLOOP.

  IF gt_additional_data IS INITIAL.
    CLEAR gs_tglfllvl-hdrstr.
  ENDIF.

ENDFORM.                    " ADDITIONAL_FIELDS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  ADDITIONAL_DATA_FIELDS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM additional_data_fields_get  USING lv_user_structure.

****Added on 11/21/2005
  DATA: ls_additional_data TYPE /agri/s_abgl_user_scrfields.
  DATA: lt_dd03p TYPE TABLE OF dd03p,
        ls_dd03p TYPE dd03p.

  REFRESH gt_additional_data.

  CHECK NOT lv_user_structure IS INITIAL.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = lv_user_structure
*     STATE         = 'A'
      langu         = sy-langu
    TABLES
      dd03p_tab     = lt_dd03p
    EXCEPTIONS ##FM_SUBRC_OK
      illegal_input = 1
      OTHERS        = 2.

  LOOP AT lt_dd03p INTO ls_dd03p.
    CLEAR ls_additional_data.
    ls_additional_data-fieldname = ls_dd03p-fieldname.
    IF NOT ls_dd03p-scrtext_m IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_m.
    ELSEIF NOT ls_dd03p-scrtext_l IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_l.
    ELSEIF NOT ls_dd03p-scrtext_s IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_s.
    ELSEIF NOT ls_dd03p-reptext IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-reptext.
    ELSE.
      ls_additional_data-fieldtxt = ls_dd03p-fieldname.
    ENDIF.
    ls_additional_data-fieldtyp = ls_dd03p-inttype.
    ls_additional_data-outputlen = ls_dd03p-outputlen.
    ls_additional_data-convexit = ls_dd03p-convexit.
***Single Field
    ls_additional_data-tabname = lv_user_structure.
    APPEND ls_additional_data TO gt_additional_data.
  ENDLOOP.

  DELETE gt_additional_data WHERE fieldname EQ '.INCLUDE'
                               OR fieldname EQ '.APPEND'
                               OR fieldname CS 'DUMMY'
****Added on 07/24/2007
                               OR fieldname CS '.INCLU'.


ENDFORM.                    " ADDITIONAL_DATA_FIELDS_GET
*&---------------------------------------------------------------------*
*&      Form  ADDRESS_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM address_maintain    TABLES lt_errors STRUCTURE  addr_error
                          USING lv_handle
                                lv_check_address
                                lv_updateflag
                       CHANGING lwa_address_complete TYPE
                                szadr_addr1_complete.

  DATA: lt_errors_temp TYPE TABLE OF addr_error.
  DATA: lwa_error_temp TYPE addr_error.

  lwa_address_complete-addrhandle = lv_handle.
  CLEAR lwa_address_complete-addrnumber.

  CALL FUNCTION 'ADDR_MAINTAIN_COMPLETE'
    EXPORTING
      updateflag               = lv_updateflag
      addr1_complete           = lwa_address_complete
      address_group            = c_cam_group
      check_address            = lv_check_address
      substitute_all_comm_data = c_true
    TABLES
*****Temporary Corrections
*      error_table              = lt_errors[]
       error_table             = lt_errors_temp[]
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error          = 1
      address_not_exist        = 2
      handle_exist             = 3
      internal_error           = 4
      OTHERS                   = 5.

  LOOP AT lt_errors_temp INTO lwa_error_temp.

    IF lwa_error_temp-msg_id EQ 'AM'
    AND lwa_error_temp-msg_type EQ 'W'
    AND lwa_error_temp-msg_number EQ '320'.
      CONTINUE.
    ENDIF.

    APPEND lwa_error_temp TO lt_errors[].

  ENDLOOP.

ENDFORM.                    " ADDRESS_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_infocus_save  CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_messages   TYPE /agri/t_gprolog,
        lwa_message   TYPE /agri/s_gprolog.

  CALL FUNCTION '/AGRI/GLCS_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK  = 'X'
      i_commit_work      = c_false
    CHANGING
      ct_csdoc           = gt_csdoc_infocus
      ct_messages        = lt_messages
    EXCEPTIONS
      no_change          = 1
      error_while_saving = 2
      OTHERS             = 3.
  IF sy-subrc NE 1.
    lv_subrc = sy-subrc.
  ENDIF.

  LOOP AT lt_messages INTO lwa_message.
    MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
       NUMBER lwa_message-msgno WITH lwa_message-msgv1
       lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

ENDFORM.                    " ASSIGNMENTS_INFOCUS_SAVE
