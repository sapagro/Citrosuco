*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0S
*&---------------------------------------------------------------------*
FORM subscreen_area_set .
  DATA: lv_routine(30) VALUE 'SUBSCREEN_AREA_SET_'.
  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (c_program) IF FOUND.
ENDFORM.                    " SUBSCREEN_AREA_SET
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0100.

  IF NOT gs_acdoc_infocus IS INITIAL.
    gs_variables-main_screen  = c_screen-overview.
  ELSE.
    gs_variables-main_screen = c_screen-dummy.
  ENDIF.

ENDFORM.                    "subscreen_area_set_0100
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0101.

  gs_variables-subs_header = c_screen-dummy.
  gs_variables-subs_details = c_screen-dummy.

  IF gs_acdoc_infocus IS NOT INITIAL.
    IF gs_variables-header_display = c_true.
      gs_variables-subs_header = c_screen-header.
    ELSE.
      gs_variables-subs_header = c_screen-header_compressed.
    ENDIF.
    gs_variables-subs_details = c_screen-items.
  ENDIF.

ENDFORM.                    "subscreen_area_set_0101
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify .

  DATA: lv_perform(30) TYPE c VALUE 'SCREEN_MODIFY_'.

  CONCATENATE lv_perform sy-dynnr INTO lv_perform.

  LOOP AT SCREEN.
    PERFORM (lv_perform) IN PROGRAM (c_program)
                         IF FOUND.
    IF gs_variables-document_mode EQ c_mode_display.
      CHECK screen-group1 NE c_screen_group-display_only.

      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY
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
    IF sy-subrc EQ 0.
      screen-invisible = 0.
    ELSE.
      screen-invisible = 1.
    ENDIF.

    IF gs_variables-document_mode = c_mode_create AND
       lwa_tabstrip_fcodes-local_fcode EQ 'TAB_ADMN'.
      screen-active = 0.
    ENDIF.
  ENDIF.
  MODIFY SCREEN.

ENDFORM.                    "screen_modify_0300
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0300.

  DATA: lv_active,
        ls_tabstrip TYPE /agri/s_gtabstrip,
        lv_descr TYPE /agri/gdescr.

  IF ts_items-activetab IS INITIAL.
    LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
      EXIT.
    ENDLOOP.
    ts_items-activetab = ls_tabstrip-ts_fcode.
  ENDIF.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
               WITH KEY ts_fcode = ts_items-activetab.

  gs_variables-program = c_program.
  gs_variables-subs_items = c_screen-dummy.

  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_items.
      gs_variables-subs_items = c_screen-ac_items.
    WHEN c_fcode-tab_postings.
**  added on  25.09.19
*      PERFORM terrain_details.
*" commented  on 25.09.19

      PERFORM confirmations_maintain.

      gs_variables-subs_items = c_screen-ac_postings.

    WHEN c_fcode-tab_texts.
      gs_variables-subs_items = c_screen-ac_texts.
    WHEN c_fcode-tab_status.
      gs_variables-subs_items = c_screen-ac_status.
      PERFORM status_maintain.
    WHEN c_fcode-tab_notes.
      gs_variables-subs_items = c_screen-dummy.
      PERFORM notes_maintain.
    WHEN c_fcode-tab_additional_data1.
      IF gt_additional_data IS NOT INITIAL.
        gs_variables-subs_items = c_screen-user_additional_data.
      ELSE.
        gs_variables-subs_items = c_screen-dummy.
      ENDIF.
    WHEN c_fcode-tab_admin.
      gs_variables-subs_items = c_screen-ac_admin.
    WHEN OTHERS.
      gs_variables-subs_items = c_screen-dummy.
  ENDCASE.

  PERFORM notes_title_prepare.

ENDFORM.                    "subscreen_area_set_0300
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0200.

  DATA: lv_descr TYPE /agri/gdescr_40.
  IF screen-group2 = c_screen_group-aufnr.
    IF gs_tfmactyp-acapp EQ c_accom_appli-aufnr
     OR gs_tfmactyp-acapp EQ c_accom_appli-prnum.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.
  IF screen-name = 'IO_TEXT'.
    IF gs_tfmactyp-actyp IS  NOT INITIAL.
      SELECT SINGLE descr
             FROM /agri/tfmactypt
             INTO lv_descr
             WHERE actyp EQ gs_tfmactyp-actyp."#EC CI_NOORDER
      IF sy-subrc EQ 0.
        io_text = lv_descr.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "screen_modify_0200
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0201
*&---------------------------------------------------------------------*
FORM screen_modify_0201.

  IF screen-group2 = c_screen_group-aufnr.
    IF gs_tfmactyp-acapp NE c_accom_appli-aufnr.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group2 = c_screen_group-wonum.
    IF gs_tfmactyp-acapp NE c_accom_appli-wonum.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group2 = c_screen_group-accom.
    IF gs_tfmactyp-acapp EQ c_accom_appli-wonum
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-prnum
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-aufnr
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF /agri/s_fmachdr-actyp IS INITIAL.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group2 = c_screen_group-copy AND
         gs_variables-copy = c_true.
    screen-active = 0.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                    "screen_modify_0201
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0201
*&---------------------------------------------------------------------*
FORM screen_modify_0010.

  IF screen-group1 = c_screen_group-aufnr.
    IF gs_tfmactyp-acapp NE c_accom_appli-aufnr.
      screen-active = 0.
    ENDIF.
*    IF screen-name EQ '%_SO_AUFNR_%_APP_%-TO_TEXT' OR screen-name EQ 'SO_AUFNR-HIGH'.
*      screen-active = 0.
*    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group1 = c_screen_group-wonum.
    IF gs_tfmactyp-acapp NE c_accom_appli-wonum.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group1 = c_screen_group-accom.
    IF gs_tfmactyp-acapp EQ c_accom_appli-wonum
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-prnum
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-aufnr
      AND gs_tfmactyp-numki IS NOT INITIAL.
      screen-active = 0.
    ELSEIF /agri/s_fmachdr-actyp IS INITIAL.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ELSEIF screen-group1 = c_screen_group-copy AND
         gs_variables-copy = c_true.
    screen-active = 0.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                    "screen_modify_010
*&----------------------------------------.-----------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set .
  DATA: lt_fcode_excludes TYPE ui_functions.

  IF NOT gs_acdoc_infocus-x-achdr-accom IS INITIAL.
    keytext_get_simple '/AGRI/FMACHDR'
                       'ACCOM'
                       gs_acdoc_infocus-x-achdr-accom
                       gs_variables-object_text.
  ELSE.
    gs_variables-object_text = text-001.
  ENDIF.
  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN c_screen-main_screen.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-create_accom.
      SET PF-STATUS 'S201'.
    WHEN c_screen-multi_lang_desc.
      SET PF-STATUS 'S202'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  STATUS_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_maintain .
  DATA: lv_objnr TYPE onr00-objnr,
        lv_mode LIKE t180-trtyp.

  lv_mode = gs_variables-document_mode.
**  lv_objnr = gs_cmdoc_infocus-x-cmhdr-objnr.

****Release 60E Status Changes for BRF
  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.
****

  CALL FUNCTION '/AGRI/G_STATUS_SUBSCR_IMPORT'
    EXPORTING
      i_mode                        = lv_mode
      i_objnr                       = lv_objnr
      i_no_user_status              = ' '
      i_no_system_status            = c_true
****Release 60E Status Changes for BRF
       iref_object           = ref_status_handler
     CHANGING
       c_change_flag                 = gs_variables-document_changed
       c_program                     = gs_variables-program
       c_subscreen                   = gs_variables-subs_items
     EXCEPTIONS ##FM_SUBRC_OK
       invalid_objnr                 = 1
       invalid_combination           = 2
       no_fieldcatalog_found         = 3
       OTHERS                        = 4.
ENDFORM.                    " STATUS_MAINTAIN
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_subscreen_import .
  DATA : lv_objnr TYPE onr00-objnr,
         lv_mode  LIKE t180-trtyp.

  lv_mode = gs_variables-document_mode.
  lv_objnr = gs_acdoc_infocus-x-achdr-objnr.

  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.

  CALL FUNCTION '/AGRI/G_STATUS_SUBSCR_IMPORT'
    EXPORTING
      i_mode                        = lv_mode
      i_objnr                       = lv_objnr
      i_no_user_status              = ' '
      i_no_system_status            = c_true
      iref_object                   = ref_status_handler
*   I_NO_SET_DATE                 = ' '
*   I_NO_SET_TIME                 = ' '
*   I_REFRESH_SYSTEM_STATUS       = 'X'
*   i_call_from_popup             =
*   I_PROGRAM_TOP                 =
*   I_SUBS_TOP                    =
* TABLES
*   T_SYSTEM_STATUS               =
    CHANGING
      c_change_flag                 = gs_variables-data_changed
      c_program                     = gs_variables-program
      c_subscreen                   = gs_variables-subs_items
    EXCEPTIONS ##FM_SUBRC_OK
      invalid_objnr                 = 1
      invalid_combination           = 2
      no_fieldcatalog_found         = 3
      OTHERS                        = 4.
ENDFORM.                    " STATUS_SUBSCREEN_IMPORT
*&---------------------------------------------------------------------*
*&      Form  STATUS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_update .
  DATA: lv_ustat TYPE j_status,
        lv_kfrst TYPE kfrst.

  CHECK: gs_variables-data_changed  EQ c_true OR
         gs_variables-document_mode EQ c_mode_create.

  CHECK NOT gs_acdoc_infocus-x-achdr-objnr IS INITIAL.

  CALL FUNCTION '/AGRI/G_STATUS_ACTIVE_GET'
    EXPORTING
      i_objnr          = gs_acdoc_infocus-x-achdr-objnr
    IMPORTING
      e_current_status = lv_ustat
    EXCEPTIONS ##FM_SUBRC_OK
      object_not_found = 1
      OTHERS           = 2.

  IF lv_ustat NE gs_acdoc_infocus-x-achdr-ustat.
    gs_acdoc_infocus-x-achdr-ustat = lv_ustat.

    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.

    PERFORM release_status_determine USING lv_ustat
                                  CHANGING lv_kfrst.
    CHECK NOT lv_kfrst IS INITIAL.

    IF lv_kfrst EQ 'R'.
      lv_kfrst = space.
*      gs_IRdoc_infocus-x-IRhdr-status = space.
    ENDIF.

    gs_acdoc_infocus-x-achdr-kfrst = lv_kfrst.

  ENDIF.
ENDFORM.                    " STATUS_UPDATE
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0306.
  PERFORM admin_data_maintain.
ENDFORM.                    "subscreen_area_set_0306

*&---------------------------------------------------------------------*
*&      Form  material_services_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_ACTIM       text
*      -->LT_COMPONENTS  text
*----------------------------------------------------------------------*
FORM material_services_get CHANGING lt_actim TYPE /agri/t_fmacitm
                            lt_components TYPE resb_t.

  CHECK NOT lt_actim[] IS INITIAL.

  DELETE lt_actim[] WHERE status NE 'CTD'
                                 AND intext EQ c_true.
  SELECT * FROM resb
        INTO CORRESPONDING FIELDS OF TABLE lt_components
     FOR ALL ENTRIES IN lt_actim
    WHERE aufnr EQ lt_actim-aufnr
       AND  postp EQ 'N'.  "#EC CI_NOFIELD.

ENDFORM.                    "material_services_get
