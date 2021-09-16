*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0S .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_set .

  DATA : lt_fcode_excludes TYPE ui_functions.

  IF NOT gs_rcdoc_infocus-x-rchdr-rctyp IS INITIAL.
    keytext_get_simple 'ZFMRCHDR'
                       'RCTYP'
                       gs_rcdoc_infocus-x-rchdr-rctyp
                       gs_variables-object_text.
  ELSE.
    gs_variables-object_text = TEXT-001.
  ENDIF.

  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN c_screen-main_screen.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
*    WHEN c_screen-hierarchy_items.
*      SET PF-STATUS 'S201'.
    WHEN c_screen-create_mass_dialog OR
         c_screen-create_dialog.
      SET PF-STATUS 'S101' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-update_mass.
      SET PF-STATUS 'S310' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-popup_recipe_create.
      SET PF-STATUS 'S204' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-popup_version_create.
      SET PF-STATUS 'S202' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-popup_version_modify.
      SET PF-STATUS 'S206' EXCLUDING lt_fcode_excludes.
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
  PERFORM (lv_routine) IN PROGRAM (c_program_rcm) IF FOUND.

ENDFORM.                    " SUBSCREEN_AREA_SET
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0100.

  IF NOT gs_rcdoc_infocus IS INITIAL.
    gs_variables-subscr_quick_info = c_screen-quick_info.
    gs_variables-subscr_details = c_screen-details.
  ELSE.
    gs_variables-subscr_quick_info = c_screen-dummy.
    gs_variables-subscr_details = c_screen-dummy.
  ENDIF.

ENDFORM.                    "subscreen_area_set_0100
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0101.
*  gs_variables-subscr_quick_info = c_screen-mass_quick_info.
ENDFORM.                    "subscreen_area_set_0101
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0300.

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.

  IF ts_items-activetab IS INITIAL.
    LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
      EXIT.
    ENDLOOP.
    ts_items-activetab = ls_tabstrip-ts_fcode.
  ENDIF.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
              WITH KEY ts_fcode = ts_items-activetab.
  gs_variables-program = c_program_rcm.
  gs_variables-subscr_details = c_screen-dummy.

  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_version.
      gs_variables-subscr_details = c_screen-version.
    WHEN c_fcode-tab_dose.
      gs_variables-subscr_details = c_screen-grid_dose.
    WHEN c_fcode-tab_bom.
      gs_variables-subscr_details = c_screen-grid_bom.
    WHEN c_fcode-tab_texts.
      gs_variables-subscr_details = c_screen-texts.
    WHEN c_fcode-tab_notes.
      gs_variables-subscr_details = c_screen-dummy.
      PERFORM notes_maintain.
    WHEN c_fcode-tab_admin.
      gs_variables-subscr_details = c_screen-admin.
*...Vistex-11.01.2019/Begin
    WHEN c_fcode-tab_similar.
      gs_variables-subscr_details = c_screen-produto_similar.
*...Vistex-11.01.2019/End
    WHEN OTHERS.
      gs_variables-subscr_details = c_screen-dummy.
  ENDCASE.

  PERFORM notes_title_prepare.

ENDFORM.                    "subscreen_area_set_0300
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0303
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0303.
  PERFORM admin_data_maintain.
ENDFORM.                    "subscreen_area_set_0303
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0102.
  gs_variables-subscr_quick_info = c_screen-quick_info.
ENDFORM.                    "subscreen_area_set_0102
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0305
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0305.
  IF gs_variables-display_mode IS INITIAL.
    gs_variables-subscr_details = c_screen-latest_measurements.
  ELSE.
    gs_variables-subscr_details = c_screen-measurements_display.
  ENDIF.
ENDFORM.                    "subscreen_area_set_0305
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify .

  DATA: lv_perform(30) TYPE c VALUE 'SCREEN_MODIFY_'.

  CONCATENATE lv_perform sy-dynnr INTO lv_perform.

  LOOP AT SCREEN.
    PERFORM (lv_perform) IN PROGRAM (c_program_rcm)
                         IF FOUND.
    IF gs_variables-overview_mode EQ c_mode_display.
      CHECK screen-group1 NE c_screen_group-display_only.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  screen_modify_0205
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0205.

  IF screen-name EQ zsc_fmrchdr-rctyp AND
     zsc_fmrchdr-rctyp IS NOT INITIAL.
    screen-input = 0.
    MODIFY SCREEN.
  ENDIF.
ENDFORM.                    "screen_modify_0205

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

FORM screen_modify_0010.

ENDFORM.                    "screen_modify_0010

FORM screen_modify_0200.

  IF gs_variables-overview_mode NE c_mode_display.
    IF zsc_fmrchdr-rctyp EQ 'ZORC'.
      IF screen-name EQ 'ZSC_FMRCHDR-AUSME'.
        screen-input = 1.
      ENDIF.
    ELSE.
      IF screen-name EQ 'ZSC_FMRCHDR-AUSME'.
        screen-input = 0.
      ENDIF.
    ENDIF.
  ELSE.
    IF screen-name EQ 'ZSC_FMRCHDR-AUSME'.
      screen-input = 0.
    ENDIF.
  ENDIF.
  MODIFY SCREEN.

ENDFORM.                    "screen_modify_0200
