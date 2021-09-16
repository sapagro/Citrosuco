*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0S .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_set .

  DATA : lt_fcode_excludes TYPE ui_functions.

  IF NOT gs_acdoc_infocus-x-achdr-actyp IS INITIAL.
    keytext_get_simple 'ZFMACHDR'
                       'ACTYP'
                       gs_acdoc_infocus-x-achdr-actyp
                       gs_variables-object_text.
  ELSE.
    gs_variables-object_text = TEXT-001.
  ENDIF.

  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN c_screen-main_screen.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-hierarchy_items.
      SET PF-STATUS 'S201'.
    WHEN c_screen-create_mass_dialog OR
         c_screen-create_dialog.
      SET PF-STATUS 'S101' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-update_mass.
      SET PF-STATUS 'S310' EXCLUDING lt_fcode_excludes.
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
  PERFORM (lv_routine) IN PROGRAM (c_program_acm) IF FOUND.

ENDFORM.                    " SUBSCREEN_AREA_SET
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0100.

  IF NOT gs_acdoc_infocus IS INITIAL.
    gs_variables-subscr_quick_info = c_screen-quick_info.
    gs_variables-subscr_details    = c_screen-details.
    gs_variables-subscr_budget     = c_screen-budget.
    gs_variables-subscr_cost       = c_screen-cost.
    gs_variables-subscr_recipe     = c_screen-recipe.
  ELSE.
    gs_variables-subscr_quick_info = c_screen-dummy.
    gs_variables-subscr_details    = c_screen-dummy.
    gs_variables-subscr_budget     = c_screen-dummy.
    gs_variables-subscr_cost       = c_screen-dummy.
    gs_variables-subscr_recipe     = c_screen-dummy.
  ENDIF.

ENDFORM.                    "subscreen_area_set_0100
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0101.
  gs_variables-subscr_quick_info = c_screen-mass_quick_info.
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
  gs_variables-program = c_program_acm.
  gs_variables-subscr_details = c_screen-dummy.

  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_items.
      gs_variables-subscr_details = c_screen-items.
    WHEN c_fcode-tab_vlcld.
      gs_variables-subscr_details = c_screen-volumen_calda.
    WHEN c_fcode-tab_texts.
      gs_variables-subscr_details = c_screen-texts.
    WHEN c_fcode-tab_notes.
      gs_variables-subscr_details = c_screen-dummy.
      PERFORM notes_maintain.
    WHEN c_fcode-tab_admin.
      gs_variables-subscr_details = c_screen-admin.
  ENDCASE.

  PERFORM notes_title_prepare.

ENDFORM.                    "subscreen_area_set_0300
*&---------------------------------------------------------------------*
*&      Form  subscreen_area_set_0312
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM subscreen_area_set_0312.

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.

  IF ts_items-activetab IS INITIAL.
    LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
      EXIT.
    ENDLOOP.
    ts_items-activetab = ls_tabstrip-ts_fcode.
  ENDIF.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
              WITH KEY ts_fcode = ts_items-activetab.
  gs_variables-program = c_program_acm.
  gs_variables-subscr_details = c_screen-dummy.

  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_dose.
      gs_variables-subscr_details = c_screen-dose.
    WHEN c_fcode-tab_qtde.
      gs_variables-subscr_details = c_screen-qtde.
    WHEN c_fcode-tab_items.
      gs_variables-subscr_details = c_screen-items.
    WHEN c_fcode-tab_vlcld.
      gs_variables-subscr_details = c_screen-volumen_calda.
    WHEN c_fcode-tab_texts.
      gs_variables-subscr_details = c_screen-texts.
    WHEN c_fcode-tab_notes.
      gs_variables-subscr_details = c_screen-dummy.
      PERFORM notes_maintain.
    WHEN c_fcode-tab_admin.
      gs_variables-subscr_details = c_screen-admin.
    WHEN OTHERS.
      gs_variables-subscr_details = c_screen-dummy.
  ENDCASE.

  PERFORM notes_title_prepare.

ENDFORM.                    "subscreen_area_set_0312
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

  CLEAR gv_budget_vrs.
  IF lv_perform EQ 'SCREEN_MODIFY_0200'.
    IF gs_acdoc_infocus IS NOT INITIAL.
      SELECT *
        FROM zabst_orcamento
        INTO TABLE @DATA(lt_versao)
       WHERE acnum EQ @gs_acdoc_infocus-x-achdr-acnum
         AND ajahr EQ @gs_acdoc_infocus-x-achdr-ajahr
         AND actyp EQ @gs_acdoc_infocus-x-achdr-actyp.

      IF sy-subrc EQ 0.
        gv_budget_vrs = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT SCREEN.
    PERFORM (lv_perform) IN PROGRAM (c_program_acm)
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

  IF screen-name EQ zsc_fmachdr-actyp AND
     zsc_fmachdr-actyp IS NOT INITIAL.
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

*&---------------------------------------------------------------------*
*&      Form  screen_modify_0312
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_0312.

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

ENDFORM.                    "screen_modify_0312

FORM screen_modify_0200.

  IF ( screen-name EQ 'GV_PROCESSO' OR
       screen-name EQ 'GV_TAREFA' OR
       screen-name EQ 'GV_VERSAO' OR
       screen-name EQ 'GV_STATUS' OR
       screen-name EQ 'CRIAR' OR
       screen-name EQ 'COPIAR' )
  AND gs_variables-document_mode EQ c_mode_change.
    IF screen-name EQ 'CRIAR'.
      IF gv_budget_vrs EQ abap_true.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ELSEIF screen-name EQ 'COPIAR'.
      IF gv_budget_vrs EQ abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
    ELSEIF screen-name EQ 'GV_STATUS'.
      IF gv_versao IS NOT INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
    ELSE.
      screen-input = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                    "screen_modify_0200

FORM screen_modify_0010.

ENDFORM.                    "screen_modify_0010
