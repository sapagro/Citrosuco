*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0T .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TRANSACTION_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM transaction_init  USING lv_mode.

  CHECK gs_variables-overview_mode IS INITIAL.

  gs_variables-worklist_is_visible = c_true.
  gs_variables-overview_mode       = lv_mode.
  gs_variables-document_mode       = lv_mode.

****Register the log events that are to be handled
  CREATE OBJECT ref_log_handler.
  message_log_event_handler_set ref_log_handler
                                on_log_display_profile.

ENDFORM.                    " TRANSACTION_INIT
*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM title_set .

  DATA: lv_acnum        TYPE zfmacnum,
        lv_object_type  TYPE string,
        lv_description  TYPE /agri/gdescr_40.

  lv_object_type = text-001.
  lv_acnum       = gs_acdoc_infocus-acnum.

  CASE sy-dynnr.

    WHEN c_screen-main_screen.

****If no document is in focus
      IF lv_acnum IS INITIAL.
        IF gs_variables-overview_mode EQ c_mode_change
           AND NOT gs_variables-document_mode EQ c_mode_create.
          SET TITLEBAR 'T100' WITH text-005
                                   lv_object_type.
        ELSEIF gs_variables-overview_mode EQ c_mode_display.
          SET TITLEBAR 'T100' WITH text-002
                                   gs_variables-object_text.
        ENDIF.
****If some document is in focus
      ELSEIF gs_variables-document_mode EQ c_mode_change.
        SET TITLEBAR 'T101' WITH text-005
                                 gs_variables-object_text
                                 lv_acnum.

      ELSEIF gs_variables-document_mode EQ c_mode_display.
        SET TITLEBAR 'T101' WITH text-002
                                 gs_variables-object_text
                                 lv_acnum.

      ELSEIF gs_variables-document_mode EQ c_mode_create.
        SET TITLEBAR 'T101' WITH text-003
                                 gs_variables-object_text
                                 text-004.
      ENDIF.
    WHEN c_screen-hierarchy_items.
      SET TITLEBAR 'T202'.
    WHEN c_screen-create_mass_dialog OR
         c_screen-create_dialog.
**      keytext_get_simple 'ZFMACHDR' 'ASLVL' /agri/1899SC_FMACHDR-aslvl
**                         lv_description.
      SET TITLEBAR 'T102' WITH lv_description.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " TITLE_SET
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_excludes_prepare
                          TABLES lt_toolbar_excludes TYPE ui_functions.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_info  TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_check TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_refresh
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_mb_paste TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_append_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_cut
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_delete_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_move_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste_new_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_undo
                                           TO lt_toolbar_excludes.

ENDFORM.                    " TOOLBAR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_build .

  DATA: lv_fcode TYPE syucomm,
        lv_exist.

  FIELD-SYMBOLS: <ls_tabstrip_fcode> TYPE /agri/s_gtabstrip.

  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                                 WITH KEY ts_fcode = ts_items-activetab.
  IF sy-subrc EQ 0.
    lv_fcode = <ls_tabstrip_fcode>-local_fcode.
  ENDIF.

  PERFORM tabstrip_initialize.

****Set back previous tab
  IF lv_fcode IS INITIAL.
    CLEAR ts_items-activetab.
  ELSE.
    READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                                   WITH KEY local_fcode = lv_fcode.
    IF sy-subrc EQ 0.
      IF <ls_tabstrip_fcode>-invisible IS INITIAL.
        ts_items-activetab = <ls_tabstrip_fcode>-ts_fcode.
      ELSE.
        CLEAR ts_items-activetab.
      ENDIF.
    ELSE.
      CLEAR ts_items-activetab.
    ENDIF.
  ENDIF.

****Adjust scroll
  IF ts_items-activetab IS INITIAL.
    CLEAR ts_items-%_scrollposition.
  ENDIF.

ENDFORM.                    " TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_initialize .

  DATA: lv_domain             TYPE domname VALUE 'ZFMACTSTAB',
        lv_actual_tabname(32) TYPE c,
        lv_agtab(2)           TYPE c,
        lt_domain_values      TYPE TABLE OF dd07v,
        ls_domain_value       TYPE dd07v,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip.

  CONSTANTS: c_prefix_caption(24)  TYPE c
                                  VALUE 'GS_TABSTRIP_CAPTIONS-TAB',
             c_prefix_tab_fcode(2) TYPE c VALUE 'T\'.

  FIELD-SYMBOLS:  <lv_actual_tab> TYPE text60.

  IF gs_tabstrip_captions IS INITIAL.
****Prepare default tabs
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname              = lv_domain
        text                 = c_true
       langu                = sy-langu
*       BYPASS_BUFFER        = ' '
*     IMPORTING
*       RC                   =
      TABLES
        dd07v_tab            = lt_domain_values
      EXCEPTIONS
        wrong_textflag       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT lt_domain_values INTO ls_domain_value.
      CLEAR ls_tabstrip_fcode.
      lv_agtab = ls_domain_value-valpos+2(2).

      CONCATENATE c_prefix_caption lv_agtab INTO lv_actual_tabname.
      ASSIGN (lv_actual_tabname) TO <lv_actual_tab>.
      ls_tabstrip_fcode-tscode = ls_domain_value-domvalue_l.

      CASE ls_domain_value-domvalue_l.
        WHEN 'GS'.
****General Tab
****Prepare title
          WRITE icon_detail AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.
****Store other info for screen processing
          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab
                 INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_attributes.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
        WHEN 'TX'.
****Texts
          WRITE icon_create_text AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab
                 INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_texts.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'NT'.
****Notes
          WRITE icon_change_text AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab
                 INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_notes.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'AD'.
****Admin Data
          WRITE icon_administrative_data AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab
                 INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_admin.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  gt_tabstrip_texts = lt_domain_values.
  /agri/s_gtabstrip_captions = gs_tabstrip_captions.

ENDFORM.                    " TABSTRIP_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  TEXT_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM text_maintain USING lv_txtgr
                         lv_text_object
                CHANGING lv_changed.

**** ESP6 Task #30035 - Global Text Engine Integration
*  DATA: lv_objval       TYPE tdobname,
  DATA: lv_objval       TYPE /agri/gtxobjval,
****
        lv_display_only.

*  IF ref_text IS INITIAL.
*    CREATE OBJECT ref_text
***** ESP6 Task #30035 - Global Text Engine Integration
*      EXPORTING
*        i_objtp =  c_switch_object_type.
*****
*  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_display_only = c_true.
  ENDIF.

  CONCATENATE gs_acdoc_infocus-x-achdr-acnum '' INTO lv_objval.

  IF NOT ref_text IS INITIAL.
**** ESP6 Task #30035 - Global Text Engine Integration
    PERFORM text_control_initialize.
*    CALL FUNCTION '/AGRI/G_TEXT_INIT'.
****
**** ESP6 Task #30035 - Global Text Engine Integration
*    CALL METHOD ref_text->text_initialize
    CALL METHOD ref_text->text_infocus_set
****
      EXPORTING
        i_tdobject              = lv_text_object
        i_objval                = lv_objval
*       I_OBJDESCR              =
        i_display_only          = lv_display_only
        i_txtgr                 = lv_txtgr
        i_spras                 = '*'
*       i_mode                  = gs_variables-document_mode
      EXCEPTIONS
        object_not_found        = 1
        obj_procedure_not_found = 2
        text_ids_not_maintained = 3
        OTHERS                  = 4.

    CALL METHOD ref_text->text_maintain
      EXPORTING
        i_subscreen = c_true
        i_save      = c_false
      IMPORTING
        e_changed   = lv_changed.

  ENDIF.

ENDFORM.                    " TEXT_MAINTAIN
**** ESP6 Task #30035 - Global Text Engine Integration
FORM text_control_initialize.
  CALL FUNCTION '/AGRI/G_TEXT_INIT'.
ENDFORM.                    "text_control_initialize
****
*&---------------------------------------------------------------------*
*&      Form  TEXTS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM texts_update .
  DATA: lv_texts_changed.
  CLEAR sy-subrc.

  IF NOT ref_text IS INITIAL.
    CALL METHOD ref_text->text_update
      IMPORTING
        e_data_changed = lv_texts_changed.
  ENDIF.

  IF sy-subrc EQ 0
 AND lv_texts_changed IS NOT INITIAL.

    gs_variables-data_changed = c_true.

    IF gs_acdoc_infocus-x-achdr-updkz IS INITIAL.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.
ENDFORM.                    " TEXTS_UPDATE
