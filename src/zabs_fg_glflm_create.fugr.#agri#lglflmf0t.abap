*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0T .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TRANSACTION_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM transaction_init USING lv_mode .

*  DATA: lwa_itobcust  TYPE itobcust.

  CHECK gs_variables-overview_mode IS INITIAL.

  gs_variables-overview_mode = lv_mode.
  gs_variables-document_mode = c_mode_display.
  gs_variables-worklist_is_visible = c_true.
  gs_variables-header_display = c_true.

*--Check alternative labeling
*  SELECT SINGLE * FROM itobcust INTO lwa_itobcust.
*  gs_variables-labeling_active = lwa_itobcust-cnvrt.
*  CALL FUNCTION 'FUNC_LOCATION_LABELING_PROFILE'
**   EXPORTING
**     USER_NAME                    = SY-UNAME
*    IMPORTING
**     LABELING_PROFILE             =
*      labeling_system = gs_variables-alkey
**     LABEL_SYST_DESCRIPTION       =
**     LABELING_SYSTEM_DATA         =
**     STRUCTURE_INDICATOR          =
**     STRUCT_IND_DESCRIPTION       =
**     EDITMASK        =
**     HIERARCHY_LEVELS             =
*    .

  SELECT * FROM /agri/tglcsatl INTO TABLE gt_tglcsatl.
  IF sy-subrc EQ 0.
    SORT gt_tglcsatl BY tplkz tplvl.
  ENDIF.

ENDFORM.                    " TRANSACTION_INIT
*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM title_set .

  DATA: lv_tplnr           TYPE /agri/glstrno,
        lv_object_type(20).

  lv_object_type = TEXT-001.
  lv_tplnr = gs_fldoc_infocus-x-flhdr-strno.

  CASE sy-dynnr.
    WHEN c_screen-overview.
      IF lv_tplnr IS INITIAL.
        IF gs_variables-overview_mode = c_mode_change AND
           NOT gs_variables-document_mode EQ c_mode_create.
          SET TITLEBAR 'T100' WITH TEXT-002
                                 lv_object_type.
        ELSEIF gs_variables-overview_mode = c_mode_display.
          SET TITLEBAR 'T100' WITH TEXT-004
                               gs_variables-object_text.
        ENDIF.
      ELSEIF gs_variables-document_mode EQ c_mode_change.
        SET TITLEBAR 'T101' WITH TEXT-005
                               gs_variables-object_text
                               lv_tplnr.

      ELSEIF gs_variables-document_mode EQ c_mode_display.
        SET TITLEBAR 'T101' WITH TEXT-004
                               gs_variables-object_text
                               lv_tplnr.

      ELSEIF gs_variables-document_mode EQ c_mode_create.
        SET TITLEBAR 'T101' WITH TEXT-003
                                 gs_variables-object_text
                                 TEXT-006.
      ENDIF.

    WHEN c_screen-create_floc.
      SET TITLEBAR 'T01'.

    WHEN c_screen-class_assignment.
      SET TITLEBAR 'T02'.

    WHEN c_screen-multi_lang_desc.
      SET TITLEBAR 'T203'.

    WHEN c_screen-terrain_labels.
      SET TITLEBAR 'T205'.

    WHEN c_screen-hierarchy_display.
      SET TITLEBAR 'T206'.

    WHEN c_screen-new_label.
      SET TITLEBAR 'T207'.

    WHEN c_screen-change_superior_fl.
      SET TITLEBAR 'T03'.

    WHEN c_screen-reference_summary.
      SET TITLEBAR 'T319' WITH gs_fldoc_infocus-tplnr_fl.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " TITLE_SET

*&---------------------------------------------------------------------*
*&      Form  tabstrip_initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_initialize.

  DATA: lv_domain             TYPE domname VALUE '/AGRI/GLFLTSTAB',
        lv_actual_tabname(32) TYPE c,
        lv_agtab(2)           TYPE c,
        lt_domain_values      TYPE TABLE OF dd07v,
        ls_domain_value       TYPE dd07v,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip.

  CONSTANTS: c_prefix_caption(24)  TYPE c VALUE 'GS_TABSTRIP_CAPTIONS-TAB',
             c_prefix_tab_fcode(2) TYPE c VALUE 'T\'.

  FIELD-SYMBOLS:  <lv_actual_tab> TYPE text60.

  IF gs_tabstrip_captions IS INITIAL.

    REFRESH: gt_tabstrip_fcodes.
****Prepare default tabs
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = c_true
        langu          = sy-langu
*       BYPASS_BUFFER  = ' '
*     IMPORTING
*       RC             =
      TABLES
        dd07v_tab      = lt_domain_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
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
        WHEN 'GN'.
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
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_details.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'OR'.
          WRITE icon_dispo_level AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_organization.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'PP'.
          WRITE icon_plant AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_plants.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'OW'.
          WRITE icon_shared_position AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_owners.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'AS'.
          WRITE icon_address AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_address.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'PA'.
          WRITE icon_partner AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_partners.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'CL'.
          WRITE icon_oo_class AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_classification.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'AA'.
          WRITE icon_reference_list AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_assignments.
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
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
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
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_notes.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'ST'.
****Status
          WRITE icon_set_state AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_status.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'A1'.
****Additional Data1
          WRITE icon_new_task AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_additional_data.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.

        WHEN 'A2'.
****Additional Data2
          WRITE icon_new_task AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.

          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_additional_data2.
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
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_admin.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  gt_tabstrip_texts = lt_domain_values.
  /agri/s_gtabstrip_captions = gs_tabstrip_captions.

ENDFORM.                    "toolbar_excludes_prepare

*&---------------------------------------------------------------------*
*&      Form  toolbar_buttons_exclude
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_TOOLBAR_EXCLUDES  text
*----------------------------------------------------------------------*
FORM toolbar_buttons_exclude  TABLES
                              lt_toolbar_excludes TYPE ui_functions.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_info  TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print_back TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print_prev TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_view_excel TO lt_toolbar_excludes.
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
  IF sy-dynnr NE c_screen-overview.
    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort
                                             TO lt_toolbar_excludes.
    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort_asc
                                             TO lt_toolbar_excludes.
    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort_dsc
                                             TO lt_toolbar_excludes.
  ENDIF.

  IF sy-dynnr EQ c_screen-reference_summary.
    APPEND:
   /agri/cl_alv_tree_simple_hier=>mc_fc_change_hierarchy
                                         TO lt_toolbar_excludes,
   /agri/cl_alv_tree_simple_hier=>mc_fc_detail
                                         TO lt_toolbar_excludes,
   /agri/cl_alv_tree_simple_hier=>mc_fc_show_detail
                                         TO lt_toolbar_excludes,
   /agri/cl_alv_tree_simple_hier=>mc_fc_calculate
                                         TO lt_toolbar_excludes.
  ENDIF.

ENDFORM.                    " TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_build .

  DATA: lv_fcode    TYPE syucomm,
        lv_exist,
        ls_tabstrip TYPE /agri/s_gtabstrip.

  FIELD-SYMBOLS: <ls_tabstrip_fcode> TYPE /agri/s_gtabstrip.

  SELECT a~tplkz a~tstab a~seqnr a~ticon b~vtext
    INTO CORRESPONDING FIELDS OF TABLE gt_tglflts
         FROM ( /agri/tglflts AS a           "#EC CI_BUFFJOIN
         LEFT OUTER JOIN /agri/tglfltst AS b ON  "#EC CI_BUFFJOIN
                         b~spras EQ sy-langu
                     AND b~tplkz EQ a~tplkz
                     AND b~tstab EQ a~tstab )
        WHERE a~tplkz = gs_t370s-tplkz.

  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                          WITH KEY ts_fcode = ts_items-activetab.
  IF sy-subrc EQ 0.
    lv_fcode = <ls_tabstrip_fcode>-local_fcode.
  ENDIF.

  PERFORM tabstrip_initialize.

*--Apply Customizing.
  PERFORM tabstrip_sequence_set.

**--Set assignments Tab
*  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
*                 WITH KEY local_fcode = c_fcode-tab_assignments.
*  IF sy-subrc EQ 0.
*    IF gs_tglfllvl-flcsn IS INITIAL.
*      <ls_tabstrip_fcode>-invisible = c_true.
*    ELSE.
*      CLEAR <ls_tabstrip_fcode>-invisible.
*    ENDIF.
*  ENDIF.

****Set back previous tab
  IF lv_fcode IS INITIAL.
    LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
      EXIT.
    ENDLOOP.
    ts_items-activetab = ls_tabstrip-ts_fcode.

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
*&      Form  TABSTRIP_SEQUENCE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_sequence_set.

  DATA: lv_agtab(2)           TYPE n,
        lv_tabix(2)           TYPE n,
****Tab Sequence Issue Fix
*       lv_actual_tabname(31) TYPE c,
        lv_actual_tabname(32) TYPE c,
****
        lv_tabname(5)         TYPE c,
        lv_tab_title          TYPE val_text,
        ls_tabs_sequence      TYPE /agri/v_tglflts,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip,
        ls_tabstrip_text      TYPE dd07v,
        ls_tabstrip_captions  TYPE /agri/s_gtabstrip_captions,
        lt_tabstrip_fcodes    TYPE /agri/t_gtabstrip.

  CONSTANTS: c_prefix_tab(3) TYPE c VALUE 'TAB',
             c_prefix_ts(28) TYPE c VALUE 'LS_TABSTRIP_CAPTIONS-TAB'.

  FIELD-SYMBOLS: <lv_actual_tab> TYPE text60,
                 <lv_tab_text>   TYPE any.

  SORT gt_tglflts BY seqnr.

  LOOP AT gt_tglflts INTO ls_tabs_sequence.
    lv_tabix = sy-tabix.

****Set tab
    CONCATENATE c_prefix_ts lv_tabix INTO lv_actual_tabname.
    ASSIGN (lv_actual_tabname) TO <lv_actual_tab>.

    READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip_fcode
                            WITH KEY tscode = ls_tabs_sequence-tstab.
    CHECK sy-subrc EQ 0.
    lv_agtab = sy-tabix.
    CONCATENATE c_prefix_tab lv_agtab INTO lv_tabname.

    IF ls_tabs_sequence-vtext IS NOT INITIAL OR
       ls_tabs_sequence-ticon IS NOT INITIAL.
      READ TABLE gt_tabstrip_texts INTO ls_tabstrip_text
                     WITH KEY domvalue_l = ls_tabs_sequence-tstab.

      IF ls_tabs_sequence-vtext IS INITIAL.
        ls_tabs_sequence-vtext = ls_tabstrip_text-ddtext.
      ENDIF.

      IF ls_tabs_sequence-ticon IS INITIAL.
        ASSIGN COMPONENT lv_tabname OF STRUCTURE gs_tabstrip_captions
                             TO <lv_tab_text>.
        IF sy-subrc EQ 0 AND
           <lv_actual_tab> IS ASSIGNED.
          <lv_actual_tab> = <lv_tab_text>.
          IF ls_tabstrip_text-ddtext IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF ls_tabstrip_text-ddtext
                    IN <lv_actual_tab> WITH ls_tabs_sequence-vtext.
          ELSE.
            CONCATENATE <lv_actual_tab> ls_tabs_sequence-vtext
                   INTO <lv_actual_tab>.
            lv_tab_title = ls_tabs_sequence-vtext.
            PERFORM icon_text_prepare USING lv_tab_title
                                   CHANGING <lv_actual_tab>.
          ENDIF.
        ENDIF.
      ELSE.
        WRITE (ls_tabs_sequence-ticon) AS ICON TO <lv_actual_tab>.
        CONCATENATE <lv_actual_tab> ls_tabs_sequence-vtext
               INTO <lv_actual_tab>.
        lv_tab_title = ls_tabs_sequence-vtext.
        PERFORM icon_text_prepare USING lv_tab_title
                               CHANGING <lv_actual_tab>.
      ENDIF.
    ELSE.
      ASSIGN COMPONENT lv_tabname OF STRUCTURE gs_tabstrip_captions
                           TO <lv_tab_text>.
      <lv_actual_tab> = <lv_tab_text>.
    ENDIF.

    REPLACE 'LS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
    ls_tabstrip_fcode-tabname = lv_actual_tabname.
    ls_tabstrip_fcode-ts_fcode+2(2) = lv_tabix.
    APPEND ls_tabstrip_fcode TO lt_tabstrip_fcodes.
  ENDLOOP.

  IF ls_tabstrip_captions IS NOT INITIAL AND
     lt_tabstrip_fcodes IS NOT INITIAL.
    /agri/s_gtabstrip_captions = gs_tabstrip_captions
                              = ls_tabstrip_captions.
    gt_tabstrip_fcodes = lt_tabstrip_fcodes.
  ENDIF.

ENDFORM.                    " TABSTRIP_SEQUENCE_SET
*&---------------------------------------------------------------------*
*&      Form  text_maintain
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

  IF ref_text IS INITIAL.
    CREATE OBJECT ref_text
**** ESP6 Task #30035 - Global Text Engine Integration
      EXPORTING
        i_objtp = c_switch_object_type.
****
  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_display_only = c_true.
  ENDIF.

  MOVE gs_fldoc_infocus-x-flhdr-strno TO lv_objval.

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
*&      Form  texts_update
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

    IF gs_fldoc_infocus-x-flhdr-updkz IS INITIAL.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.

ENDFORM.                    " TEXTS_UPDATE

*&---------------------------------------------------------------------*
*&      Form  TERRAIN_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM terrain_display .

  gs_variables-worklist_refresh = c_true.
  CALL SCREEN 100.

ENDFORM.                    " TERRAIN_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  TERRAIN_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM terrain_data_check  USING    lv_before_save
                         CHANGING lv_subrc.

  PERFORM owners_data_check USING lv_before_save
                         CHANGING lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

  IF /agri/s_glflot-garea IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '026' INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF /agri/s_glflot-msehi IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '027' INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " TERRAIN_DATA_CHECK
