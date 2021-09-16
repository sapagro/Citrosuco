*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0T
*&---------------------------------------------------------------------*
FORM transaction_init  USING  lv_mode.

  CHECK gs_variables-overview_mode IS INITIAL.

  gs_variables-worklist_is_visible  = c_true.
  gs_variables-header_display       = c_true.
  gs_variables-overview_mode        = lv_mode.
  gs_variables-document_mode        = c_mode_display.

ENDFORM.                    " TRANSACTION_INIT
*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM title_set .
  DATA: lv_accom           TYPE /agri/fmaccom,
        lv_object_type(20).

  lv_object_type = TEXT-001.
  lv_accom = gs_acdoc_infocus-accom.

  CASE sy-dynnr.

    WHEN c_screen-main_screen.

****If no document is in focus
      IF lv_accom IS INITIAL.
        IF gs_variables-overview_mode EQ c_mode_change
           AND NOT gs_variables-document_mode EQ c_mode_create.
          SET TITLEBAR 'T100' WITH TEXT-002
                                   lv_object_type.
        ELSEIF gs_variables-overview_mode EQ c_mode_display.
          SET TITLEBAR 'T100' WITH TEXT-004
                                 gs_variables-object_text.
        ENDIF.
****If some document is in focus
      ELSEIF gs_variables-document_mode EQ c_mode_change.
        SET TITLEBAR 'T101' WITH TEXT-005
                               gs_variables-object_text
                               lv_accom.

      ELSEIF gs_variables-document_mode EQ c_mode_display.
        SET TITLEBAR 'T101' WITH TEXT-004
                               gs_variables-object_text
                               lv_accom.

      ELSEIF gs_variables-document_mode EQ c_mode_create.
        SET TITLEBAR 'T101' WITH TEXT-003
                                 gs_variables-object_text
                                 TEXT-006.
      ENDIF.

    WHEN c_screen-create_accom.
      SET TITLEBAR 'T01'.
    WHEN c_screen-multi_lang_desc.
      SET TITLEBAR 'T203'.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " TITLE_SET
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TOOLBAR_EXCLUDES  text
*----------------------------------------------------------------------*
FORM toolbar_buttons_exclude  TABLES
                              lt_toolbar_excludes TYPE ui_functions.

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
  IF sy-dynnr NE c_screen-main_screen.
    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort
                                             TO lt_toolbar_excludes.
*    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort_asc
*                                             TO lt_toolbar_excludes.
*    APPEND /agri/cl_gui_alv_grid=>mc_fc_sort_dsc
*                                             TO lt_toolbar_excludes.
  ENDIF.

ENDFORM.                    " TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabstrip_build .
  DATA: lv_fcode TYPE syucomm,
        lv_exist.

  FIELD-SYMBOLS: <ls_tabstrip_fcode> TYPE /agri/s_gtabstrip.

*  SELECT a~irtyp a~tstab a~seqnr a~ticon b~vtext
*     INTO CORRESPONDING FIELDS OF TABLE gt_tfmirts
*         FROM ( /agri/tfmirts AS a
*             LEFT OUTER JOIN /agri/tfmirtst AS b ON
*                         b~spras EQ sy-langu
*                     AND b~irtyp EQ a~irtyp
*                     AND b~tstab EQ a~tstab )
*              WHERE a~irtyp = gs_tfmirtyp-irtyp.
*
*  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
*                          WITH KEY ts_fcode = ts_items-activetab.
*  IF sy-subrc EQ 0.
*    lv_fcode = <ls_tabstrip_fcode>-local_fcode.
*  ENDIF.
  PERFORM tabstrip_initialize.

*--Apply Customizing.
  PERFORM tabstrip_sequence_set.

****Set back previous tab
*  IF lv_fcode IS INITIAL.
*    CLEAR ts_items-activetab.
*  ELSE.
  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                 WITH KEY ts_fcode = ts_items-activetab.
  IF sy-subrc EQ 0.
    IF <ls_tabstrip_fcode>-invisible IS INITIAL.
      ts_items-activetab = <ls_tabstrip_fcode>-ts_fcode.
    ELSE.
      CLEAR ts_items-activetab.
    ENDIF.
  ELSE.
    CLEAR ts_items-activetab.
  ENDIF.
*  ENDIF.

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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabstrip_initialize .

  DATA: lv_domain             TYPE domname VALUE '/AGRI/FMACTSTAB',
        lv_actual_tabname(32) TYPE c,
        lv_agtab(2)           TYPE c,
        lt_domain_values      TYPE TABLE OF dd07v,
        ls_domain_value       TYPE dd07v,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip.

  CONSTANTS: c_prefix_caption(24)  TYPE c VALUE 'GS_TABSTRIP_CAPTIONS-TAB',
             c_prefix_tab_fcode(2) TYPE c VALUE 'T\'.

  FIELD-SYMBOLS:  <lv_actual_tab> TYPE text60.

  IF gs_tabstrip_captions IS INITIAL.

    REFRESH gt_tabstrip_fcodes.
****Prepare default tabs
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = c_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain_values
      EXCEPTIONS ##FM_SUBRC_OK
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
    ENDIF.

    LOOP AT lt_domain_values INTO ls_domain_value.
      CLEAR ls_tabstrip_fcode.
      lv_agtab = ls_domain_value-valpos+2(2).

      CONCATENATE c_prefix_caption lv_agtab INTO lv_actual_tabname.
      ASSIGN (lv_actual_tabname) TO <lv_actual_tab>.
      ls_tabstrip_fcode-tscode = ls_domain_value-domvalue_l.

      CASE ls_domain_value-domvalue_l.
        WHEN 'PO'.
          ls_domain_value-ddtext = 'Orders'.
****Postings Tab
          WRITE icon_te_receipts AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.
****Store other info for screen processing
          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_postings.
          APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
        WHEN 'AC'.
****Accomplishment
****Prepare title
          WRITE icon_equipment AS ICON TO <lv_actual_tab>.
          CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                 INTO <lv_actual_tab>.

          PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                 CHANGING <lv_actual_tab>.
****Store other info for screen processing
          REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
          ls_tabstrip_fcode-tabname = lv_actual_tabname.
          CONCATENATE c_prefix_tab_fcode lv_agtab INTO ls_tabstrip_fcode-ts_fcode.
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_items.
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
          ls_tabstrip_fcode-local_fcode = c_fcode-tab_additional_data1.
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
ENDFORM.                    " TABSTRIP_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_SEQUENCE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabstrip_sequence_set .

  DATA: lv_agtab(2)           TYPE n,
        lv_tabix(2)           TYPE n,
        lv_actual_tabname(32) TYPE c,
        lv_tabname(5)         TYPE c,
        lv_tab_title          TYPE val_text,
        ls_tabs_sequence      TYPE /agri/v_tfmirts,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip,
        ls_tabstrip_captions  TYPE /agri/s_gtabstrip_captions,
        ls_tabstrip_text      TYPE dd07v,
        lt_tabstrip_fcodes    TYPE /agri/t_gtabstrip.

  CONSTANTS: c_prefix_tab(3) TYPE c VALUE 'TAB',
             c_prefix_ts(28) TYPE c VALUE 'LS_TABSTRIP_CAPTIONS-TAB'.

  FIELD-SYMBOLS: <lv_actual_tab> TYPE text60,
                 <lv_tab_text>   TYPE any.

  SORT gt_tfmirts BY seqnr.

  LOOP AT gt_tfmirts INTO ls_tabs_sequence.
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
        IF sy-subrc EQ 0.
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
*&      Form  TEXTS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM texts_update .
  DATA: lv_texts_changed.
  CLEAR sy-subrc.

  IF NOT ref_text IS INITIAL.
    CALL METHOD ref_text->text_update
      IMPORTING
        e_data_changed = lv_texts_changed.
  ENDIF.

  IF sy-subrc EQ 0 AND lv_texts_changed IS NOT INITIAL.

    gs_variables-document_changed = c_true.

    IF gs_acdoc_infocus-x-achdr-updkz IS INITIAL.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.
ENDFORM.                    " TEXTS_UPDATE
*&---------------------------------------------------------------------*
*&      Form  TEXT_MAINTAIN
*&---------------------------------------------------------------------*
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

  CONCATENATE gs_acdoc_infocus-x-achdr-accom '' INTO lv_objval.

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
*&      Form  TIME_CONVERT
*&---------------------------------------------------------------------*
FORM time_convert USING    lv_qmein TYPE meins
                           lv_meins TYPE meins
                           lv_duran TYPE /agri/glduran
                  CHANGING lv_auszt TYPE auszt.

  DATA: lwa_t006 TYPE t006.

  SELECT SINGLE * FROM t006
      INTO lwa_t006
      WHERE msehi EQ lv_qmein
        AND dimid EQ 'TIME'.

  CHECK sy-subrc EQ 0.

  lv_auszt = lv_duran.
*  CALL FUNCTION 'PM_TIME_CONVERSION'
*    EXPORTING
*      time_in           = lv_auszt
**     UNIT_IN           = 'S'
*      unit_in_int       = lv_meins
**     UNIT_OUT          = 'H'
*      unit_out_int      = lv_qmein
*    IMPORTING
**     EXTERNAL_UNIT     =
**     INTERNAL_UNIT     =
*      time_out          = lv_auszt
*    EXCEPTIONS
*      invalid_time_unit = 1
*      OTHERS            = 2.
*  IF sy-subrc EQ 0.
*  ENDIF.

  CALL FUNCTION '/AGRI/G_PM_TIME_CONVERSION'
    EXPORTING
      i_time_in         = lv_auszt
*     i_unit_in         = 's'
      i_unit_in_int     = lv_meins
*     i_unit_out        = 'h'
      i_unit_out_int    = lv_qmein
    IMPORTING
*     e_external_unit   =
*     e_internal_unit   =
      e_time_out        = lv_auszt
    EXCEPTIONS ##FM_SUBRC_OK
      invalid_time_unit = 1
      OTHERS            = 2.
  IF sy-subrc EQ 0.
  ENDIF.

ENDFORM.                    " TIME_CONVERT

**&---------------------------------------------------------------------*
**& Form TERRAIN_DETAILS
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM terrain_details .
*
*  DATA : lt_terrain  TYPE STANDARD TABLE OF /agri/glflcma,
*         lwa_terrain TYPE /agri/glflcma,
*         lt_order    TYPE STANDARD TABLE OF /agri/fmfphdr,
*         lwa_order   TYPE /agri/fmfphdr,
*         lwa_details TYPE ty_details,
*         lwa_acm_ord TYPE zabs_acm_ord,
*         lt_acm_ord  TYPE STANDARD TABLE OF zabs_acm_ord.
*
**get the terrain & counter details.
*
*  CLEAR : gt_details_trr[], lt_terrain[], lt_order[].
*
** while creation only.
*
*  IF gs_acdoc_infocus-updkz EQ 'I'.
*
*    IF p_strdat IS INITIAL.
*      p_strdat = /agri/s_fmachdr-strtdat.
*    ENDIF.
*
*    IF p_findat IS  INITIAL.
*      p_findat = /agri/s_fmachdr-findat.
*    ENDIF.
*
*    SELECT tplnr_fl contr
*      FROM /agri/glflcma
*      INTO CORRESPONDING FIELDS OF TABLE lt_terrain
*     WHERE tplnr_fl IN so_tplnr
*       AND datab <= p_strdat
*       AND datbi >= p_findat
*       AND astat = 'A'
*       AND loevm = space.
*    IF sy-subrc = 0.
*      SELECT aufnr auart tplnr_fl contr matnr gamng gmein
*        FROM /agri/fmfphdr INTO CORRESPONDING FIELDS OF TABLE lt_order
*         FOR ALL ENTRIES IN lt_terrain
*       WHERE aufnr IN so_aufnr
*         AND tplnr_fl = lt_terrain-tplnr_fl
*         AND contr    = lt_terrain-contr
*         AND matnr    = p_mat.
*    ENDIF.
*
*    LOOP AT lt_order INTO lwa_order.
*      MOVE-CORRESPONDING lwa_order TO lwa_details.
*      lwa_details-meins = lwa_order-gmein.
*      lwa_details-menge = lwa_order-gamng.
*
*      CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_OUTPUT'
*        EXPORTING
*          i_input  = lwa_order-tplnr_fl
*        IMPORTING
*          o_output = lwa_details-tplnr_fl.
*
*      APPEND lwa_details TO gt_details_trr.
*      CLEAR : lwa_details.
*    ENDLOOP.
*
*  ELSE.
*
*    CALL FUNCTION 'ZABS_FM_ORD_GET'
*      EXPORTING
*        iv_accom  = gs_acdoc_infocus-accom
*      TABLES
*        t_acm_ord = lt_acm_ord.
*
*    LOOP AT lt_acm_ord INTO lwa_acm_ord.
*      CLEAR lwa_details.
*      MOVE-CORRESPONDING lwa_acm_ord TO lwa_details.
*      APPEND lwa_details TO gt_details_trr.
*    ENDLOOP.
*
*  ENDIF.
*
*  gs_variables-refresh_postings_grid = c_true.
*
*ENDFORM.
