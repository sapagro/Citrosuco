*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0E .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_processing .

  DATA:  lv_answer.

  fcode = ok_code.
  CLEAR ok_code.

  CASE sy-dynnr.
    WHEN c_screen-overview.

****Check whether there are any changes
      PERFORM changes_confirm CHANGING lv_answer.

      IF lv_answer EQ 'A'.
        IF ok_code EQ c_fcode-save.
          IF fcode EQ c_fcode-exit.
            gs_variables-exit_after_save = c_true.
          ELSE.
            gs_variables-exit_after_save = 'C'.
          ENDIF.
          ok_code = c_fcode-back.
        ENDIF.
      ENDIF.

      CHECK lv_answer NE 'A'.
      IF fcode EQ c_fcode-exit.
        SET SCREEN 0.
        LEAVE SCREEN.
      ELSE.
        PERFORM document_data_initialize USING c_true.
        SET SCREEN 100.
        LEAVE SCREEN.
      ENDIF.
    WHEN c_screen-create_floc.
      CLEAR: gs_fldoc_infocus, /agri/s_glflot.
      CALL FUNCTION 'DEQUEUE_ALL'
*       EXPORTING
*         _SYNCHRON       = ' '
        .
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN c_screen-reference_summary.

      REFRESH: gt_glflot_buffer.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN c_screen-new_label.
      CLEAR: gs_variables-new_label.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN OTHERS.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    " EXIT_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  edit_functions_exclude
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_EXCLUDES  text
*----------------------------------------------------------------------*
FORM edit_functions_exclude CHANGING lt_excludes TYPE ui_functions.

  APPEND cl_gui_alv_grid=>mc_fc_auf TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_average TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_back_classic TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_chain TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_crbatch TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_crweb TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_lineitems TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_master_data TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_report TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_xint TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_call_xxl TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_check TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_data_save TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_delete_filter TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_deselect_all TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdata TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdesig TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_expcrtempl TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_expmdb TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_extend TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_f4 TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_fix_columns TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_help TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_html TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_info TO lt_excludes.
*  APPEND cl_gui_alv_grid=>mc_mb_export TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_mb_view TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_mb_sum TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_mb_subtot TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_mb_filter TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy      TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut       TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row  TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste     TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo      TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_print   TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_refresh  TO lt_excludes.
  APPEND cl_gui_alv_grid=>mc_fc_detail   TO lt_excludes.

ENDFORM.                    "edit_functions_exclude

*&---------------------------------------------------------------------*
*& Form EXTHIER_TREE_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_REFERENCE_DOCS[]
*&      --> C_TRUE
*&---------------------------------------------------------------------*
FORM exthier_tree_prepare USING lt_reference_docs TYPE /agri/t_glflot.

  DATA: lwa_glflot TYPE /agri/s_glflot,
        lt_glflot  TYPE /agri/t_glflot.

  IF NOT lt_reference_docs[] IS INITIAL.

****To find childs of particular doc.
    SELECT * FROM /agri/glflot
      INTO CORRESPONDING FIELDS OF TABLE lt_glflot
       FOR ALL ENTRIES IN lt_reference_docs
     WHERE tplma = lt_reference_docs-tplnr_fl.

  ELSE.

    SELECT * FROM /agri/glflot
      INTO CORRESPONDING FIELDS OF TABLE lt_glflot
     WHERE tplma = gs_fldoc_infocus-tplnr_fl.

  ENDIF.

  LOOP AT lt_glflot INTO lwa_glflot.
    READ TABLE gt_glflot_buffer TRANSPORTING NO FIELDS
          WITH KEY tplma    = lwa_glflot-tplma
                   tplnr_fl = lwa_glflot-tplnr_fl.

    IF sy-subrc EQ 0.
      DELETE lt_glflot.
    ENDIF.
  ENDLOOP.

  IF NOT lt_glflot[] IS INITIAL.
    APPEND LINES OF lt_glflot TO gt_glflot_buffer.
    PERFORM exthier_tree_prepare USING lt_glflot.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXTDFL_TREE_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_EXTDFL_TREE
*&      --> GT_GLFLOT_BUFFER
*&---------------------------------------------------------------------*
FORM extdfl_tree_prepare CHANGING lt_glflot_buffer TYPE /agri/t_glflot
                       lt_extdfl_tree   TYPE /agri/t_glfldfl_fcat.


  DATA: lwa_extdfl_tree TYPE /agri/s_glfldfl_fcat,
        lv_level        TYPE i.

  FIELD-SYMBOLS: <lwa_extdfl_tree_tmp> TYPE /agri/s_glfldfl_fcat.

  REFRESH : lt_extdfl_tree[].

  IF NOT lt_glflot_buffer[] IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM lt_glflot_buffer    "#EC CI_SORTED
                          COMPARING tplma tplnr_fl.

    PERFORM child_nodes_get USING gs_fldoc_infocus-tplnr_fl
                                  lt_glflot_buffer
                         CHANGING lt_extdfl_tree
                                  lv_level.

    READ TABLE lt_extdfl_tree ASSIGNING <lwa_extdfl_tree_tmp>
                              INDEX 1.
    IF sy-subrc = 0.
      <lwa_extdfl_tree_tmp>-tplma_txt = gs_fldoc_infocus-x-flhdr-strno.
    ENDIF.

  ENDIF.

ENDFORM.
