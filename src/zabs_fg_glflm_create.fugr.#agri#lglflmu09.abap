FUNCTION /agri/glfl_hierarchy_get .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_TPLNR) TYPE  /AGRI/GLTPLNR_FL
*"     REFERENCE(IV_TOP) TYPE  XFELD DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_FLHIER_LIST) TYPE  /AGRI/T_GLFLHIER_LIST
*"  EXCEPTIONS
*"      INVALID_DATA
*"      INCOMPLETE_DATA
*"----------------------------------------------------------------------
  DATA: lv_tplma          TYPE /agri/gltplma,
        lv_lines          TYPE i,
        lv_tplnr          TYPE /agri/gltplnr_fl,
        lv_no_data_exists,
        ls_hlist          TYPE /agri/s_glflhier_list,
        ls_extdfl_tree    TYPE /agri/s_glfldfl_fcat.

  DEFINE add_terrain.
    CHECK &1 IS NOT INITIAL.
    ls_hlist-tplnr_fl = &1.
    keytext_get_simple '/AGRI/GLFLOTX' 'TPLNR_FL' ls_hlist-tplnr_fl
                       ls_hlist-pltxt.
    INSERT ls_hlist INTO et_flhier_list INDEX 1.
  END-OF-DEFINITION.

  IF i_tplnr IS INITIAL.
    RAISE incomplete_data.
  ENDIF.

  IF iv_top IS NOT INITIAL.
    IF i_tplnr EQ gs_fldoc_infocus-x-flhdr-tplnr_fl.
      lv_tplma = gs_fldoc_infocus-x-flhdr-tplma.
    ELSE.
      SELECT SINGLE tplma FROM /agri/glflot
        INTO lv_tplma
       WHERE tplnr_fl EQ i_tplnr.
      IF sy-subrc NE 0.
        RAISE invalid_data.
      ENDIF.
    ENDIF.

    REFRESH et_flhier_list.
    add_terrain i_tplnr.

    DO.
      IF lv_tplma IS INITIAL.
        EXIT.
      ENDIF.

      add_terrain lv_tplma.
      lv_tplnr = lv_tplma.
      CLEAR lv_tplma.

      SELECT SINGLE tplma INTO lv_tplma     "#EC CI_SEL_NESTED
        FROM /agri/glflot
       WHERE tplnr_fl EQ lv_tplnr.
      IF sy-subrc NE 0 OR
         lv_tplma IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    DESCRIBE TABLE et_flhier_list LINES lv_lines.
    IF lv_lines EQ 1.
      REFRESH: et_flhier_list.
    ENDIF.
  ELSE.
    gs_fldoc_infocus-tplnr_fl = i_tplnr.
    PERFORM display_fl_hierarchy CHANGING lv_no_data_exists.
    IF lv_no_data_exists EQ c_true.
      RAISE invalid_data.
    ENDIF.

    PERFORM extdfl_tree_prepare CHANGING gt_glflot_buffer
                              gt_extdfl_tree.

    LOOP AT gt_extdfl_tree INTO ls_extdfl_tree.
      ls_hlist-tplnr_fl = ls_extdfl_tree-tplnr_fl.
      ls_hlist-tplma    = ls_extdfl_tree-tplma.
      keytext_get_simple '/AGRI/GLFLOTX' 'TPLNR_FL' ls_hlist-tplnr_fl
                          ls_hlist-pltxt.
      ls_hlist-hlevel   = ls_extdfl_tree-level.
      APPEND ls_hlist TO et_flhier_list.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
